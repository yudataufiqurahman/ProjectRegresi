# ============================================================
# DASHBOARD REGRESI CCPP - IMPROVED VERSION
# Fokus utama:
#   1. UI lebih rapi & lebih lengkap di Page 1-4
#   2. Loading lebih cepat dengan cache GAM + subset efisien
#   3. Algoritma model tetap mengikuti fungsi asli
# ============================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(callr)

source("functions_regresi_ccpp.R")
source("functions_dashboard.R")
source("functions_optimization.R")

ccpp <- load_ccpp_data()

theme_plot <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

fmt_num <- function(x, d = 3) formatC(x, format = "f", digits = d, big.mark = ",")

info_box <- function(text) {
  div(class = "alert alert-info", style = "font-size:0.92rem;", HTML(text))
}

warn_box <- function(text) {
  div(class = "alert alert-warning", style = "font-size:0.92rem;", HTML(text))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================
# OPTIMIZED ANALYSIS BUILD
# ============================================================
run_full_analysis_fast <- function(data_sheets,
                                  target = "PE",
                                  features = c("AT", "V", "AP", "RH"),
                                  K = 10,
                                  seed = 42) {
  validate_ccpp_data(data_sheets, target, features)

  sheet_names <- names(data_sheets)
  n <- nrow(data_sheets[[1]])

  if (!all(vapply(data_sheets, nrow, integer(1)) == n)) {
    stop("Semua sheet harus memiliki jumlah baris yang sama untuk eksperimen ini.", call. = FALSE)
  }

  split_idx <- make_train_test_split(n = n, test_size = 0.20, seed = seed)
  folds <- make_folds(n = length(split_idx$train), K = K, seed = seed)

  splits <- lapply(data_sheets, function(d) {
    list(
      X_train = d[split_idx$train, features, drop = FALSE],
      X_test = d[split_idx$test, features, drop = FALSE],
      y_train = d[split_idx$train, target],
      y_test = d[split_idx$test, target]
    )
  })
  names(splits) <- sheet_names

  poly_all <- list(); en_all <- list(); gam_all <- list()

  for (sh in sheet_names) {
    sp <- splits[[sh]]
    poly_all[[sh]] <- run_polynomial(sp$X_train, sp$y_train, sp$X_test, sp$y_test, folds = folds)
    en_all[[sh]] <- run_elasticnet(sp$X_train, sp$y_train, sp$X_test, sp$y_test, folds = folds, seed = seed)

    # Optimasi GAM: sheet pertama full, sheet berikutnya pakai cache sp
    use_cached <- sh != sheet_names[1]
    gam_all[[sh]] <- run_gam_optimized(
      sp$X_train, sp$y_train, sp$X_test, sp$y_test, folds,
      k_spline = max(4, min(20, floor(length(sp$y_train) / (3 * length(features))))),
      use_cached_sp = use_cached,
      sheet_id = sh,
      timeout_sec = 10
    )
  }

  best_model_rows <- lapply(sheet_names, function(sh) {
    aicc_vals <- c(
      Polynomial = poly_all[[sh]]$final$AIC$AICc,
      `Elastic Net` = en_all[[sh]]$final$AIC$AICc,
      GAM = gam_all[[sh]]$final$AIC$AICc
    )
    cv_vals <- c(
      Polynomial = poly_all[[sh]]$final$CV$CV_RMSE,
      `Elastic Net` = en_all[[sh]]$final$CV$CV_RMSE,
      GAM = gam_all[[sh]]$final$CV$CV_RMSE
    )
    model_best_aic <- names(which.min(aicc_vals))
    model_best_cv <- names(which.min(cv_vals))
    data.frame(
      Sheet = sh,
      AICc_Polynomial = unname(aicc_vals["Polynomial"]),
      AICc_ElasticNet = unname(aicc_vals["Elastic Net"]),
      AICc_GAM = unname(aicc_vals["GAM"]),
      CV_RMSE_Polynomial = unname(cv_vals["Polynomial"]),
      CV_RMSE_ElasticNet = unname(cv_vals["Elastic Net"]),
      CV_RMSE_GAM = unname(cv_vals["GAM"]),
      Model_Terbaik_AIC = model_best_aic,
      Model_Terbaik_CV = model_best_cv,
      Sama = model_best_aic == model_best_cv,
      row.names = NULL
    )
  })

  best_model_df <- dplyr::bind_rows(best_model_rows)
  agreement_rate <- mean(best_model_df$Sama)

  rank_corr_rows <- lapply(sheet_names, function(sh) {
    row <- best_model_df[best_model_df$Sheet == sh, , drop = FALSE]
    aicc <- as.numeric(row[1, c("AICc_Polynomial", "AICc_ElasticNet", "AICc_GAM")])
    cv <- as.numeric(row[1, c("CV_RMSE_Polynomial", "CV_RMSE_ElasticNet", "CV_RMSE_GAM")])
    test <- suppressWarnings(stats::cor.test(aicc, cv, method = "spearman", exact = FALSE))
    data.frame(
      Sheet = sh,
      Spearman_rho = unname(test$estimate),
      p_value = test$p.value,
      row.names = NULL
    )
  })
  rank_corr_df <- dplyr::bind_rows(rank_corr_rows)

  cv_fold_store <- list(
    Polynomial = lapply(sheet_names, function(sh) {
      deg <- get_config_degree(poly_all[[sh]]$final$CV$config)
      poly_all[[sh]]$cv_fold_rmse[[as.character(deg)]]
    }),
    `Elastic Net` = lapply(sheet_names, function(sh) en_all[[sh]]$cv_fold_rmse),
    GAM = lapply(sheet_names, function(sh) gam_all[[sh]]$cv_fold_rmse)
  )
  lapply(cv_fold_store, stats::setNames, sheet_names) -> cv_fold_store

  anova_rows <- lapply(names(cv_fold_store), function(model_name) {
    per_sheet <- cv_fold_store[[model_name]]
    plot_df <- dplyr::bind_rows(lapply(names(per_sheet), function(sh) {
      data.frame(Sheet = sh, CV_Fold_RMSE = per_sheet[[sh]])
    }))
    fit_aov <- stats::aov(CV_Fold_RMSE ~ Sheet, data = plot_df)
    aov_tab <- summary(fit_aov)[[1]]
    f_stat <- aov_tab[1, "F value"]
    p_anova <- aov_tab[1, "Pr(>F)"]
    kw <- stats::kruskal.test(CV_Fold_RMSE ~ Sheet, data = plot_df)
    data.frame(
      Model = model_name,
      ANOVA_F = f_stat,
      ANOVA_p = p_anova,
      KruskalWallis_H = unname(kw$statistic),
      KruskalWallis_p = kw$p.value,
      Signifikan_p_lt_0_05 = p_anova < 0.05,
      row.names = NULL
    )
  })
  anova_df <- dplyr::bind_rows(anova_rows)

  structure(list(
    data_sheets = data_sheets,
    sheet_names = sheet_names,
    target = target,
    features = features,
    splits = splits,
    folds = folds,
    poly_all = poly_all,
    en_all = en_all,
    gam_all = gam_all,
    best_model_df = best_model_df,
    agreement_rate = agreement_rate,
    rank_corr_df = rank_corr_df,
    cv_fold_store = cv_fold_store,
    anova_df = anova_df
  ), class = c("ccpp_analysis_fast", "list"))
}

build_ccpp_analysis_fast <- function(data_sheets, on_step = function(msg, frac) NULL,
                                    K = 10, seed = 42) {
  capped <- lapply(data_sheets, cap_rows, max_rows = MAX_ROWS_PER_SHEET, seed = seed)
  data_used <- stats::setNames(lapply(capped, `[[`, "data"), names(data_sheets))
  cap_info <- dplyr::bind_rows(lapply(names(capped), function(sh) {
    ci <- capped[[sh]]
    data.frame(Sheet = sh, Baris_Digunakan = ci$n_used, Baris_Total = ci$n_total,
               Disubsample = ci$capped, stringsAsFactors = FALSE)
  }))

  res <- run_full_analysis_fast(data_used, K = K, seed = seed)
  attr(res, "cap_info") <- cap_info
  res
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tagList(icon("bolt"), "Dashboard Regresi CCPP"),
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#6366f1") %>%
    bs_add_rules(" 
      body, .navbar-brand, .nav-link, .btn, .card { font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', sans-serif; }
      h1, h2, h3, h4, h5, .card-header { font-family: 'Lexend', 'Inter', system-ui, sans-serif; }
      .card { box-shadow: 0 1px 3px rgba(0,0,0,.08); border: 1px solid rgba(0,0,0,.06); }
      .eq-box { background:#0f172a; color:#e2e8f0; padding:16px 18px; border-radius:10px; font-family: 'Fira Code', 'Consolas', monospace; white-space:pre-wrap; font-size:0.92rem; line-height:1.55; }
      .navbar-brand { font-weight:700; }
    "),
  header = tags$head(tags$style(HTML(".nav-link{font-weight:500;}") )),

  nav_panel(
    title = "1. Eksplorasi Data",
    icon = icon("magnifying-glass-chart"),
    page_sidebar(
      sidebar = sidebar(
        width = 300, title = "Pengaturan Eksplorasi",
        selectInput("eda_sheet", "Pilih Sheet", choices = c("Semua sheet (gabungan)", ccpp$data %>% names())),
        selectInput("eda_var", "Variabel untuk histogram/density", choices = c(CCPP_FEATURES, CCPP_TARGET), selected = CCPP_TARGET),
        hr(),
        p(tags$b("Sumber data:"), style = "margin-bottom:2px;"),
        p(ccpp$source, style = "font-size:0.85rem; color:#64748b;"),
        if (ccpp$simulated)
          div(class = "alert alert-warning", style = "font-size:0.8rem; padding:8px;",
              "File CCPP asli tidak ditemukan. Dashboard memakai data simulasi bergaya CCPP agar seluruh fitur tetap bisa didemokan.")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Total Baris", value = textOutput("vb_rows"), showcase = icon("table-cells"), theme = "primary"),
        value_box(title = "Jumlah Sheet", value = textOutput("vb_sheets"), showcase = icon("layer-group"), theme = "info"),
        value_box(title = "Rata-rata PE (MW)", value = textOutput("vb_mean_pe"), showcase = icon("gauge"), theme = "success"),
        value_box(title = "Korelasi |PE~AT|", value = textOutput("vb_cor_at"), showcase = icon("link"), theme = "warning")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Heatmap Korelasi Antar-Variabel"), plotlyOutput("eda_corr", height = 320)),
        card(card_header("Distribusi Variabel Terpilih"), plotlyOutput("eda_hist", height = 320))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("PE vs Setiap Prediktor"), plotlyOutput("eda_scatter", height = 360)),
        card(card_header("Konsistensi Distribusi PE Antar-Sheet"), plotlyOutput("eda_boxplot", height = 360))
      ),
      card(card_header("Pemeriksaan Kualitas Data (missing value & outlier per-IQR)"), DTOutput("eda_quality_tbl"))
    )
  ),

  nav_panel(
    title = "2. Analisis Model",
    icon = icon("chart-line"),
    page_sidebar(
      sidebar = sidebar(
        width = 300, title = "Pengaturan Analisis",
        selectInput("an_sheet", "Pilih Sheet", choices = NULL),
        selectInput("an_model", "Pilih Model", choices = MODEL_NAMES),
        radioButtons("an_route", "Rute Pemilihan Model", choices = c("AICc" = "AICc", "Cross-Validation" = "CV"), selected = "AICc"),
        hr(),
        uiOutput("an_status_note")
      ),
      uiOutput("an_body")
    )
  ),

  nav_panel(
    title = "3. Evaluasi Model",
    icon = icon("scale-balanced"),
    uiOutput("eval_body")
  ),

  nav_panel(
    title = "4. Simulasi Interaktif",
    icon = icon("sliders"),
    page_sidebar(
      sidebar = sidebar(
        width = 320, title = "Dataset Dummy",
        selectInput("sim_func", "Pola data (kebenaran/truth)", choices = SIM_FUNCTIONS),
        sliderInput("sim_n", "Jumlah titik data", 40, 400, 150, step = 10),
        sliderInput("sim_noise", "Tingkat noise", 0, 5, 1.5, step = 0.1),
        numericInput("sim_seed", "Seed acak", 1, min = 1, max = 9999),
        actionButton("sim_generate", "Buat / Acak Ulang Data", icon = icon("dice"), class = "btn-primary w-100"),
        hr(),
        p(tags$b("Polynomial"), style="margin-bottom:4px;"),
        sliderInput("sim_poly_degree", "Derajat polinomial", 1, 12, 4),
        hr(),
        p(tags$b("Elastic Net"), style="margin-bottom:4px;"),
        sliderInput("sim_en_degree", "Derajat basis", 2, 12, 8),
        sliderInput("sim_en_alpha", "Rasio L1 (α: 0=Ridge, 1=Lasso)", 0, 1, 0.5, step = 0.05),
        sliderInput("sim_en_loglam", "log10(λ) relatif", -4, 1, -2, step = 0.1),
        hr(),
        p(tags$b("GAM"), style="margin-bottom:4px;"),
        sliderInput("sim_gam_k", "Basis spline (k)", 4, 30, 10),
        checkboxInput("sim_gam_auto", "λ otomatis (GCV)", TRUE),
        conditionalPanel("!input.sim_gam_auto",
          sliderInput("sim_gam_loglam", "log10(λ) manual", -3, 3, 0, step = 0.1))
      ),
      layout_columns(
        col_widths = c(12),
        card(card_header("Simulasi Polynomial"), plotlyOutput("sim_plot_poly", height = 360)),
        card(card_header("Simulasi Elastic Net"), plotlyOutput("sim_plot_en", height = 360)),
        card(card_header("Simulasi GAM"), plotlyOutput("sim_plot_gam", height = 360))
      )
    )
  ),

  nav_panel(
    title = "5. Upload Data Anda",
    icon = icon("upload"),
    page_sidebar(
      sidebar = sidebar(
        width = 340, title = "1. Sediakan Data",
        fileInput("up_file", "Unggah file (CSV / XLSX / XLS / ODS)", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".ods")),
        uiOutput("up_sheet_selector"),
        actionButton("up_use_example", "Atau pakai dataset contoh", icon = icon("flask"), class = "btn-outline-secondary w-100"),
        hr(),
        h6("2. Pilih Variabel"),
        uiOutput("up_target_selector"),
        uiOutput("up_features_selector"),
        hr(),
        h6("3. Pengaturan Analisis"),
        sliderInput("up_k", "Jumlah fold Cross-Validation (K)", 3, 15, 10),
        sliderInput("up_test_size", "Proporsi data uji", 0.1, 0.4, 0.2, step = 0.05),
        actionButton("up_run", "Jalankan Analisis Otomatis", icon = icon("play"), class = "btn-primary w-100"),
        uiOutput("up_validation_msg")
      ),
      uiOutput("up_body")
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text", style = "font-size:0.8rem; opacity:.7;", "Polynomial · Elastic Net · GAM"))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  analysis_state <- reactiveValues(result = NULL, status = "starting", progress = 0, note = "")

  observeEvent(TRUE, {
    cached <- load_cached_analysis(ccpp$signature)
    if (!is.null(cached)) {
      analysis_state$result <- cached
      analysis_state$status <- "ready"
      return(invisible())
    }
    analysis_state$status <- "computing"
    withProgress(message = "Menjalankan analisis awal (Polynomial, Elastic Net, GAM)...", value = 0, {
      res <- build_ccpp_analysis_fast(ccpp$data, on_step = function(msg, frac) {
        analysis_state$note <- msg
        setProgress(value = frac, detail = msg)
      }, K = 10, seed = 42)
      save_cached_analysis(res, ccpp$signature)
      analysis_state$result <- res
      analysis_state$status <- "ready"
    })
  }, once = TRUE)

  req_analysis <- function() {
    validate(need(analysis_state$status == "ready", "Sedang menyiapkan analisis, mohon tunggu sebentar..."))
    analysis_state$result
  }

  observe({
    res <- analysis_state$result
    if (!is.null(res)) updateSelectInput(session, "an_sheet", choices = res$sheet_names)
  })

  # Page 1
  eda_df <- reactive({
    if (identical(input$eda_sheet, "Semua sheet (gabungan)")) {
      dplyr::bind_rows(ccpp$data, .id = "Sheet")
    } else {
      d <- ccpp$data[[input$eda_sheet]]
      d$Sheet <- input$eda_sheet
      d
    }
  })

  output$vb_rows <- renderText(format(nrow(eda_df()), big.mark = ","))
  output$vb_sheets <- renderText(length(ccpp$data))
  output$vb_mean_pe <- renderText(fmt_num(mean(eda_df()$PE), 1))
  output$vb_cor_at <- renderText(fmt_num(abs(cor(eda_df()$AT, eda_df()$PE)), 3))

  output$eda_corr <- renderPlotly({
    d <- eda_df()[, c(CCPP_FEATURES, CCPP_TARGET)]
    cm <- round(cor(d), 2)
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap",
            colors = colorRamp(c("#ef4444", "white", "#6366f1")), zmin = -1, zmax = 1,
            text = cm, texttemplate = "%{text}") %>%
      layout(margin = list(t = 10), xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$eda_hist <- renderPlotly({
    d <- eda_df()
    p <- ggplot(d, aes(x = .data[[input$eda_var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#6366f1", alpha = .75) +
      geom_density(color = "#0f172a", linewidth = .8) +
      labs(x = input$eda_var, y = "Densitas", title = paste("Distribusi", input$eda_var)) +
      theme_plot()
    ggplotly(p) %>% layout(showlegend = FALSE)
  })

  output$eda_scatter <- renderPlotly({
    d <- eda_df()
    dl <- tidyr::pivot_longer(d, all_of(CCPP_FEATURES), names_to = "Prediktor", values_to = "Nilai")
    p <- ggplot(dl, aes(x = Nilai, y = PE)) +
      geom_point(alpha = .18, color = "#6366f1", size = .8) +
      geom_smooth(method = "loess", color = "#0f172a", se = FALSE, linewidth = .8, formula = y ~ x) +
      facet_wrap(~Prediktor, scales = "free_x") +
      labs(x = NULL, y = "PE (MW)") + theme_plot()
    ggplotly(p, height = 340)
  })

  output$eda_boxplot <- renderPlotly({
    dl <- dplyr::bind_rows(ccpp$data, .id = "Sheet")
    p <- ggplot(dl, aes(x = Sheet, y = PE, fill = Sheet)) +
      geom_boxplot(alpha = .8, show.legend = FALSE) +
      labs(x = NULL, y = "PE (MW)", title = "PE per Sheet (idealnya distribusi mirip)") +
      theme_plot()
    ggplotly(p)
  })

  output$eda_quality_tbl <- renderDT({
    rows <- lapply(names(ccpp$data), function(sh) {
      d <- ccpp$data[[sh]]
      out <- lapply(names(d), function(v) {
        x <- d[[v]]
        q <- quantile(x, c(.25, .75), na.rm = TRUE); iqr <- q[2] - q[1]
        lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
        data.frame(Sheet = sh, Variabel = v, Missing = sum(is.na(x)),
                   Outlier_IQR = sum(x < lo | x > hi, na.rm = TRUE),
                   Persen_Outlier = round(100 * mean(x < lo | x > hi, na.rm = TRUE), 2))
      })
      dplyr::bind_rows(out)
    })
    datatable(dplyr::bind_rows(rows), rownames = FALSE, options = list(pageLength = 10))
  })

  # Page 2
  an_obj <- reactive({
    res <- req_analysis(); req(input$an_sheet)
    switch(input$an_model,
           "Polynomial" = res$poly_all[[input$an_sheet]],
           "Elastic Net" = res$en_all[[input$an_sheet]],
           "GAM" = res$gam_all[[input$an_sheet]])
  })

  an_predictor <- reactive({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]
    build_predictor(input$an_model, input$an_route, an_obj(), sp$X_train, sp$y_train,
                    k_spline = res$k_spline %||% 20)
  })

  output$an_status_note <- renderUI({
    res <- req_analysis(); gf <- attr(res, "gam_fallback"); mf <- attr(res, "model_failed")
    tags <- list()
    if (length(gf)) tags <- c(tags, list(warn_box(sprintf("⚠️ GAM pada sheet <b>%s</b> memakai basis lebih kecil karena konvergensi standar lambat.", paste(gf, collapse = ", ")))))
    if (length(mf)) tags <- c(tags, list(warn_box(sprintf("⚠️ GAM pada sheet <b>%s</b> digantikan regresi linear sementara.", paste(mf, collapse = ", ")))))
    tagList(tags)
  })

  output$an_body <- renderUI({
    res <- req_analysis(); sh <- input$an_sheet; route <- input$an_route
    best_tbl <- res$best_model_df
    model_obj <- an_obj(); pr <- an_predictor(); desc <- describe_predictor(pr, CCPP_TARGET)
    fin <- model_obj$final[[route_key(route)]]

    tagList(
      card(card_header("Ringkasan AIC & CV untuk Semua Sheet"), 
           DTOutput("an_top_summary_tbl")),
      card(card_header(sprintf("Model %s — %s", input$an_model, input$an_route)),
           layout_columns(
             col_widths = c(3, 3, 3, 3),
             value_box(title = "AICc", value = if (is.na(fin$AICc)) "-" else fmt_num(fin$AICc, 1), showcase = icon("chart-simple"), theme = "primary"),
             value_box(title = "CV RMSE", value = if (is.na(fin$CV_RMSE)) "-" else fmt_num(fin$CV_RMSE, 3), showcase = icon("layer-group"), theme = "info"),
             value_box(title = "Test RMSE", value = fmt_num(fin$test_RMSE, 3), showcase = icon("bullseye"), theme = "success"),
             value_box(title = "Test R²", value = fmt_num(fin$test_R2, 4), showcase = icon("percent"), theme = "warning")
           ),
           div(class = "eq-box", desc$eq_std),
           if (!is.null(desc$eq_orig)) tags$p(tags$b("Dalam satuan asli:"), style="margin-top:10px;margin-bottom:4px;", tags$div(class = "eq-box", desc$eq_orig)),
           tags$p(desc$note, class = "text-muted", style = "font-size:0.85rem; margin-top:10px;"),
           hr(),
           DTOutput("an_coef_tbl"),
           layout_columns(
             col_widths = c(6, 6),
             card(card_header("Aktual vs Prediksi (Data Uji)"), plotlyOutput(sprintf("an_actual_pred_%s", tolower(gsub(" ", "_", input$an_model))), height = 300)),
             card(card_header("Kurva Tuning"), plotlyOutput(sprintf("an_tuning_%s", tolower(gsub(" ", "_", input$an_model))), height = 300))
           )
      )
    )
  })

  output$an_top_summary_tbl <- renderDT({
    res <- req_analysis()
    tbl <- res$best_model_df %>%
      mutate(
        Best_AIC = pmax(AICc_Polynomial, AICc_ElasticNet, AICc_GAM, na.rm = TRUE),
        Best_CV = pmax(CV_RMSE_Polynomial, CV_RMSE_ElasticNet, CV_RMSE_GAM, na.rm = TRUE)
      )
    datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("AICc_Polynomial", "AICc_ElasticNet", "AICc_GAM", "CV_RMSE_Polynomial", "CV_RMSE_ElasticNet", "CV_RMSE_GAM"), 4)
  })

  output$an_coef_tbl <- renderDT({
    desc <- describe_predictor(an_predictor(), CCPP_TARGET)
    datatable(desc$table, rownames = FALSE, options = list(pageLength = 8)) %>%
      { if ("Koefisien" %in% names(desc$table)) formatRound(., "Koefisien", 5) else . }
  })

  output$an_actual_pred_Polynomial <- renderPlotly({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]; pr <- build_predictor("Polynomial", input$an_route, res$poly_all[[input$an_sheet]], sp$X_train, sp$y_train, k_spline = res$k_spline %||% 20)
    pred <- pr$predict(sp$X_test); d <- data.frame(Aktual = sp$y_test, Prediksi = pred); rng <- range(c(d$Aktual, d$Prediksi)); p <- ggplot(d, aes(Aktual, Prediksi)) + geom_point(alpha = .35, color = "#6366f1") + geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = .7, linetype = "dashed") + coord_equal(xlim = rng, ylim = rng) + labs(x = "PE Aktual", y = "PE Prediksi") + theme_plot(); ggplotly(p)
  })

  output$an_actual_pred_Elastic_Net <- renderPlotly({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]; pr <- build_predictor("Elastic Net", input$an_route, res$en_all[[input$an_sheet]], sp$X_train, sp$y_train, k_spline = res$k_spline %||% 20)
    pred <- pr$predict(sp$X_test); d <- data.frame(Aktual = sp$y_test, Prediksi = pred); rng <- range(c(d$Aktual, d$Prediksi)); p <- ggplot(d, aes(Aktual, Prediksi)) + geom_point(alpha = .35, color = "#6366f1") + geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = .7, linetype = "dashed") + coord_equal(xlim = rng, ylim = rng) + labs(x = "PE Aktual", y = "PE Prediksi") + theme_plot(); ggplotly(p)
  })

  output$an_actual_pred_GAM <- renderPlotly({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]; pr <- build_predictor("GAM", input$an_route, res$gam_all[[input$an_sheet]], sp$X_train, sp$y_train, k_spline = res$k_spline %||% 20)
    pred <- pr$predict(sp$X_test); d <- data.frame(Aktual = sp$y_test, Prediksi = pred); rng <- range(c(d$Aktual, d$Prediksi)); p <- ggplot(d, aes(Aktual, Prediksi)) + geom_point(alpha = .35, color = "#6366f1") + geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = .7, linetype = "dashed") + coord_equal(xlim = rng, ylim = rng) + labs(x = "PE Aktual", y = "PE Prediksi") + theme_plot(); ggplotly(p)
  })

  output$an_tuning_Polynomial <- renderPlotly({
    obj <- res <- req_analysis()$poly_all[[input$an_sheet]]; yval <- if (input$an_route == "AICc") "AICc" else "CV_RMSE"; tbl <- obj$table; p <- ggplot(tbl, aes(x = degree, y = .data[[yval]])) + geom_line(color = "#6366f1") + geom_point(color = "#6366f1") + labs(x = "Derajat", y = yval, title = "Skor per kandidat hyperparameter") + theme_plot(); ggplotly(p)
  })

  output$an_tuning_Elastic_Net <- renderPlotly({
    obj <- req_analysis()$en_all[[input$an_sheet]]; yval <- if (input$an_route == "AICc") "AICc" else "CV_RMSE"; tbl <- obj$table; p <- ggplot(tbl, aes(x = lambda, y = .data[[yval]])) + geom_line(color = "#f59e0b") + geom_point(color = "#f59e0b") + scale_x_log10() + labs(x = "lambda", y = yval, title = "Elastic Net tuning") + theme_plot(); ggplotly(p)
  })

  output$an_tuning_GAM <- renderPlotly({
    obj <- req_analysis()$gam_all[[input$an_sheet]]; yval <- if (input$an_route == "AICc") "AICc" else "CV_RMSE"; tbl <- obj$table; p <- ggplot(tbl, aes(x = lambda, y = .data[[yval]])) + geom_line(color = "#10b981") + geom_point(color = "#10b981") + scale_x_log10() + labs(x = "lambda", y = yval, title = "GAM tuning") + theme_plot(); ggplotly(p)
  })

  # Page 3
  output$eval_body <- renderUI({
    res <- req_analysis()
    counts <- table(c(res$best_model_df$Model_Terbaik_AIC, res$best_model_df$Model_Terbaik_CV))
    overall_model <- names(sort(counts, decreasing = TRUE))[1]
    sheet_top <- res$sheet_names[which.max(table(factor(res$best_model_df$Model_Terbaik_AIC, levels = names(counts))))]

    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Model Terbaik (mayoritas)", value = overall_model, showcase = icon("trophy"), theme = "primary"),
        value_box(title = "Tingkat Kesepakatan AIC vs CV", value = paste0(round(res$agreement_rate * 100), "%"), showcase = icon("handshake"), theme = "info"),
        value_box(title = "Jumlah Sheet Dianalisis", value = length(res$sheet_names), showcase = icon("layer-group"), theme = "success"),
        value_box(title = "K (Cross-Validation)", value = length(res$folds), showcase = icon("repeat"), theme = "warning")
      ),
      card(card_header("Ringkasan Model Terbaik per Sheet (AICc vs CV)"), DTOutput("eval_best_tbl")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Perbandingan AICc Antar Model"), plotlyOutput("eval_aicc_plot", height = 340)),
        card(card_header("Perbandingan CV RMSE Antar Model"), plotlyOutput("eval_cv_plot", height = 340))
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Garis Regresi Model Terpilih pada Data Aktual"), plotlyOutput("eval_selected_fit", height = 380)),
        card(card_header("Ukuran Evaluasi"), DTOutput("eval_metric_summary"))
      )
    )
  })

  output$eval_best_tbl <- renderDT({
    res <- req_analysis(); datatable(res$best_model_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>% formatRound(grep("AICc_|CV_RMSE_", names(res$best_model_df)), 3)
  })

  output$eval_aicc_plot <- renderPlotly({
    res <- req_analysis(); long <- collect_test_summary(res) %>% filter(Route == "AICc"); p <- ggplot(long, aes(Sheet, AICc, fill = Model)) + geom_col(position = "dodge") + scale_fill_manual(values = MODEL_COLORS) + labs(x = NULL, y = "AICc") + theme_plot(); ggplotly(p)
  })

  output$eval_cv_plot <- renderPlotly({
    res <- req_analysis(); long <- collect_test_summary(res) %>% filter(Route == "CV"); p <- ggplot(long, aes(Sheet, CV_RMSE, fill = Model)) + geom_col(position = "dodge") + scale_fill_manual(values = MODEL_COLORS) + labs(x = NULL, y = "CV RMSE") + theme_plot(); ggplotly(p)
  })

  output$eval_metric_summary <- renderDT({
    res <- req_analysis();
    tbl <- collect_test_summary(res) %>%
      group_by(Model, Route) %>%
      summarise(Median_AICc = median(AICc, na.rm = TRUE), Median_CV = median(CV_RMSE, na.rm = TRUE), .groups = "drop")
    datatable(tbl, rownames = FALSE, options = list(dom = "t")) %>% formatRound(c("Median_AICc", "Median_CV"), 4)
  })

  output$eval_selected_fit <- renderPlotly({
    res <- req_analysis();
    if (is.null(res)) return(NULL)
    win_table <- table(c(res$best_model_df$Model_Terbaik_AIC, res$best_model_df$Model_Terbaik_CV))
    best_model <- names(sort(win_table, decreasing = TRUE))[1]
    sh <- res$sheet_names[1]
    sp <- res$splits[[sh]]
    obj <- switch(best_model,
                  "Polynomial" = res$poly_all[[sh]],
                  "Elastic Net" = res$en_all[[sh]],
                  "GAM" = res$gam_all[[sh]])
    pr <- build_predictor(best_model, "AICc", obj, sp$X_train, sp$y_train, k_spline = res$k_spline %||% 20)
    pred <- pr$predict(sp$X_test)
    d <- data.frame(Aktual = sp$y_test, Prediksi = pred)
    p <- ggplot(d, aes(Aktual, Prediksi)) +
      geom_point(alpha = .45, color = "#6366f1") +
      geom_smooth(method = "lm", se = FALSE, color = "#ef4444", linewidth = 1.2) +
      labs(x = "PE Aktual", y = "PE Prediksi", title = sprintf("Model terpilih: %s (Sheet %s)", best_model, sh)) +
      theme_plot()
    ggplotly(p)
  })

  # Page 4
  sim_data <- eventReactive(list(input$sim_generate, input$sim_func), {
    sim_generate(input$sim_func, input$sim_n, input$sim_noise, input$sim_seed)
  }, ignoreNULL = FALSE)

  sim_fits <- reactive({
    d <- sim_data(); list(
      Polynomial = fit_poly1d(d$train, input$sim_poly_degree),
      `Elastic Net` = fit_en1d(d$train, input$sim_en_degree, input$sim_en_alpha, input$sim_en_loglam),
      GAM = fit_gam1d(d$train, input$sim_gam_k, input$sim_gam_auto, input$sim_gam_loglam)
    )
  })

  output$sim_plot_poly <- renderPlotly({
    d <- sim_data(); fit <- fit_poly1d(d$train, input$sim_poly_degree)
    grid <- d$grid
    pred <- fit$predict(grid$x)$fit
    p <- plot_ly() %>%
      add_trace(x = d$train$x, y = d$train$y, type = "scatter", mode = "markers", name = "Data latih", marker = list(color = "#94a3b8", size = 6, opacity = .6)) %>%
      add_trace(x = d$test$x, y = d$test$y, type = "scatter", mode = "markers", name = "Data uji", marker = list(color = "#0f172a", size = 6, symbol = "diamond", opacity = .7)) %>%
      add_trace(x = grid$x, y = d$truth, type = "scatter", mode = "lines", name = "Kebenaran (truth)", line = list(color = "#94a3b8", dash = "dot", width = 2)) %>%
      add_trace(x = grid$x, y = pred, type = "scatter", mode = "lines", name = "Polynomial", line = list(color = "#6366f1", width = 3)) %>%
      layout(xaxis = list(title = "x"), yaxis = list(title = "y"), legend = list(orientation = "h", y = -0.2))
    p
  })

  output$sim_plot_en <- renderPlotly({
    d <- sim_data(); fit <- fit_en1d(d$train, input$sim_en_degree, input$sim_en_alpha, input$sim_en_loglam)
    grid <- d$grid; pred <- fit$predict(grid$x)$fit
    p <- plot_ly() %>%
      add_trace(x = d$train$x, y = d$train$y, type = "scatter", mode = "markers", name = "Data latih", marker = list(color = "#94a3b8", size = 6, opacity = .6)) %>%
      add_trace(x = d$test$x, y = d$test$y, type = "scatter", mode = "markers", name = "Data uji", marker = list(color = "#0f172a", size = 6, symbol = "diamond", opacity = .7)) %>%
      add_trace(x = grid$x, y = d$truth, type = "scatter", mode = "lines", name = "Kebenaran (truth)", line = list(color = "#94a3b8", dash = "dot", width = 2)) %>%
      add_trace(x = grid$x, y = pred, type = "scatter", mode = "lines", name = "Elastic Net", line = list(color = "#f59e0b", width = 3)) %>%
      layout(xaxis = list(title = "x"), yaxis = list(title = "y"), legend = list(orientation = "h", y = -0.2))
    p
  })

  output$sim_plot_gam <- renderPlotly({
    d <- sim_data(); fit <- fit_gam1d(d$train, input$sim_gam_k, input$sim_gam_auto, input$sim_gam_loglam)
    grid <- d$grid; pred <- fit$predict(grid$x)$fit
    p <- plot_ly() %>%
      add_trace(x = d$train$x, y = d$train$y, type = "scatter", mode = "markers", name = "Data latih", marker = list(color = "#94a3b8", size = 6, opacity = .6)) %>%
      add_trace(x = d$test$x, y = d$test$y, type = "scatter", mode = "markers", name = "Data uji", marker = list(color = "#0f172a", size = 6, symbol = "diamond", opacity = .7)) %>%
      add_trace(x = grid$x, y = d$truth, type = "scatter", mode = "lines", name = "Kebenaran (truth)", line = list(color = "#94a3b8", dash = "dot", width = 2)) %>%
      add_trace(x = grid$x, y = pred, type = "scatter", mode = "lines", name = "GAM", line = list(color = "#10b981", width = 3)) %>%
      layout(xaxis = list(title = "x"), yaxis = list(title = "y"), legend = list(orientation = "h", y = -0.2))
    p
  })

  # Page 5 (Upload Data)
  up_state <- reactiveValues(df = NULL, source_label = NULL)

  output$up_sheet_selector <- renderUI({
    req(input$up_file)
    ext <- tolower(tools::file_ext(input$up_file$name))
    if (ext %in% c("xlsx", "xls")) {
      sheets <- tryCatch(readxl::excel_sheets(input$up_file$datapath), error = function(e) NULL)
      if (!is.null(sheets) && length(sheets) > 1)
        selectInput("up_sheet", "Pilih sheet di dalam file", choices = sheets)
    }
  })

  observeEvent(input$up_file, {
    req(input$up_file)
    ext <- tools::file_ext(input$up_file$name)
    d <- tryCatch(read_user_table(input$up_file$datapath, ext, sheet = input$up_sheet),
                 error = function(e) { showNotification(paste("Gagal membaca file:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(d)) { up_state$df <- d; up_state$source_label <- input$up_file$name }
  })

  observeEvent(input$up_use_example, {
    up_state$df <- example_dataset(); up_state$source_label <- "Dataset contoh (disimulasikan)"; showNotification("Dataset contoh dimuat.", type = "message")
  })

  output$up_target_selector <- renderUI({
    req(up_state$df)
    selectInput("up_target", "Variabel Target (Y)", choices = names(up_state$df))
  })

  output$up_features_selector <- renderUI({
    req(up_state$df, input$up_target)
    ch <- setdiff(names(up_state$df), input$up_target)
    checkboxGroupInput("up_features", "Variabel Prediktor (X) — pilih 1-8", choices = ch, selected = head(ch, min(4, length(ch))))
  })

  output$up_validation_msg <- renderUI({
    req(up_state$df, input$up_target)
    err <- validate_auto_inputs(up_state$df, input$up_target, input$up_features)
    if (!is.null(err)) warn_box(err) else info_box(sprintf("Siap dianalisis: %s baris, target <b>%s</b>, %d prediktor.", format(nrow(up_state$df), big.mark = ","), input$up_target, length(input$up_features)))
  })

  up_result <- eventReactive(input$up_run, {
    err <- validate_auto_inputs(up_state$df, input$up_target, input$up_features)
    validate(need(is.null(err), err))
    withProgress(message = "Menjalankan analisis otomatis...", value = 0, {
      auto_analysis(up_state$df, input$up_target, input$up_features,
                    K = input$up_k, test_size = input$up_test_size, seed = 42,
                    on_step = function(msg, frac) setProgress(value = frac, detail = msg))
    })
  })

  output$up_body <- renderUI({
    if (is.null(up_state$df)) {
      return(info_box("Unggah file di sidebar, atau klik \"pakai dataset contoh\" untuk mencoba fitur ini."))
    }
    if (input$up_run == 0) {
      return(tagList(
        card(card_header(paste("Pratinjau Data:", up_state$source_label)), DTOutput("up_preview_tbl")),
        info_box("Pilih target & prediktor di sidebar, lalu klik <b>Jalankan Analisis Otomatis</b>.")
      ))
    }
    res <- up_result()
    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Baris Dipakai", value = format(res$n_used, big.mark = ","), showcase = icon("database"), theme = "primary"),
        value_box(title = "Data Latih / Uji", value = paste(res$n_train, "/", res$n_test), showcase = icon("scale-balanced"), theme = "info"),
        value_box(title = "Model Berhasil", value = length(res$models), showcase = icon("check"), theme = "success"),
        value_box(title = "Model Terbaik (AICc)", value = res$summary$Model[which.min(res$summary$AICc)], showcase = icon("trophy"), theme = "warning")
      ),
      if (length(res$errors)) warn_box(paste("Model gagal dijalankan:", paste(names(res$errors), res$errors, sep = " – ", collapse = "; "))),
      card(card_header("Perbandingan Model: AIC vs Cross-Validation"), DTOutput("up_summary_tbl")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Perbandingan AICc"), plotlyOutput("up_aicc_plot", height = 320)),
        card(card_header("Perbandingan CV RMSE"), plotlyOutput("up_cv_plot", height = 320))
      ),
      card(card_header("Persamaan Model Terbaik (menurut CV)"), uiOutput("up_equation"))
    )
  })

  output$up_preview_tbl <- renderDT(datatable(head(up_state$df, 100), options = list(scrollX = TRUE)))

  output$up_summary_tbl <- renderDT({
    res <- up_result(); datatable(res$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>% formatRound(setdiff(names(res$summary), c("Model", "Config_AICc", "Config_CV", "Rank_AICc", "Rank_CV")), 3)
  })

  output$up_aicc_plot <- renderPlotly({
    res <- up_result(); p <- ggplot(res$summary, aes(reorder(Model, AICc), AICc, fill = Model)) + geom_col(show.legend = FALSE) + coord_flip() + scale_fill_manual(values = MODEL_COLORS) + labs(x = NULL, y = "AICc (lebih kecil lebih baik)") + theme_plot(); ggplotly(p)
  })

  output$up_cv_plot <- renderPlotly({
    res <- up_result(); p <- ggplot(res$summary, aes(reorder(Model, CV_RMSE), CV_RMSE, fill = Model)) + geom_col(show.legend = FALSE) + coord_flip() + scale_fill_manual(values = MODEL_COLORS) + labs(x = NULL, y = "CV RMSE (lebih kecil lebih baik)") + theme_plot(); ggplotly(p)
  })

  output$up_equation <- renderUI({
    res <- up_result(); best <- res$summary$Model[which.min(res$summary$CV_RMSE)]; obj <- res$models[[best]]; pr <- build_predictor(best, "CV", obj, res$Xtr, res$ytr, k_spline = res$k_spline); desc <- describe_predictor(pr, res$target)
    tagList(tags$p(tags$b("Model terpilih: "), best), div(class = "eq-box", desc$eq_std), tags$p(desc$note, class = "text-muted", style = "font-size:0.85rem; margin-top:8px;"))
  })
}

shinyApp(ui, server)
