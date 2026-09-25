# ============================================================
# DASHBOARD REGRESI CCPP
# Perbandingan Polynomial Regression, Elastic Net, dan GAM
# 5 Halaman:
#   1. Eksplorasi Data       (tanpa upload)
#   2. Analisis Data / Model (tanpa upload)
#   3. Evaluasi Model (AIC & CV) (tanpa upload)
#   4. Simulasi Interaktif   (tanpa upload, dataset dummy)
#   5. Upload & Auto-Analisis (dataset milik pengguna)
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

# ------------------------------------------------------------
# STARTUP (dijalankan sekali saat aplikasi start, dipakai bersama
# oleh semua sesi pengguna)
# ------------------------------------------------------------
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
# UI
# ============================================================
ui <- page_navbar(
  title = tagList(icon("bolt"), "Dashboard Regresi CCPP"),
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#6366f1") %>%
    bs_add_rules("
      body, .navbar-brand, .nav-link, .btn, .card { font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', sans-serif; }
      h1, h2, h3, h4, h5, .card-header { font-family: 'Lexend', 'Inter', system-ui, sans-serif; }
      .value-box-title { font-size: 0.8rem !important; opacity:.85; }
      .card { box-shadow: 0 1px 3px rgba(0,0,0,.08); border: 1px solid rgba(0,0,0,.06); }
      .eq-box { background:#0f172a; color:#e2e8f0; padding:16px 18px; border-radius:10px;
                font-family: 'Fira Code', 'Consolas', monospace; white-space:pre-wrap; font-size:0.92rem;
                line-height:1.55; }
      .navbar-brand { font-weight:700; }
    "),
  header = tags$head(tags$style(HTML(".nav-link{font-weight:500;}"))),

  # ---------------- PAGE 1: EKSPLORASI DATA ----------------
  nav_panel(
    title = "1. Eksplorasi Data",
    icon = icon("magnifying-glass-chart"),
    page_sidebar(
      sidebar = sidebar(
        width = 300, title = "Pengaturan Eksplorasi",
        selectInput("eda_sheet", "Pilih Sheet",
                   choices = c("Semua sheet (gabungan)", ccpp$data %>% names())),
        selectInput("eda_var", "Variabel untuk histogram/density",
                   choices = c(CCPP_FEATURES, CCPP_TARGET), selected = CCPP_TARGET),
        hr(),
        p(tags$b("Sumber data:"), style = "margin-bottom:2px;"),
        p(ccpp$source, style = "font-size:0.85rem; color:#64748b;"),
        if (ccpp$simulated)
          div(class = "alert alert-warning", style = "font-size:0.8rem; padding:8px;",
             "File CCPP asli (.xlsx/.ods) tidak ditemukan di folder aplikasi. Dashboard memakai data simulasi bergaya CCPP agar seluruh fitur tetap bisa didemokan.")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Total Baris", value = textOutput("vb_rows"), showcase = icon("table-cells"), theme = "primary"),
        value_box(title = "Jumlah Sheet", value = textOutput("vb_sheets"), showcase = icon("layer-group"), theme = "info"),
        value_box(title = "Rata-rata PE (MW)", value = textOutput("vb_mean_pe"), showcase = icon("gauge"), theme = "success"),
        value_box(title = "Korelasi |PE~AT|", value = textOutput("vb_cor_at"), showcase = icon("link"), theme = "warning")
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Deskripsi Variabel CCPP"), tableOutput("var_info_tbl")),
        card(card_header("Statistik Deskriptif"), DTOutput("desc_stats_tbl"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Distribusi Variabel Terpilih"), plotlyOutput("eda_hist", height = 320)),
        card(card_header("Heatmap Korelasi Antar-Variabel"), plotlyOutput("eda_corr", height = 320))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("PE vs Setiap Prediktor (dengan garis tren)"), plotlyOutput("eda_scatter", height = 360)),
        card(card_header("Konsistensi Distribusi PE Antar-Sheet"), plotlyOutput("eda_boxplot", height = 360))
      ),
      card(
        card_header("Pemeriksaan Kualitas Data (missing value & outlier per-IQR)"),
        DTOutput("eda_quality_tbl")
      )
    )
  ),

  # ---------------- PAGE 2: ANALISIS DATA CCPP ----------------
  nav_panel(
    title = "2. Analisis Model",
    icon = icon("chart-line"),
    page_sidebar(
      sidebar = sidebar(
        width = 300, title = "Pengaturan Analisis",
        selectInput("an_sheet", "Pilih Sheet", choices = NULL),
        selectInput("an_model", "Pilih Model", choices = MODEL_NAMES),
        radioButtons("an_route", "Rute Pemilihan Model", choices = c("AICc" = "AICc", "Cross-Validation" = "CV")),
        hr(),
        uiOutput("an_status_note")
      ),
      uiOutput("an_body")
    )
  ),

  # ---------------- PAGE 3: EVALUASI MODEL ----------------
  nav_panel(
    title = "3. Evaluasi Model",
    icon = icon("scale-balanced"),
    uiOutput("eval_body")
  ),

  # ---------------- PAGE 4: SIMULASI INTERAKTIF ----------------
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
        sliderInput("sim_en_alpha", "Rasio L1 (\u03b1: 0=Ridge, 1=Lasso)", 0, 1, 0.5, step = 0.05),
        sliderInput("sim_en_loglam", "log10(\u03bb) relatif", -4, 1, -2, step = 0.1),
        hr(),
        p(tags$b("GAM"), style="margin-bottom:4px;"),
        sliderInput("sim_gam_k", "Basis spline (k)", 4, 30, 10),
        checkboxInput("sim_gam_auto", "\u03bb otomatis (GCV)", TRUE),
        conditionalPanel("!input.sim_gam_auto",
          sliderInput("sim_gam_loglam", "log10(\u03bb) manual", -3, 3, 0, step = 0.1))
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Perbandingan 3 Model pada Data Dummy (geser slider untuk lihat pengaruhnya)"),
          plotlyOutput("sim_plot", height = 460)
        )
      ),
      card(
        card_header("Metrik Model (dihitung ulang otomatis saat slider digeser)"),
        DTOutput("sim_metrics_tbl")
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Efek Kompleksitas Model terhadap AICc & RMSE-Test (model aktif = Polynomial)"),
          plotlyOutput("sim_sweep_plot", height = 320)
        )
      )
    )
  ),

  # ---------------- PAGE 5: UPLOAD & AUTO-ANALISIS ----------------
  nav_panel(
    title = "5. Upload Data Anda",
    icon = icon("upload"),
    page_sidebar(
      sidebar = sidebar(
        width = 340, title = "1. Sediakan Data",
        fileInput("up_file", "Unggah file (CSV / XLSX / XLS / ODS)",
                 accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".ods")),
        uiOutput("up_sheet_selector"),
        actionButton("up_use_example", "Atau pakai dataset contoh",
                    icon = icon("flask"), class = "btn-outline-secondary w-100"),
        hr(),
        h6("2. Pilih Variabel"),
        uiOutput("up_target_selector"),
        uiOutput("up_features_selector"),
        hr(),
        h6("3. Pengaturan Analisis"),
        sliderInput("up_k", "Jumlah fold Cross-Validation (K)", 3, 15, 10),
        sliderInput("up_test_size", "Proporsi data uji", 0.1, 0.4, 0.2, step = 0.05),
        actionButton("up_run", "Jalankan Analisis Otomatis", icon = icon("play"),
                    class = "btn-primary w-100"),
        uiOutput("up_validation_msg")
      ),
      uiOutput("up_body")
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text", style = "font-size:0.8rem; opacity:.7;",
                     "Polynomial \u00b7 Elastic Net \u00b7 GAM"))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ----------------------------------------------------------
  # ANALISIS CCPP (Page 1-3) -- dihitung sekali, dicache ke disk
  # ----------------------------------------------------------
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
      res <- build_ccpp_analysis(ccpp$data, on_step = function(msg, frac) {
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
    if (!is.null(res)) {
      updateSelectInput(session, "an_sheet", choices = res$sheet_names)
    }
  })

  # ---------------- PAGE 1: EKSPLORASI DATA ----------------
  eda_df <- reactive({
    if (identical(input$eda_sheet, "Semua sheet (gabungan)")) {
      dplyr::bind_rows(ccpp$data, .id = "Sheet")
    } else {
      d <- ccpp$data[[input$eda_sheet]]; d$Sheet <- input$eda_sheet; d
    }
  })

  output$vb_rows <- renderText(format(nrow(eda_df()), big.mark = ","))
  output$vb_sheets <- renderText(length(ccpp$data))
  output$vb_mean_pe <- renderText(fmt_num(mean(eda_df()$PE), 1))
  output$vb_cor_at <- renderText(fmt_num(abs(cor(eda_df()$AT, eda_df()$PE)), 3))

  output$var_info_tbl <- renderTable(VAR_INFO, striped = TRUE, width = "100%")

  output$desc_stats_tbl <- renderDT({
    d <- eda_df()[, c(CCPP_FEATURES, CCPP_TARGET)]
    stats <- do.call(rbind, lapply(names(d), function(v) {
      x <- d[[v]]
      data.frame(Variabel = v, Min = min(x), Q1 = quantile(x, .25), Median = median(x),
                Mean = mean(x), Q3 = quantile(x, .75), Max = max(x), SD = sd(x))
    }))
    datatable(stats, rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatRound(columns = 2:8, digits = 2)
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

  output$eda_corr <- renderPlotly({
    d <- eda_df()[, c(CCPP_FEATURES, CCPP_TARGET)]
    cm <- round(cor(d), 2)
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap",
           colors = colorRamp(c("#ef4444", "white", "#6366f1")), zmin = -1, zmax = 1,
           text = cm, texttemplate = "%{text}") %>%
      layout(margin = list(t = 10))
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
        q <- quantile(x, c(.25, .75)); iqr <- q[2] - q[1]
        lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
        data.frame(Sheet = sh, Variabel = v, Missing = sum(is.na(x)),
                  Outlier_IQR = sum(x < lo | x > hi, na.rm = TRUE),
                  Persen_Outlier = round(100 * mean(x < lo | x > hi, na.rm = TRUE), 2))
      })
      dplyr::bind_rows(out)
    })
    datatable(dplyr::bind_rows(rows), rownames = FALSE, options = list(pageLength = 10))
  })

  # ---------------- PAGE 2: ANALISIS MODEL PER SHEET ----------------
  an_obj <- reactive({
    res <- req_analysis()
    req(input$an_sheet)
    switch(input$an_model,
          "Polynomial" = res$poly_all[[input$an_sheet]],
          "Elastic Net" = res$en_all[[input$an_sheet]],
          "GAM" = res$gam_all[[input$an_sheet]])
  })

  an_predictor <- reactive({
    res <- req_analysis()
    sp <- res$splits[[input$an_sheet]]
    build_predictor(input$an_model, input$an_route, an_obj(), sp$X_train, sp$y_train,
                    k_spline = res$k_spline %||% 20)
  })

  output$an_status_note <- renderUI({
    res <- req_analysis()
    gf <- attr(res, "gam_fallback"); mf <- attr(res, "model_failed")
    tags <- list()
    if (length(gf)) tags <- c(tags, list(warn_box(sprintf(
      "\u26a0\ufe0f GAM pada sheet <b>%s</b> memakai basis lebih kecil (mode ringan) karena konvergensi standar lambat.",
      paste(gf, collapse = ", ")))))
    if (length(mf)) tags <- c(tags, list(warn_box(sprintf(
      "\u26a0\ufe0f GAM pada sheet <b>%s</b> digantikan regresi linear sementara (kendala numerik ganda).",
      paste(mf, collapse = ", ")))))
    ci <- attr(res, "cap_info")
    row <- ci[ci$Sheet == input$an_sheet, ]
    if (nrow(row) && isTRUE(row$Disubsample)) {
      tags <- c(tags, list(info_box(sprintf(
        "\u2139\ufe0f Sheet ini disubsample acak %s dari %s baris agar performa dashboard tetap responsif (seed tetap).",
        format(row$Baris_Digunakan, big.mark=","), format(row$Baris_Total, big.mark=",")))))
    }
    tagList(tags)
  })

  output$an_body <- renderUI({
    req_analysis()
    obj <- an_obj(); rt <- route_key(input$an_route)
    fin <- obj$final[[rt]]
    tc <- tuning_counts(req_analysis(), input$an_sheet, input$an_model, input$an_route)

    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "AICc", value = if (is.na(fin$AICc)) "-" else fmt_num(fin$AICc, 1), showcase = icon("chart-simple"), theme = "primary"),
        value_box(title = "CV RMSE", value = if (is.na(fin$CV_RMSE)) "-" else fmt_num(fin$CV_RMSE, 3), showcase = icon("layer-group"), theme = "info"),
        value_box(title = "Test RMSE", value = fmt_num(fin$test_RMSE, 3), showcase = icon("bullseye"), theme = "success"),
        value_box(title = "Test R\u00b2", value = fmt_num(fin$test_R2, 4), showcase = icon("percent"), theme = "warning")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header(paste("Persamaan Model \u2014", input$an_model, "/", input$an_route)),
            uiOutput("an_equation")),
        card(card_header("Info Tuning Hyperparameter"),
            tags$ul(
              tags$li(sprintf("Konfigurasi terpilih: %s", fin$config)),
              tags$li(sprintf("Kandidat dicoba: %s", tc$candidates)),
              tags$li(sprintf("Total model di-fit: %s", tc$fits)),
              tags$li(tc$note)
            ),
            tags$p(class = "text-muted", style="font-size:0.85rem;",
                  "Rute AICc memilih model dari kualitas fit pada seluruh data latih (hukuman kompleksitas via AICc). Rute CV memilih dari rata-rata error pada data yang disisihkan bergilir (K-fold).")
        )
      ),
      card(card_header("Tabel Koefisien / Ringkasan Smooth Term"), DTOutput("an_coef_tbl")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Aktual vs Prediksi (Data Uji)"), plotlyOutput("an_actual_pred", height = 340)),
        card(card_header(sprintf("Kurva Tuning: %s vs %s", if (input$an_route == "AICc") "AICc" else "CV RMSE", "Parameter")),
            plotlyOutput("an_tuning_curve", height = 340))
      ),
      card(card_header("Efek Parsial Tiap Prediktor terhadap PE (variabel lain di rata-rata)"),
          plotlyOutput("an_partial", height = 380))
    )
  })

  output$an_equation <- renderUI({
    desc <- describe_predictor(an_predictor(), CCPP_TARGET)
    tagList(
      div(class = "eq-box", desc$eq_std),
      if (!is.null(desc$eq_orig)) tagList(tags$p(tags$b("Dalam satuan asli:"), style="margin-top:10px;margin-bottom:4px;"),
                                          div(class = "eq-box", desc$eq_orig)),
      tags$p(desc$note, class = "text-muted", style = "font-size:0.85rem; margin-top:10px;")
    )
  })

  output$an_coef_tbl <- renderDT({
    desc <- describe_predictor(an_predictor(), CCPP_TARGET)
    datatable(desc$table, rownames = FALSE, options = list(pageLength = 8)) %>%
      { if ("Koefisien" %in% names(desc$table)) formatRound(., "Koefisien", 5) else . }
  })

  output$an_actual_pred <- renderPlotly({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]
    pr <- an_predictor()
    pred <- pr$predict(sp$X_test)
    d <- data.frame(Aktual = sp$y_test, Prediksi = pred)
    rng <- range(c(d$Aktual, d$Prediksi))
    p <- ggplot(d, aes(Aktual, Prediksi)) +
      geom_point(alpha = .35, color = "#6366f1") +
      geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = .7, linetype = "dashed") +
      coord_equal(xlim = rng, ylim = rng) +
      labs(x = "PE Aktual", y = "PE Prediksi") + theme_plot()
    ggplotly(p)
  })

  output$an_tuning_curve <- renderPlotly({
    obj <- an_obj(); yval <- if (input$an_route == "AICc") "AICc" else "CV_RMSE"
    tbl <- if (input$an_model == "Elastic Net" && input$an_route == "CV") obj$cv_grid else obj$table
    validate(need(nrow(tbl) > 0 && yval %in% names(tbl), "Kurva tuning tidak tersedia untuk konfigurasi ini."))
    xcol <- intersect(c("degree", "lambda"), names(tbl))[1]
    p <- ggplot(tbl, aes(x = .data[[xcol]], y = .data[[yval]])) +
      geom_line(color = "#6366f1") + geom_point(color = "#6366f1") +
      { if (xcol == "lambda") scale_x_log10() } +
      labs(x = xcol, y = yval, title = "Skor per kandidat hyperparameter") + theme_plot()
    ggplotly(p)
  })

  output$an_partial <- renderPlotly({
    res <- req_analysis(); sp <- res$splits[[input$an_sheet]]
    pr <- an_predictor()
    pe <- partial_effects(setNames(list(pr), input$an_model), sp$X_train, CCPP_FEATURES, n = 60)
    p <- ggplot(pe, aes(x, yhat)) +
      geom_line(color = "#6366f1", linewidth = .9) +
      facet_wrap(~Feature, scales = "free_x") +
      labs(x = NULL, y = "Prediksi PE") + theme_plot()
    ggplotly(p, height = 360)
  })

  # ---------------- PAGE 3: EVALUASI SELURUH MODEL ----------------
  output$eval_body <- renderUI({
    res <- req_analysis()
    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Model Terbaik (mayoritas)",
                 value = names(sort(table(c(res$best_model_df$Model_Terbaik_AIC, res$best_model_df$Model_Terbaik_CV)), decreasing = TRUE))[1],
                 showcase = icon("trophy"), theme = "primary"),
        value_box(title = "Tingkat Kesepakatan AIC vs CV",
                 value = paste0(round(res$agreement_rate * 100), "%"), showcase = icon("handshake"), theme = "info"),
        value_box(title = "Jumlah Sheet Dianalisis", value = length(res$sheet_names), showcase = icon("layer-group"), theme = "success"),
        value_box(title = "K (Cross-Validation)", value = length(res$folds), showcase = icon("repeat"), theme = "warning")
      ),
      if (length(attr(res, "gam_fallback")) || length(attr(res, "model_failed")))
        warn_box(sprintf(
          "Beberapa sheet memakai penyederhanaan otomatis pada GAM karena kendala konvergensi numerik: %s. Lihat detail per-sheet di halaman Analisis Model.",
          paste(unique(c(attr(res,"gam_fallback"), attr(res,"model_failed"))), collapse = ", "))),
      card(card_header("Ringkasan Model Terbaik per Sheet (AICc vs CV)"), DTOutput("eval_best_tbl")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Perbandingan AICc Antar Model (lebih kecil lebih baik)"), plotlyOutput("eval_aicc_plot", height = 360)),
        card(card_header("Perbandingan CV RMSE Antar Model (lebih kecil lebih baik)"), plotlyOutput("eval_cv_plot", height = 360))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Stabilitas CV: Sebaran RMSE per Fold per Model"), plotlyOutput("eval_cv_box", height = 360)),
        card(card_header("Uji ANOVA / Kruskal-Wallis (RMSE-fold antar Sheet)"), DTOutput("eval_anova_tbl"))
      ),
      card(card_header("Korelasi Peringkat Spearman antara Ranking AICc & CV per Sheet"), DTOutput("eval_rank_tbl")),
      card(card_header("Tabel Lengkap Seluruh Kombinasi Sheet \u00d7 Model \u00d7 Rute"), DTOutput("eval_full_tbl"))
    )
  })

  output$eval_best_tbl <- renderDT({
    res <- req_analysis()
    datatable(res$best_model_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(grep("AICc_|CV_RMSE_", names(res$best_model_df)), 3)
  })

  output$eval_aicc_plot <- renderPlotly({
    res <- req_analysis()
    long <- collect_test_summary(res) %>% filter(Route == "AICc")
    p <- ggplot(long, aes(Sheet, AICc, fill = Model)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(x = NULL, y = "AICc") + theme_plot()
    ggplotly(p)
  })

  output$eval_cv_plot <- renderPlotly({
    res <- req_analysis()
    long <- collect_test_summary(res) %>% filter(Route == "CV")
    p <- ggplot(long, aes(Sheet, CV_RMSE, fill = Model)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(x = NULL, y = "CV RMSE") + theme_plot()
    ggplotly(p)
  })

  output$eval_cv_box <- renderPlotly({
    res <- req_analysis()
    long <- purrr::imap_dfr(res$cv_fold_store, function(bysheet, model) {
      purrr::imap_dfr(bysheet, function(v, sh) data.frame(Model = model, Sheet = sh, RMSE = v))
    })
    p <- ggplot(long, aes(Model, RMSE, fill = Model)) +
      geom_boxplot(alpha = .85, show.legend = FALSE) +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(x = NULL, y = "RMSE per fold") + theme_plot()
    ggplotly(p)
  })

  output$eval_anova_tbl <- renderDT({
    datatable(req_analysis()$anova_df, rownames = FALSE, options = list(dom = "t")) %>%
      formatRound(c("ANOVA_F", "ANOVA_p", "KruskalWallis_H", "KruskalWallis_p"), 4)
  })

  output$eval_rank_tbl <- renderDT({
    datatable(req_analysis()$rank_corr_df, rownames = FALSE, options = list(dom = "t")) %>%
      formatRound(c("Spearman_rho", "p_value"), 4)
  })

  output$eval_full_tbl <- renderDT({
    datatable(collect_test_summary(req_analysis()), rownames = FALSE,
             options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("AICc", "CV_RMSE", "Test_RMSE", "Test_R2"), 4)
  })

  # ---------------- PAGE 4: SIMULASI INTERAKTIF ----------------
  sim_data <- eventReactive(list(input$sim_generate, input$sim_func), {
    sim_generate(input$sim_func, input$sim_n, input$sim_noise, input$sim_seed)
  }, ignoreNULL = FALSE)

  sim_fits <- reactive({
    d <- sim_data()
    list(
      Polynomial = fit_poly1d(d$train, input$sim_poly_degree),
      `Elastic Net` = fit_en1d(d$train, input$sim_en_degree, input$sim_en_alpha, input$sim_en_loglam),
      GAM = fit_gam1d(d$train, input$sim_gam_k, input$sim_gam_auto, input$sim_gam_loglam)
    )
  })

  output$sim_plot <- renderPlotly({
    d <- sim_data(); fits <- sim_fits()
    grid <- d$grid
    curves <- purrr::imap_dfr(fits, function(f, nm) {
      pr <- f$predict(grid$x)
      data.frame(x = grid$x, yhat = pr$fit, Model = nm)
    })
    p <- plot_ly()
    p <- add_trace(p, x = d$train$x, y = d$train$y, type = "scatter", mode = "markers",
                   name = "Data latih", marker = list(color = "#94a3b8", size = 6, opacity = .6))
    p <- add_trace(p, x = d$test$x, y = d$test$y, type = "scatter", mode = "markers",
                   name = "Data uji", marker = list(color = "#0f172a", size = 6, symbol = "diamond", opacity = .7))
    p <- add_trace(p, x = grid$x, y = d$truth, type = "scatter", mode = "lines",
                   name = "Kebenaran (truth)", line = list(color = "#94a3b8", dash = "dot", width = 2))
    for (nm in names(fits)) {
      sub <- curves[curves$Model == nm, ]
      p <- add_trace(p, x = sub$x, y = sub$yhat, type = "scatter", mode = "lines", name = nm,
                    line = list(color = unname(MODEL_COLORS[nm]), width = 3))
    }
    p %>% layout(xaxis = list(title = "x"), yaxis = list(title = "y"),
                legend = list(orientation = "h", y = -0.2))
  })

  output$sim_metrics_tbl <- renderDT({
    d <- sim_data(); fits <- sim_fits()
    tbl <- purrr::imap_dfr(fits, function(f, nm) {
      m <- sim_metrics(d, f)
      cv <- sim_cv(d$train, function(tr) {
        switch(nm,
              "Polynomial" = fit_poly1d(tr, input$sim_poly_degree),
              "Elastic Net" = fit_en1d(tr, input$sim_en_degree, input$sim_en_alpha, input$sim_en_loglam),
              "GAM" = fit_gam1d(tr, input$sim_gam_k, input$sim_gam_auto, input$sim_gam_loglam))
      })
      data.frame(Model = nm, `Kompleksitas (k)` = round(m$k, 2), AICc = m$AICc,
                RMSE_Train = m$RMSE_train, RMSE_Test = m$RMSE_test, CV_RMSE = cv,
                R2_Test = m$R2_test, check.names = FALSE)
    })
    datatable(tbl, rownames = FALSE, options = list(dom = "t")) %>%
      formatRound(c("AICc", "RMSE_Train", "RMSE_Test", "CV_RMSE", "R2_Test"), 3) %>%
      formatStyle("Model", target = "row", fontWeight = styleEqual(
        tbl$Model[which.min(tbl$AICc)], "bold"))
  })

  output$sim_sweep_plot <- renderPlotly({
    d <- sim_data()
    sweep <- sim_sweep(d, 1:12, function(tr, v) fit_poly1d(tr, v))
    sweep_long <- tidyr::pivot_longer(sweep, c(AICc, RMSE_test), names_to = "Metrik", values_to = "Nilai")
    p <- ggplot(sweep_long, aes(x = value, y = Nilai, color = Metrik)) +
      geom_line(linewidth = .9) + geom_point() +
      geom_vline(xintercept = input$sim_poly_degree, linetype = "dashed", color = "#ef4444") +
      facet_wrap(~Metrik, scales = "free_y") +
      labs(x = "Derajat Polinomial", y = NULL,
          title = "Garis putus-putus merah = derajat yang sedang dipilih pada slider") +
      theme_plot() + theme(legend.position = "none")
    ggplotly(p)
  })

  # ---------------- PAGE 5: UPLOAD & AUTO-ANALISIS ----------------
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
  observeEvent(input$up_sheet, {
    req(input$up_file, input$up_sheet)
    d <- tryCatch(read_user_table(input$up_file$datapath, tools::file_ext(input$up_file$name), sheet = input$up_sheet),
                 error = function(e) NULL)
    if (!is.null(d)) up_state$df <- d
  })
  observeEvent(input$up_use_example, {
    up_state$df <- example_dataset()
    up_state$source_label <- "Dataset contoh (disimulasikan)"
    showNotification("Dataset contoh dimuat.", type = "message")
  })

  output$up_target_selector <- renderUI({
    req(up_state$df)
    selectInput("up_target", "Variabel Target (Y)", choices = names(up_state$df))
  })
  output$up_features_selector <- renderUI({
    req(up_state$df, input$up_target)
    ch <- setdiff(names(up_state$df), input$up_target)
    checkboxGroupInput("up_features", "Variabel Prediktor (X) \u2014 pilih 1-8",
                       choices = ch, selected = head(ch, min(4, length(ch))))
  })

  output$up_validation_msg <- renderUI({
    req(up_state$df, input$up_target)
    err <- validate_auto_inputs(up_state$df, input$up_target, input$up_features)
    if (!is.null(err)) warn_box(err) else info_box(sprintf(
      "Siap dianalisis: %s baris, target <b>%s</b>, %d prediktor.",
      format(nrow(up_state$df), big.mark = ","), input$up_target, length(input$up_features)))
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
      if (length(res$errors)) warn_box(paste("Model gagal dijalankan:",
                                             paste(names(res$errors), res$errors, sep = " \u2013 ", collapse = "; "))),
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
    res <- up_result()
    datatable(res$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
      formatRound(setdiff(names(res$summary), c("Model", "Config_AICc", "Config_CV", "Rank_AICc", "Rank_CV")), 3)
  })

  output$up_aicc_plot <- renderPlotly({
    res <- up_result()
    p <- ggplot(res$summary, aes(reorder(Model, AICc), AICc, fill = Model)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(x = NULL, y = "AICc (lebih kecil lebih baik)") + theme_plot()
    ggplotly(p)
  })
  output$up_cv_plot <- renderPlotly({
    res <- up_result()
    p <- ggplot(res$summary, aes(reorder(Model, CV_RMSE), CV_RMSE, fill = Model)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(x = NULL, y = "CV RMSE (lebih kecil lebih baik)") + theme_plot()
    ggplotly(p)
  })

  output$up_equation <- renderUI({
    res <- up_result()
    best <- res$summary$Model[which.min(res$summary$CV_RMSE)]
    obj <- res$models[[best]]
    pr <- build_predictor(best, "CV", obj, res$Xtr, res$ytr, k_spline = res$k_spline)
    desc <- describe_predictor(pr, res$target)
    tagList(
      tags$p(tags$b("Model terpilih: "), best),
      div(class = "eq-box", desc$eq_std),
      tags$p(desc$note, class = "text-muted", style = "font-size:0.85rem; margin-top:8px;")
    )
  })
}

shinyApp(ui, server)
