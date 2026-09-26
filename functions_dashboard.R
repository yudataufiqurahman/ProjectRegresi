# ============================================================
# FUNCTIONS DASHBOARD (pelengkap functions_regresi_ccpp.R)
# - loader data CCPP + fallback simulasi
# - cache hasil analisis
# - membangun ulang model terpilih (persamaan, grafik, diagnostik)
# - simulasi interaktif 1-D (Polynomial / Elastic Net / GAM)
# - analisis otomatis untuk dataset unggahan pengguna
# ============================================================

MODEL_NAMES  <- c("Polynomial", "Elastic Net", "GAM")
MODEL_COLORS <- c(Polynomial = "#6366f1", `Elastic Net` = "#f59e0b", GAM = "#10b981")
CCPP_FEATURES <- c("AT", "V", "AP", "RH")
CCPP_TARGET   <- "PE"

VAR_INFO <- data.frame(
  Variabel  = c("AT", "V", "AP", "RH", "PE"),
  Arti      = c("Ambient Temperature", "Exhaust Vacuum", "Ambient Pressure",
                "Relative Humidity", "Net hourly electrical energy output"),
  Satuan    = c("°C", "cm Hg", "mbar", "%", "MW"),
  Peran     = c("Prediktor", "Prediktor", "Prediktor", "Prediktor", "Target"),
  stringsAsFactors = FALSE
)

route_key <- function(route) if (identical(route, "AICc")) "AIC" else "CV"

FUNCS_PATH <- normalizePath("functions_regresi_ccpp.R", mustWork = FALSE)

# Guard proses: menjalankan satu fungsi berat (mis. run_gam) di SUBPROCESS
# terpisah lewat callr, dengan batas waktu keras. Sengaja memakai r_bg() +
# polling + kill(grace = 0) manual -- bukan callr::r(timeout=...) tingkat
# tinggi -- karena pengujian menunjukkan callr::r() tidak selalu mem-
# force-kill cukup cepat saat subprocess sedang di tengah pemanggilan C
# (mgcv) yang tidak sering memeriksa sinyal interupsi; sisa proses itu
# bisa terus berjalan di latar belakang dan merebut CPU dari proses utama.
# kill(grace = 0) mengirim SIGKILL langsung, memastikan proses benar-benar
# mati sebelum kita lanjut, sehingga proses utama Shiny tidak pernah ikut
# melambat atau macet akibat sisa komputasi yang terbunuh.
run_in_subprocess <- function(fn_name, args, timeout_sec, funcs_path = FUNCS_PATH,
                              poll_ms = 200) {
  p <- tryCatch(
    callr::r_bg(
      func = function(fn_name, args, fp) { source(fp); do.call(get(fn_name), args) },
      args = list(fn_name = fn_name, args = args, fp = funcs_path)
    ),
    error = function(e) NULL
  )
  if (is.null(p)) return(NULL)

  deadline <- Sys.time() + timeout_sec
  repeat {
    if (!p$is_alive()) break
    if (Sys.time() >= deadline) {
      p$kill(grace = 0)
      for (i in 1:10) { if (!p$is_alive()) break; Sys.sleep(0.05) }
      return(NULL)
    }
    Sys.sleep(poll_ms / 1000)
  }
  tryCatch(p$get_result(), error = function(e) NULL)
}

MAX_ROWS_PER_SHEET <- 2000L

# Subsample acak (seed tetap) agar sheet besar tetap responsif untuk
# analisis model (EDA tetap memakai data penuh, lihat load_ccpp_data()).
cap_rows <- function(df, max_rows = MAX_ROWS_PER_SHEET, seed = 42) {
  if (nrow(df) <= max_rows) return(list(data = df, capped = FALSE, n_used = nrow(df), n_total = nrow(df)))
  set.seed(seed)
  idx <- sample.int(nrow(df), max_rows)
  list(data = df[idx, , drop = FALSE], capped = TRUE, n_used = max_rows, n_total = nrow(df))
}

# KONSISTENSI dengan app_improved.R:
# - validate_ccpp_data & make_train_test_split dipanggil langsung
# - tidak perlu wrapper run_full_analysis() yang custom di sini
# - run_polynomial, run_elasticnet, run_gam dipanggil via hook di app

# ------------------------------------------------------------
# 1. Loader data CCPP (+ fallback simulasi)
# ------------------------------------------------------------
find_ccpp_files <- function(dirs = c(".", "data")) {
  files <- unlist(lapply(dirs, function(d) {
    if (dir.exists(d)) {
      list.files(d, pattern = "\\.(xlsx|xls|ods)$", full.names = TRUE, ignore.case = TRUE)
    } else character(0)
  }))
  files <- files[!grepl("^~\\$", basename(files))]
  if (length(files) == 0) return(character(0))
  prefer <- grepl("ccpp|folds5x2|power|plant", basename(files), ignore.case = TRUE)
  files[order(!prefer)]
}

simulate_ccpp <- function(n = 1200, n_sheets = 3, seed = 2024) {
  set.seed(seed)
  AT <- stats::runif(n, 2, 37)
  V  <- pmin(pmax(28 + 0.85 * AT + stats::rnorm(n, 0, 13), 25.4), 81.6)
  AP <- pmin(pmax(1019 - 0.25 * AT + stats::rnorm(n, 0, 8), 993), 1033)
  RH <- pmin(pmax(88 - 0.55 * AT + stats::rnorm(n, 0, 16), 26), 100)
  PE <- 497 - 1.75 * AT - 0.010 * AT^2 - 0.22 * V + 0.06 * (AP - 1013) -
    0.13 * (RH - 73) + stats::rnorm(n, 0, 4)
  base <- data.frame(AT = AT, V = V, AP = AP, RH = RH, PE = PE)
  sheets <- lapply(seq_len(n_sheets), function(i) base[sample.int(n), , drop = FALSE])
  stats::setNames(lapply(sheets, function(d) { rownames(d) <- NULL; d }),
                  paste0("Sheet", seq_len(n_sheets)))
}

load_ccpp_data <- function() {
  for (f in find_ccpp_files()) {
    d <- tryCatch({
      x <- read_ccpp_file(f)
      validate_ccpp_data(x, CCPP_TARGET, CCPP_FEATURES)
      x
    }, error = function(e) NULL)
    if (!is.null(d)) {
      d <- lapply(d, function(s) {
        s <- s[, c(CCPP_FEATURES, CCPP_TARGET), drop = FALSE]
        s[] <- lapply(s, as.numeric)
        s
      })
      info <- file.info(f)
      return(list(
        data = d, source = basename(f), simulated = FALSE,
        signature = paste(basename(f), info$size, as.numeric(info$mtime))
      ))
    }
  }
  list(data = simulate_ccpp(), source = "Data simulasi mirip CCPP", simulated = TRUE,
       signature = "simulated-v1")
}

# ------------------------------------------------------------
# 2. Cache analisis + progress
# ------------------------------------------------------------
CACHE_PATH <- file.path("cache", "analysis_ccpp.rds")

slim_analysis <- function(res) {
  for (sh in res$sheet_names) {
    g <- res$gam_all[[sh]]
    if (!is.null(g$gcv_model)) g$gcv_sp <- as.numeric(g$gcv_model$sp)
    g$gcv_model <- NULL
    g$cv_model <- NULL
    res$gam_all[[sh]] <- g
  }
  res
}

load_cached_analysis <- function(signature) {
  if (!file.exists(CACHE_PATH)) return(NULL)
  obj <- tryCatch(readRDS(CACHE_PATH), error = function(e) NULL)
  if (is.list(obj) && identical(obj$signature, signature)) obj$result else NULL
}

save_cached_analysis <- function(result, signature) {
  tryCatch({
    dir.create(dirname(CACHE_PATH), showWarnings = FALSE, recursive = TRUE)
    saveRDS(list(signature = signature, result = result), CACHE_PATH)
    TRUE
  }, error = function(e) FALSE)
}

GAM_FALLBACK_LAMBDA <- 10^seq(-2, 2, length.out = 5)
GAM_FALLBACK_K <- 8
GAM_FIT_TIMEOUT_SEC <- 15

gam_unavailable_result <- function(Xtr, ytr, Xte, yte, folds, reason = "Timeout") {
  n <- length(ytr)
  fit <- fit_linear_matrix(Xtr, ytr)
  co <- fit$coefficients; co[is.na(co)] <- 0
  pred_train <- as.numeric(cbind(1, as.matrix(Xtr)) %*% co)
  rss <- sum((ytr - pred_train)^2)
  k <- ncol(Xtr) + 1
  aicc <- compute_aicc(n, rss, k)

  fold_rmse <- vapply(folds, function(val_idx) {
    tr_idx <- setdiff(seq_len(n), val_idx)
    f <- fit_linear_matrix(Xtr[tr_idx, , drop = FALSE], ytr[tr_idx])
    c2 <- f$coefficients; c2[is.na(c2)] <- 0
    rmse(ytr[val_idx], as.numeric(cbind(1, as.matrix(Xtr[val_idx, , drop = FALSE])) %*% c2))
  }, numeric(1))

  pred_test <- as.numeric(cbind(1, as.matrix(Xte)) %*% co)
  list(table = data.frame(lambda = NA_real_, CV_RMSE = sqrt(mean(fold_rmse^2))),
       final = list(
         AIC = list(config = "fallback=linear", AICc = aicc, CV_RMSE = NA_real_,
                    test_RMSE = rmse(yte, pred_test), test_R2 = r2_score(yte, pred_test)),
         CV = list(config = "fallback=linear", AICc = NA_real_, CV_RMSE = sqrt(mean(fold_rmse^2)),
                   test_RMSE = rmse(yte, pred_test), test_R2 = r2_score(yte, pred_test))
       ),
       cv_fold_rmse = fold_rmse, degraded = TRUE, error = reason)
}

run_gam_protected <- function(Xtr, ytr, Xte, yte, folds, on_note = function(msg) NULL,
                              timeout_sec = GAM_FIT_TIMEOUT_SEC, ...) {
  args <- list(Xtr = Xtr, ytr = ytr, Xte = Xte, yte = yte, folds = folds, ...)
  res <- run_in_subprocess("run_gam", args, timeout_sec = timeout_sec)
  status <- "ok"
  if (is.null(res)) {
    on_note("GAM (mode ringan – konvergensi standar lambat)")
    args2 <- args; args2$lambda_grid <- GAM_FALLBACK_LAMBDA; args2$k_spline <- GAM_FALLBACK_K
    res <- run_in_subprocess("run_gam", args2, timeout_sec = timeout_sec)
    status <- "light_fallback"
  }
  if (is.null(res)) {
    on_note("GAM → fallback regresi linear (kendala numerik)")
    res <- gam_unavailable_result(Xtr, ytr, Xte, yte, folds, reason = "Timeout ganda")
    status <- "linear_fallback"
  }
  attr(res, "gam_status") <- status
  res
}

# ------------------------------------------------------------
# 3. Ringkasan lintas-sheet
# ------------------------------------------------------------
collect_test_summary <- function(res) {
  stores <- list(Polynomial = res$poly_all, `Elastic Net` = res$en_all, GAM = res$gam_all)
  rows <- list()
  for (sh in res$sheet_names) {
    for (m in names(stores)) {
      fin <- stores[[m]][[sh]]$final
      for (rt in c("AICc", "CV")) {
        o <- fin[[route_key(rt)]]
        rows[[length(rows) + 1]] <- data.frame(
          Sheet = sh, Model = m, Route = rt, Config = o$config,
          AICc = o$AICc, CV_RMSE = o$CV_RMSE,
          Test_RMSE = o$test_RMSE, Test_R2 = o$test_R2,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dplyr::bind_rows(rows)
}

tuning_counts <- function(res, sheet, model, route) {
  K <- length(res$folds)
  obj <- switch(model,
                "Polynomial" = res$poly_all[[sheet]],
                "Elastic Net" = res$en_all[[sheet]],
                "GAM" = res$gam_all[[sheet]])
  if (model == "Polynomial") {
    cand <- nrow(obj$table)
    fits <- if (route == "AICc") cand else cand * K
    note <- if (route == "AICc") "1 fit penuh per derajat" else sprintf("%d derajat × %d fold", cand, K)
  } else if (model == "Elastic Net") {
    if (route == "AICc") {
      cand <- nrow(obj$table); fits <- cand
      note <- "grid λ × rasio-L1, 1 fit penuh per kombinasi"
    } else {
      cand <- nrow(obj$cv_grid); fits <- cand * K
      note <- sprintf("%d kombinasi × %d fold (cv.glmnet)", cand, K)
    }
  } else {
    if (route == "AICc") {
      cand <- 1; fits <- 1
      note <- "λ dioptimasi otomatis oleh GCV.Cp"
    } else {
      cand <- nrow(obj$table); fits <- cand * K
      note <- sprintf("%d λ × %d fold", cand, K)
    }
  }
  list(candidates = cand, fits = fits, note = note)
}

# ------------------------------------------------------------
# 4. Membangun ulang model terpilih
# ------------------------------------------------------------
pretty_term <- function(nm) {
  nm <- gsub("\\^1(?![0-9])", "", nm, perl = TRUE)
  nm <- gsub("\\^2", "²", nm)
  nm <- gsub("\\^3", "³", nm)
  gsub(":", "·", nm)
}

fmt_coef <- function(x) {
  format(signif(abs(x), 4), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

build_equation <- function(target, intercept, term_names, coefs, max_terms = 12) {
  keep <- which(abs(coefs) > 1e-12)
  ord <- keep[order(abs(coefs[keep]), decreasing = TRUE)]
  shown <- ord[seq_len(min(length(ord), max_terms))]
  lines <- sprintf("%s = %s%s", target, ifelse(intercept < 0, "−", ""), fmt_coef(intercept))
  for (i in shown) {
    lines <- c(lines, sprintf("     %s %s·%s",
                              ifelse(coefs[i] >= 0, "+", "−"),
                              fmt_coef(coefs[i]), term_names[i]))
  }
  if (length(ord) > length(shown)) {
    lines <- c(lines, sprintf("     + … (%d suku lain, lihat tabel koefisien)", length(ord) - length(shown)))
  }
  paste(lines, collapse = "\n")
}

to_original_units <- function(b0_s, beta_s, center, scale) {
  beta <- beta_s / scale
  list(b0 = b0_s - sum(beta_s * center / scale), beta = beta)
}

poly_fit_std <- function(Xtr, ytr, degree) {
  feats <- names(Xtr)
  sc <- fit_scaler(Xtr)
  zf <- function(X) {
    Z <- apply_scaler(X[, feats, drop = FALSE], sc)
    names(Z) <- paste0("z_", feats)
    Z
  }
  P <- make_poly_matrix(zf(Xtr), degree)
  fit <- fit_linear_matrix(P, ytr)
  co <- fit$coefficients
  co[is.na(co)] <- 0
  list(
    model = "Polynomial", degree = degree, features = feats, scaler = sc, coef = co,
    predict = function(Xn) as.numeric(cbind(1, as.matrix(make_poly_matrix(zf(Xn), degree))) %*% co)
  )
}

en_fit_build <- function(Xtr, ytr, alpha, lambda) {
  feats <- names(Xtr)
  sc <- fit_scaler(Xtr)
  Xs <- as.matrix(apply_scaler(Xtr, sc))
  fit <- glmnet::glmnet(Xs, ytr, alpha = alpha, lambda = lambda,
                        standardize = FALSE, intercept = TRUE)
  beta <- as.numeric(stats::coef(fit))
  names(beta) <- c("(Intercept)", feats)
  list(
    model = "Elastic Net", alpha = alpha, lambda = lambda, features = feats,
    scaler = sc, beta = beta,
    predict = function(Xn) {
      Z <- as.matrix(apply_scaler(Xn[, feats, drop = FALSE], sc))
      as.numeric(cbind(1, Z) %*% beta)
    }
  )
}

gam_fit_build <- function(Xtr, ytr, route, obj, k_spline = 20) {
  feats <- names(Xtr)
  if (isTRUE(obj$degraded)) {
    fit <- fit_linear_matrix(Xtr, ytr)
    co <- fit$coefficients; co[is.na(co)] <- 0
    return(list(model = "GAM", degraded = TRUE, features = feats, coef = co,
               predict = function(Xn) as.numeric(cbind(1, as.matrix(Xn[, feats, drop = FALSE])) %*% co)))
  }
  df <- Xtr
  df$.y_internal <- ytr
  if (route == "AICc") {
    sp <- if (!is.null(obj$gcv_sp)) obj$gcv_sp else as.numeric(obj$gcv_model$sp)
    m <- mgcv::gam(make_gam_formula(".y_internal", feats, k_spline),
                   data = df, method = "GCV.Cp", sp = sp)
    lam <- mean(sp)
  } else {
    lam <- obj$table$lambda[which.min(obj$table$CV_RMSE)]
    m <- fit_gam_fixed_lambda(df, ".y_internal", feats, lam, k_spline)
  }
  list(model = "GAM", features = feats, lambda = lam, gam = m,
       predict = function(Xn) as.numeric(stats::predict(m, newdata = Xn[, feats, drop = FALSE])))
}

build_predictor <- function(model, route, obj, Xtr, ytr, k_spline = 20) {
  key <- route_key(route)
  if (model == "Polynomial") {
    deg <- get_config_degree(obj$final[[key]]$config)
    poly_fit_std(Xtr, ytr, deg)
  } else if (model == "Elastic Net") {
    row <- if (route == "AICc") obj$table[which.min(obj$table$AICc), , drop = FALSE]
           else obj$cv_grid[which.min(obj$cv_grid$CV_RMSE), , drop = FALSE]
    en_fit_build(Xtr, ytr, alpha = row$l1_ratio, lambda = row$lambda)
  } else {
    gam_fit_build(Xtr, ytr, route, obj, k_spline)
  }
}

describe_predictor <- function(pr, target) {
  if (pr$model == "Polynomial") {
    co <- pr$coef
    nm <- pretty_term(names(co)[-1])
    tab <- data.frame(Suku = nm, Koefisien = unname(co[-1]), stringsAsFactors = FALSE)
    tab <- tab[order(abs(tab$Koefisien), decreasing = TRUE), ]
    eq_std <- build_equation(target, co[1], nm, unname(co[-1]), max_terms = 14)
    eq_orig <- NULL
    if (pr$degree == 1) {
      f <- sub("^z_(.*)\\^1$", "\\1", names(co)[-1])
      ix <- match(pr$features, f)
      beta_s <- unname(co[-1])[ix]
      o <- to_original_units(co[1], beta_s, pr$scaler$center[pr$features], pr$scaler$scale[pr$features])
      eq_orig <- build_equation(target, o$b0, pr$features, o$beta)
    }
    list(eq_std = eq_std, eq_orig = eq_orig, table = tab,
         note = if (pr$degree == 1) "Model linear: koefisien juga ditampilkan pada satuan asli."
                else "Variabel z_* adalah prediktor terstandardisasi (z-score). Ini menjaga stabilitas numerik suku berderajat tinggi.")
  } else if (pr$model == "Elastic Net") {
    b <- pr$beta
    tab <- data.frame(Suku = paste0("z_", names(b)[-1]), Koefisien = unname(b[-1]),
                      Status = ifelse(abs(b[-1]) > 1e-12, "aktif", "nol (dieliminasi)"),
                      stringsAsFactors = FALSE)
    o <- to_original_units(b[1], unname(b[-1]), pr$scaler$center, pr$scaler$scale)
    list(eq_std = build_equation(target, b[1], paste0("z_", names(b)[-1]), unname(b[-1])),
         eq_orig = build_equation(target, o$b0, names(b)[-1], o$beta),
         table = tab,
         note = sprintf("λ = %.5g, rasio L1 = %.2f. Koefisien yang diciutkan menjadi nol berarti variabel dieliminasi.",
                        pr$lambda, pr$alpha))
  } else {
    if (isTRUE(pr$degraded)) {
      co <- pr$coef
      nm <- pretty_term(names(co)[-1])
      tab <- data.frame(Suku = nm, Koefisien = unname(co[-1]), stringsAsFactors = FALSE)
      return(list(
        eq_std = build_equation(target, co[1], nm, unname(co[-1])), eq_orig = NULL, table = tab,
        note = paste("PERHATIAN: GAM untuk sheet ini gagal konvergen tepat waktu (kendala numerik),",
                     "sehingga sistem otomatis memakai regresi linear sebagai pengganti sementara.")))
    }
    st <- summary(pr$gam)$s.table
    tab <- data.frame(Smooth = rownames(st), EDF = st[, "edf"], Ref.df = st[, "Ref.df"],
                      F = st[, "F"], `p-value` = st[, "p-value"], check.names = FALSE,
                      stringsAsFactors = FALSE)
    intercept <- unname(stats::coef(pr$gam)[1])
    smooths <- paste(sprintf("s(%s)", pr$features), collapse = " + ")
    list(eq_std = sprintf("%s = %s%s + %s\n\ndengan s(·) = penalized regression spline, λ (smoothing) = %.4g",
                          target, ifelse(intercept < 0, "−", ""), fmt_coef(intercept), smooths, pr$lambda),
         eq_orig = NULL, table = tab,
         note = sprintf("Total EDF = %.2f. EDF mendekati 1 berarti hubungan hampir linear; EDF besar berarti sangat non-linear.",
                        sum(pr$gam$edf)))
  }
}

partial_grid <- function(Xtr, feature, n = 80) {
  rng <- range(Xtr[[feature]], na.rm = TRUE)
  g <- as.data.frame(lapply(Xtr, function(x) rep(mean(x, na.rm = TRUE), n)))
  g[[feature]] <- seq(rng[1], rng[2], length.out = n)
  g
}

partial_effects <- function(predictors, Xtr, features, n = 80) {
  dplyr::bind_rows(lapply(features, function(f) {
    g <- partial_grid(Xtr, f, n)
    dplyr::bind_rows(lapply(names(predictors), function(m) {
      data.frame(Feature = f, x = g[[f]], Model = m, yhat = predictors[[m]]$predict(g),
                 stringsAsFactors = FALSE)
    }))
  }))
}

# ------------------------------------------------------------
# 5. Simulasi interaktif 1-D
# ------------------------------------------------------------
SIM_FUNCTIONS <- c("Gelombang sinus", "Kuadratik (parabola)", "Eksponensial jenuh",
                   "Tren + gelombang", "Sigmoid (tangga halus)")

sim_truth <- function(type, x) {
  switch(type,
    "Gelombang sinus"        = 6 * sin(0.9 * x) + 0.4 * x,
    "Kuadratik (parabola)"   = 0.45 * (x - 5)^2 - 4,
    "Eksponensial jenuh"     = 10 * (1 - exp(-0.45 * x)),
    "Tren + gelombang"       = 1.2 * x + 3 * sin(1.8 * x),
    "Sigmoid (tangga halus)" = 10 / (1 + exp(-1.6 * (x - 5))),
    6 * sin(0.9 * x) + 0.4 * x)
}

sim_generate <- function(type, n, noise, seed, test_frac = 0.3) {
  set.seed(seed)
  x <- stats::runif(n, 0, 10)
  y <- sim_truth(type, x) + stats::rnorm(n, 0, noise)
  idx <- sample.int(n, size = round(n * (1 - test_frac)))
  grid <- data.frame(x = seq(0, 10, length.out = 300))
  list(train = data.frame(x = x[idx], y = y[idx]),
       test = data.frame(x = x[-idx], y = y[-idx]),
       grid = grid, truth = sim_truth(type, grid$x), type = type)
}

fit_poly1d <- function(train, degree) {
  degree <- max(1L, min(as.integer(degree), length(unique(train$x)) - 2L))
  m <- stats::lm(stats::as.formula(sprintf("y ~ poly(x, %d)", degree)), data = train)
  list(k = degree + 1, label = sprintf("Polynomial derajat %d", degree),
       predict = function(x) {
         p <- stats::predict(m, newdata = data.frame(x = x), se.fit = TRUE)
         data.frame(fit = as.numeric(p$fit), se = as.numeric(p$se.fit))
       })
}

fit_en1d <- function(train, degree, alpha, loglam) {
  degree <- max(2L, min(as.integer(degree), length(unique(train$x)) - 2L))
  B <- stats::poly(train$x, degree)
  strip <- function(M) { M2 <- as.matrix(M); attributes(M2) <- list(dim = dim(M)); M2 }
  lam <- 10^loglam * stats::sd(train$y)
  fit <- glmnet::glmnet(strip(B), train$y, alpha = alpha, lambda = lam,
                        standardize = TRUE, intercept = TRUE)
  k <- as.numeric(fit$df) + 1
  list(k = k,
       label = sprintf("Elastic Net (basis deg %d, α=%.2f)", degree, alpha),
       predict = function(x) {
         Bn <- strip(stats::predict(B, x))
         data.frame(fit = as.numeric(stats::predict(fit, newx = Bn, s = lam)), se = NA_real_)
       })
}

fit_gam1d <- function(train, k, auto, loglam) {
  k <- max(4L, min(as.integer(k), length(unique(train$x)) - 1L))
  f <- stats::as.formula(sprintf("y ~ s(x, k = %d, bs = 'cr')", k))
  m <- if (isTRUE(auto)) mgcv::gam(f, data = train, method = "GCV.Cp")
       else mgcv::gam(f, data = train, method = "GCV.Cp", sp = 10^loglam)
  list(k = sum(m$edf), sp = m$sp,
       label = sprintf("GAM (k=%d, λ=%.3g)", k, m$sp),
       predict = function(x) {
         p <- stats::predict(m, newdata = data.frame(x = x), se.fit = TRUE)
         data.frame(fit = as.numeric(p$fit), se = as.numeric(p$se.fit))
       })
}

sim_metrics <- function(d, fit) {
  ptr <- fit$predict(d$train$x)$fit
  pte <- fit$predict(d$test$x)$fit
  n <- nrow(d$train)
  rss <- sum((d$train$y - ptr)^2)
  list(AICc = compute_aicc(n, rss, fit$k),
       RMSE_train = rmse(d$train$y, ptr), RMSE_test = rmse(d$test$y, pte),
       R2_train = r2_score(d$train$y, ptr), R2_test = r2_score(d$test$y, pte),
       k = fit$k)
}

sim_cv <- function(train, fitter, K = 5, seed = 1) {
  set.seed(seed)
  id <- sample(rep_len(seq_len(K), nrow(train)))
  errs <- vapply(seq_len(K), function(i) {
    tr <- train[id != i, , drop = FALSE]
    va <- train[id == i, , drop = FALSE]
    tryCatch(rmse(va$y, fitter(tr)$predict(va$x)$fit), error = function(e) NA_real_)
  }, numeric(1))
  mean(errs, na.rm = TRUE)
}

sim_sweep <- function(d, values, make_fit) {
  out <- lapply(values, function(v) {
    tryCatch({
      f <- make_fit(d$train, v)
      m <- sim_metrics(d, f)
      data.frame(value = v, AICc = m$AICc, RMSE_test = m$RMSE_test)
    }, error = function(e) NULL)
  })
  out <- dplyr::bind_rows(out)
  out$AICc[!is.finite(out$AICc)] <- NA
  out
}

# ------------------------------------------------------------
# 6. Analisis otomatis dataset pengguna
# ------------------------------------------------------------
read_user_table <- function(path, ext, sheet = NULL, dec = ".") {
  ext <- tolower(ext)
  if (ext %in% c("csv", "txt", "tsv")) {
    first <- readLines(path, n = 1, warn = FALSE)
    seps <- c(",", ";", "\t")
    cnt <- vapply(seps, function(s) lengths(regmatches(first, gregexpr(s, first, fixed = TRUE))), integer(1))
    sep <- seps[which.max(cnt)]
    d <- utils::read.csv(path, sep = sep, dec = dec, check.names = FALSE,
                         stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  } else if (ext %in% c("xlsx", "xls")) {
    d <- as.data.frame(readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet))
  } else if (ext == "ods") {
    if (!requireNamespace("readODS", quietly = TRUE)) stop("Package readODS belum terpasang.")
    d <- as.data.frame(readODS::read_ods(path, sheet = if (is.null(sheet)) 1 else sheet))
  } else stop("Format tidak didukung. Gunakan CSV, XLSX, XLS, atau ODS.")
  names(d) <- trimws(names(d))
  d
}

example_dataset <- function(n = 900, seed = 7) {
  set.seed(seed)
  x1 <- stats::runif(n, 0, 10); x2 <- stats::runif(n, -3, 3)
  x3 <- stats::rnorm(n, 50, 10); x4 <- stats::runif(n, 0, 1)
  y <- 4 + 2 * sin(x1) + 0.8 * x2^2 + 0.15 * x3 + stats::rnorm(n, 0, 0.9)
  data.frame(waktu = x1, sensor_a = x2, suhu = x3, noise_var = x4, hasil = y)
}

validate_auto_inputs <- function(df, target, features) {
  if (is.null(target) || !nzchar(target)) return("Pilih variabel target.")
  if (length(features) < 1) return("Pilih minimal 1 prediktor.")
  if (length(features) > 8) return("Maksimal 8 prediktor agar analisis tetap cepat.")
  if (target %in% features) return("Target tidak boleh ikut menjadi prediktor.")
  d <- df[, c(target, features), drop = FALSE]
  isnum <- vapply(d, function(x) is.numeric(x) || all(is.na(suppressWarnings(as.numeric(x))) == is.na(x)), logical(1))
  if (!all(isnum)) return(paste("Kolom bukan numerik:", paste(names(d)[!isnum], collapse = ", ")))
  d[] <- lapply(d, function(x) suppressWarnings(as.numeric(x)))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 60) return(sprintf("Data lengkap hanya %d baris; minimal 60 baris.", nrow(d)))
  uniq <- vapply(d[, features, drop = FALSE], function(x) length(unique(x)), integer(1))
  if (any(uniq < 8)) return(paste("Prediktor dengan < 8 nilai unik tidak cocok untuk GAM/Polynomial:",
                                  paste(names(uniq)[uniq < 8], collapse = ", ")))
  if (stats::sd(d[[target]]) == 0) return("Target konstan (SD = 0).")
  NULL
}

run_polynomial_auto <- function(Xtr, ytr, Xte, yte, folds, max_degree = 3) {
  n <- length(ytr)
  sc <- fit_scaler(Xtr)
  Ztr <- apply_scaler(Xtr, sc)
  Zte <- apply_scaler(Xte, sc)
  rows <- list(); fold_scores <- list(); store <- list()

  for (degree in seq_len(max_degree)) {
    P <- as.matrix(make_poly_matrix(Ztr, degree))
    k <- ncol(P) + 1
    if (degree > 1 && k >= 0.5 * n) next
    fit <- fit_linear_matrix(P, ytr)
    co <- fit$coefficients; co[is.na(co)] <- 0
    rss <- sum((ytr - as.numeric(cbind(1, P) %*% co))^2)

    fold_rmse <- vapply(folds, function(val_idx) {
      tr_idx <- setdiff(seq_len(n), val_idx)
      f <- fit_linear_matrix(P[tr_idx, , drop = FALSE], ytr[tr_idx])
      c2 <- f$coefficients; c2[is.na(c2)] <- 0
      rmse(ytr[val_idx], as.numeric(cbind(1, P[val_idx, , drop = FALSE]) %*% c2))
    }, numeric(1))

    Pte <- as.matrix(make_poly_matrix(Zte, degree))
    store[[as.character(degree)]] <- as.numeric(cbind(1, Pte) %*% co)
    fold_scores[[as.character(degree)]] <- fold_rmse
    rows[[length(rows) + 1]] <- data.frame(
      degree = degree, k = k, AICc = compute_aicc(n, rss, k),
      CV_RMSE = sqrt(mean(fold_rmse^2)), row.names = NULL)
  }

  res <- dplyr::bind_rows(rows)
  d_aic <- res$degree[which.min(res$AICc)]
  d_cv <- res$degree[which.min(res$CV_RMSE)]
  ev <- function(route, degree) {
    row <- res[res$degree == degree, , drop = FALSE]
    pred <- store[[as.character(degree)]]
    list(config = paste0("degree=", degree),
         AICc = if (route == "AIC") row$AICc else NA_real_,
         CV_RMSE = if (route == "CV") row$CV_RMSE else NA_real_,
         test_RMSE = rmse(yte, pred), test_R2 = r2_score(yte, pred))
  }
  list(table = res, final = list(AIC = ev("AIC", d_aic), CV = ev("CV", d_cv)),
       cv_fold_rmse = fold_scores)
}

summarise_models <- function(models) {
  rows <- lapply(names(models), function(m) {
    o <- models[[m]]$final
    data.frame(
      Model = m,
      AICc = o$AIC$AICc, Config_AICc = o$AIC$config,
      Test_RMSE_AICc = o$AIC$test_RMSE, Test_R2_AICc = o$AIC$test_R2,
      CV_RMSE = o$CV$CV_RMSE, Config_CV = o$CV$config,
      Test_RMSE_CV = o$CV$test_RMSE, Test_R2_CV = o$CV$test_R2,
      stringsAsFactors = FALSE)
  })
  out <- dplyr::bind_rows(rows)
  out$Delta_AICc <- out$AICc - min(out$AICc, na.rm = TRUE)
  out$Rank_AICc <- rank(out$AICc, ties.method = "min")
  out$Rank_CV <- rank(out$CV_RMSE, ties.method = "min")
  out
}

auto_analysis <- function(df, target, features, K = 10, test_size = 0.2, seed = 42,
                          max_rows = 5000, on_step = function(msg, frac) NULL) {
  d <- df[, c(target, features), drop = FALSE]
  d[] <- lapply(d, function(x) suppressWarnings(as.numeric(x)))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n_raw <- nrow(d)
  if (nrow(d) > max_rows) {
    set.seed(seed)
    d <- d[sample.int(nrow(d), max_rows), , drop = FALSE]
  }
  orig <- names(d)
  safe <- make.names(orig, unique = TRUE)
  names(d) <- safe
  name_map <- stats::setNames(orig, safe)
  tg <- safe[1]; ft <- safe[-1]

  n <- nrow(d)
  sp <- make_train_test_split(n, test_size, seed)
  folds <- make_folds(length(sp$train), K, seed)
  Xtr <- d[sp$train, ft, drop = FALSE]; ytr <- d[sp$train, tg]
  Xte <- d[sp$test, ft, drop = FALSE];  yte <- d[sp$test, tg]

  n_tr <- length(ytr); p <- length(ft)
  k_spline <- max(4, min(20, floor(n_tr / (3 * p)),
                         min(vapply(Xtr, function(x) length(unique(x)), integer(1))) - 1))

  safe_run <- function(expr) tryCatch(expr, error = function(e) structure(list(error = conditionMessage(e)), class = "model_error"))

  on_step("Polynomial Regression", 0.05)
  poly <- safe_run(run_polynomial_auto(Xtr, ytr, Xte, yte, folds))
  on_step("Elastic Net", 0.30)
  en <- safe_run(run_elasticnet(Xtr, ytr, Xte, yte, folds,
                                lambda_grid = 10^seq(-4, 1, length.out = 20) * stats::sd(ytr),
                                seed = seed))
  on_step("GAM (butuh waktu paling lama)", 0.55)
  gam <- safe_run({
    g <- run_gam_protected(Xtr, ytr, Xte, yte, folds,
                           on_note = function(m) on_step(m, 0.6), k_spline = k_spline)
    g$gcv_sp <- if (!is.null(g$gcv_model)) as.numeric(g$gcv_model$sp) else NA_real_
    g$gcv_model <- NULL; g$cv_model <- NULL
    g
  })
  on_step("Menyusun hasil", 0.95)

  models <- list(Polynomial = poly, `Elastic Net` = en, GAM = gam)
  errors <- vapply(models, function(m) if (inherits(m, "model_error")) m$error else NA_character_, character(1))
  ok <- models[is.na(errors)]
  if (length(ok) == 0) stop("Semua model gagal dijalankan: ", paste(errors, collapse = " | "))

  list(models = ok, errors = errors[!is.na(errors)], summary = summarise_models(ok),
       features = ft, target = tg, name_map = name_map,
       Xtr = Xtr, ytr = ytr, Xte = Xte, yte = yte, folds = folds,
       n_used = n, n_raw = n_raw, n_train = n_tr, n_test = length(yte),
       K = K, k_spline = k_spline, seed = seed)
}
