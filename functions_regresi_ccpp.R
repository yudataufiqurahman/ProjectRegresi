# ============================================================
# FUNCTIONS - REGRESI CCPP
# Model Selection: AICc vs 10-Fold CV
# Model: Polynomial, Elastic Net, GAM
# ============================================================

required_packages <- c(
  "readxl", "readODS", "glmnet", "mgcv", "dplyr", "tidyr",
  "ggplot2", "broom", "car", "lmtest", "e1071", "purrr"
)

check_required_packages <- function() {
  missing_pkg <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkg) > 0) {
    stop(
      paste0(
        "Package berikut belum terpasang: ", paste(missing_pkg, collapse = ", "),
        ". Jalankan install.packages(c(",
        paste(sprintf("'%s'", missing_pkg), collapse = ", "), ")) terlebih dahulu."
      ),
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------
# 1. Membaca workbook multi-sheet
# ------------------------------------------------------------
read_ccpp_file <- function(path) {
  if (!file.exists(path)) {
    stop("File dataset tidak ditemukan: ", path, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    data_sheets <- stats::setNames(
      lapply(sheets, function(sh) readxl::read_excel(path, sheet = sh)),
      sheets
    )
  } else if (ext == "ods") {
    sheets <- readODS::ods_sheets(path)
    data_sheets <- stats::setNames(
      lapply(sheets, function(sh) readODS::read_ods(path, sheet = sh, as_tibble = TRUE)),
      sheets
    )
  } else {
    stop("Format file harus .xlsx, .xls, atau .ods.", call. = FALSE)
  }

  data_sheets <- lapply(data_sheets, function(d) {
    d <- as.data.frame(d, check.names = FALSE)
    names(d) <- trimws(names(d))
    d
  })

  data_sheets
}

validate_ccpp_data <- function(data_sheets,
                               target = "PE",
                               features = c("AT", "V", "AP", "RH")) {
  if (length(data_sheets) == 0) {
    stop("Tidak ada sheet yang terbaca.", call. = FALSE)
  }

  need <- c(features, target)
  missing_by_sheet <- lapply(data_sheets, function(d) setdiff(need, names(d)))
  missing_by_sheet <- missing_by_sheet[lengths(missing_by_sheet) > 0]

  if (length(missing_by_sheet) > 0) {
    msg <- paste(
      names(missing_by_sheet),
      vapply(missing_by_sheet, paste, character(1), collapse = ", "),
      sep = ": "
    )
    stop("Variabel tidak ditemukan -> ", paste(msg, collapse = " | "), call. = FALSE)
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# 2. EDA
# ------------------------------------------------------------
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

describe_numeric <- function(df) {
  numeric_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]

  out <- lapply(names(numeric_df), function(v) {
    x <- numeric_df[[v]]
    x <- x[is.finite(x)]
    data.frame(
      Variabel = v,
      N = length(x),
      Mean = mean(x),
      SD = stats::sd(x),
      Min = min(x),
      Median = stats::median(x),
      Mode = mode_value(x),
      Max = max(x),
      IQR = stats::IQR(x),
      Skewness = e1071::skewness(x, type = 2),
      Kurtosis = e1071::kurtosis(x, type = 2),
      row.names = NULL
    )
  })

  dplyr::bind_rows(out)
}

iqr_outlier_count <- function(x) {
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr, na.rm = TRUE)
}

verify_identical_sheets <- function(data_sheets,
                                     features = c("AT", "V", "AP", "RH"),
                                     target = "PE") {
  sheet_names <- names(data_sheets)
  ref <- data_sheets[[1]][order(
    data_sheets[[1]][[features[1]]],
    data_sheets[[1]][[features[2]]],
    data_sheets[[1]][[features[3]]],
    data_sheets[[1]][[features[4]]],
    data_sheets[[1]][[target]]
  ), c(features, target), drop = FALSE]
  rownames(ref) <- NULL

  rows <- lapply(sheet_names, function(sh) {
    d <- data_sheets[[sh]]
    d_sorted <- d[order(
      d[[features[1]]], d[[features[2]]], d[[features[3]]],
      d[[features[4]]], d[[target]]
    ), c(features, target), drop = FALSE]
    rownames(d_sorted) <- NULL

    data.frame(
      Sheet = sh,
      n_baris = nrow(d),
      identik_dgn_sheet1_setelah_sort = isTRUE(all.equal(ref, d_sorted, check.attributes = FALSE)),
      row.names = NULL
    )
  })

  dplyr::bind_rows(rows)
}

# ------------------------------------------------------------
# 3. Statistik dasar model
# ------------------------------------------------------------
compute_aic <- function(n, rss, k) {
  n * log(rss / n) + 2 * k
}

compute_aicc <- function(n, rss, k) {
  aic <- compute_aic(n, rss, k)
  if ((n - k - 1) > 0) {
    aic + (2 * k * (k + 1)) / (n - k - 1)
  } else {
    Inf
  }
}

rmse <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
}

r2_score <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2, na.rm = TRUE) /
    sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
}

# ------------------------------------------------------------
# 4. Train-test split dan 10-fold CV
# ------------------------------------------------------------
make_train_test_split <- function(n, test_size = 0.20, seed = 42) {
  set.seed(seed)
  idx <- sample(seq_len(n))
  n_test <- ceiling(n * test_size)

  list(
    train = idx[(n_test + 1):n],
    test = idx[seq_len(n_test)]
  )
}

make_folds <- function(n, K = 10, seed = 42) {
  set.seed(seed)
  shuffled <- sample(seq_len(n))
  fold_id <- rep(seq_len(K), length.out = n)
  split(shuffled, fold_id)
}

fold_list_to_id <- function(folds, n) {
  fold_id <- integer(n)
  for (i in seq_along(folds)) fold_id[folds[[i]]] <- i
  fold_id
}

# ------------------------------------------------------------
# 5. Polynomial features setara PolynomialFeatures sklearn
#    include_bias = FALSE, interaction features included.
# ------------------------------------------------------------
generate_exponents <- function(p, degree) {
  result <- list()

  generate_exact <- function(pos, remaining, prefix) {
    if (pos == p) {
      result[[length(result) + 1]] <<- c(prefix, remaining)
      return(invisible(NULL))
    }
    for (e in 0:remaining) {
      generate_exact(pos + 1, remaining - e, c(prefix, e))
    }
    invisible(NULL)
  }

  for (total in seq_len(degree)) {
    generate_exact(1, total, numeric(0))
  }

  do.call(rbind, result)
}

make_poly_matrix <- function(df, degree) {
  X <- as.matrix(df)
  storage.mode(X) <- "double"
  p <- ncol(X)
  exponents <- generate_exponents(p, degree)

  out <- lapply(seq_len(nrow(exponents)), function(j) {
    exponent <- exponents[j, ]
    value <- rep(1, nrow(X))
    for (v in seq_len(p)) value <- value * X[, v]^exponent[v]
    value
  })

  out <- as.data.frame(out, check.names = FALSE)
  names(out) <- apply(exponents, 1, function(e) {
    parts <- paste0(colnames(df), "^", e)
    parts <- parts[e > 0]
    paste(parts, collapse = ":")
  })
  out
}

fit_linear_matrix <- function(X, y) {
  stats::lm.fit(x = cbind(`(Intercept)` = 1, as.matrix(X)), y = y)
}

predict_linear_matrix <- function(fit, X) {
  as.numeric(cbind(`(Intercept)` = 1, as.matrix(X)) %*% fit$coefficients)
}

# ------------------------------------------------------------
# 6. Polynomial Regression
# ------------------------------------------------------------
run_polynomial <- function(Xtr, ytr, Xte, yte, folds) {
  n <- length(ytr)
  rows <- list()
  fold_scores <- list()

  for (degree in 1:3) {
    Xp <- make_poly_matrix(Xtr, degree)
    fit <- fit_linear_matrix(Xp, ytr)
    pred_train <- predict_linear_matrix(fit, Xp)
    rss <- sum((ytr - pred_train)^2)
    k <- ncol(Xp) + 1
    aicc <- compute_aicc(n, rss, k)

    fold_rmse <- numeric(length(folds))
    for (i in seq_along(folds)) {
      val_idx <- folds[[i]]
      tr_idx <- setdiff(seq_len(n), val_idx)

      Xp_tr <- make_poly_matrix(Xtr[tr_idx, , drop = FALSE], degree)
      Xp_val <- make_poly_matrix(Xtr[val_idx, , drop = FALSE], degree)
      fold_fit <- fit_linear_matrix(Xp_tr, ytr[tr_idx])
      pred <- predict_linear_matrix(fold_fit, Xp_val)
      fold_rmse[i] <- rmse(ytr[val_idx], pred)
    }

    fold_scores[[as.character(degree)]] <- fold_rmse

    rows[[length(rows) + 1]] <- data.frame(
      degree = degree,
      k = k,
      AICc = aicc,
      CV_RMSE = sqrt(mean(fold_rmse^2)),
      row.names = NULL
    )
  }

  res <- dplyr::bind_rows(rows)
  degree_aic <- res$degree[which.min(res$AICc)]
  degree_cv <- res$degree[which.min(res$CV_RMSE)]

  evaluate_route <- function(route, degree) {
    fit <- fit_linear_matrix(make_poly_matrix(Xtr, degree), ytr)
    pred_test <- predict_linear_matrix(fit, make_poly_matrix(Xte, degree))
    row <- res[res$degree == degree, , drop = FALSE]

    list(
      config = paste0("degree=", degree),
      AICc = if (route == "AIC") row$AICc else NA_real_,
      CV_RMSE = if (route == "CV") row$CV_RMSE else NA_real_,
      test_RMSE = rmse(yte, pred_test),
      test_R2 = r2_score(yte, pred_test)
    )
  }

  list(
    table = res,
    final = list(
      AIC = evaluate_route("AIC", degree_aic),
      CV = evaluate_route("CV", degree_cv)
    ),
    cv_fold_rmse = fold_scores
  )
}

# ------------------------------------------------------------
# 7. Standardisasi
# ------------------------------------------------------------
fit_scaler <- function(X) {
  center <- vapply(X, mean, numeric(1), na.rm = TRUE)
  scale_val <- vapply(X, stats::sd, numeric(1), na.rm = TRUE)
  scale_val[!is.finite(scale_val) | scale_val == 0] <- 1
  list(center = center, scale = scale_val)
}

apply_scaler <- function(X, scaler) {
  X <- as.matrix(X)
  X <- sweep(X, 2, scaler$center, "-")
  X <- sweep(X, 2, scaler$scale, "/")
  as.data.frame(X, check.names = FALSE)
}

# ------------------------------------------------------------
# 8. Elastic Net
# ------------------------------------------------------------
run_elasticnet <- function(Xtr, ytr, Xte, yte, folds,
                           lambda_grid = 10^seq(-4, 1, length.out = 20),
                           alpha_mix_grid = c(0.1, 0.5, 0.9, 1.0),
                           seed = 42) {
  scaler <- fit_scaler(Xtr)
  Xtr_s <- as.matrix(apply_scaler(Xtr, scaler))
  Xte_s <- as.matrix(apply_scaler(Xte, scaler))
  n <- nrow(Xtr_s)
  fold_id <- fold_list_to_id(folds, n)

  lambda_grid <- sort(lambda_grid, decreasing = TRUE)

  aic_rows <- list()
  for (alpha_mix in alpha_mix_grid) {
    for (lambda_val in lambda_grid) {
      fit <- glmnet::glmnet(
        x = Xtr_s,
        y = ytr,
        alpha = alpha_mix,
        lambda = lambda_val,
        standardize = FALSE,
        intercept = TRUE
      )

      beta <- as.numeric(stats::coef(fit))
      k <- sum(abs(beta[-1]) > 1e-12) + 1
      pred <- as.numeric(stats::predict(fit, newx = Xtr_s, s = lambda_val))
      rss <- sum((ytr - pred)^2)

      aic_rows[[length(aic_rows) + 1]] <- data.frame(
        l1_ratio = alpha_mix,
        lambda = lambda_val,
        k = k,
        AICc = compute_aicc(n, rss, k),
        row.names = NULL
      )
    }
  }

  en_aic <- dplyr::bind_rows(aic_rows)
  best_aicc <- en_aic[which.min(en_aic$AICc), , drop = FALSE]

  # CV route: satu objek cv.glmnet per l1-ratio dengan foldid yang sama
  cv_objects <- lapply(alpha_mix_grid, function(alpha_mix) {
    glmnet::cv.glmnet(
      x = Xtr_s,
      y = ytr,
      alpha = alpha_mix,
      lambda = lambda_grid,
      foldid = fold_id,
      standardize = FALSE,
      intercept = TRUE,
      type.measure = "mse",
      nfolds = length(folds),
      keep = FALSE
    )
  })

  cv_grid <- dplyr::bind_rows(lapply(seq_along(cv_objects), function(i) {
    obj <- cv_objects[[i]]
    data.frame(
      l1_ratio = alpha_mix_grid[i],
      lambda = obj$lambda,
      CV_RMSE = sqrt(obj$cvm),
      row.names = NULL
    )
  }))

  best_cv <- cv_grid[which.min(cv_grid$CV_RMSE), , drop = FALSE]

  # Fold RMSE untuk kombinasi terbaik
  cv_fold_rmse <- numeric(length(folds))
  for (i in seq_along(folds)) {
    val_idx <- folds[[i]]
    tr_idx <- setdiff(seq_len(n), val_idx)

    fit <- glmnet::glmnet(
      x = Xtr_s[tr_idx, , drop = FALSE],
      y = ytr[tr_idx],
      alpha = best_cv$l1_ratio,
      lambda = best_cv$lambda,
      standardize = FALSE,
      intercept = TRUE
    )
    pred <- as.numeric(stats::predict(fit, newx = Xtr_s[val_idx, , drop = FALSE], s = best_cv$lambda))
    cv_fold_rmse[i] <- rmse(ytr[val_idx], pred)
  }

  fit_aic <- glmnet::glmnet(
    x = Xtr_s,
    y = ytr,
    alpha = best_aicc$l1_ratio,
    lambda = best_aicc$lambda,
    standardize = FALSE,
    intercept = TRUE
  )
  pred_aic <- as.numeric(stats::predict(fit_aic, newx = Xte_s, s = best_aicc$lambda))

  fit_cv <- glmnet::glmnet(
    x = Xtr_s,
    y = ytr,
    alpha = best_cv$l1_ratio,
    lambda = best_cv$lambda,
    standardize = FALSE,
    intercept = TRUE
  )
  pred_cv <- as.numeric(stats::predict(fit_cv, newx = Xte_s, s = best_cv$lambda))

  list(
    table = en_aic,
    cv_grid = cv_grid,
    final = list(
      AIC = list(
        config = sprintf("lambda=%.4f, l1=%.1f", best_aicc$lambda, best_aicc$l1_ratio),
        AICc = best_aicc$AICc,
        CV_RMSE = NA_real_,
        test_RMSE = rmse(yte, pred_aic),
        test_R2 = r2_score(yte, pred_aic)
      ),
      CV = list(
        config = sprintf("lambda=%.4f, l1=%.1f", best_cv$lambda, best_cv$l1_ratio),
        AICc = NA_real_,
        CV_RMSE = mean(cv_fold_rmse),
        test_RMSE = rmse(yte, pred_cv),
        test_R2 = r2_score(yte, pred_cv)
      )
    ),
    cv_fold_rmse = cv_fold_rmse,
    scaler = scaler
  )
}

# ------------------------------------------------------------
# 9. GAM
# ------------------------------------------------------------
make_gam_formula <- function(target, features, k_spline = 20) {
  rhs <- paste(sprintf("s(%s, k = %d)", features, k_spline), collapse = " + ")
  stats::as.formula(paste(target, "~", rhs))
}

fit_gam_fixed_lambda <- function(data, target, features, lambda,
                                 k_spline = 20) {
  formula <- make_gam_formula(target, features, k_spline)
  mgcv::gam(
    formula = formula,
    data = data,
    method = "REML",
    sp = rep(lambda, length(features))
  )
}

run_gam <- function(Xtr, ytr, Xte, yte, folds,
                    lambda_grid = 10^seq(-3, 3, length.out = 10),
                    k_spline = 20) {
  train_df <- Xtr
  train_df$.y_internal <- ytr
  test_df <- Xte

  formula <- make_gam_formula(".y_internal", names(Xtr), k_spline)

  # Rute AIC: smoothing parameter ditentukan oleh GCV.Cp
  gam_gcv <- mgcv::gam(
    formula = formula,
    data = train_df,
    method = "GCV.Cp"
  )

  pred_train_gcv <- as.numeric(stats::predict(gam_gcv, newdata = train_df))
  rss <- sum((ytr - pred_train_gcv)^2)
  edof <- sum(gam_gcv$edf)
  aicc_gcv <- compute_aicc(length(ytr), rss, edof)

  lambda_gcv <- mean(gam_gcv$sp)

  # Rute CV: common lambda untuk seluruh smooth seperti desain notebook
  cv_rows <- list()
  fold_scores <- list()

  for (lambda_val in lambda_grid) {
    fold_rmse <- numeric(length(folds))

    for (i in seq_along(folds)) {
      val_idx <- folds[[i]]
      tr_idx <- setdiff(seq_len(nrow(Xtr)), val_idx)

      fold_train <- Xtr[tr_idx, , drop = FALSE]
      fold_train$.y_internal <- ytr[tr_idx]
      fold_val <- Xtr[val_idx, , drop = FALSE]

      fit <- fit_gam_fixed_lambda(
        data = fold_train,
        target = ".y_internal",
        features = names(Xtr),
        lambda = lambda_val,
        k_spline = k_spline
      )

      pred <- as.numeric(stats::predict(fit, newdata = fold_val))
      fold_rmse[i] <- rmse(ytr[val_idx], pred)
    }

    fold_scores[[as.character(lambda_val)]] <- fold_rmse
    cv_rows[[length(cv_rows) + 1]] <- data.frame(
      lambda = lambda_val,
      CV_RMSE = mean(fold_rmse),
      row.names = NULL
    )
  }

  cv_df <- dplyr::bind_rows(cv_rows)
  best_cv <- cv_df[which.min(cv_df$CV_RMSE), , drop = FALSE]
  best_cv_fold_rmse <- fold_scores[[as.character(best_cv$lambda)]]

  pred_test_gcv <- as.numeric(stats::predict(gam_gcv, newdata = test_df))

  gam_cv <- fit_gam_fixed_lambda(
    data = train_df,
    target = ".y_internal",
    features = names(Xtr),
    lambda = best_cv$lambda,
    k_spline = k_spline
  )
  pred_test_cv <- as.numeric(stats::predict(gam_cv, newdata = test_df))

  list(
    table = cv_df,
    final = list(
      AIC = list(
        config = sprintf("lambda(GCV)~=%.4g", lambda_gcv),
        AICc = aicc_gcv,
        CV_RMSE = NA_real_,
        test_RMSE = rmse(yte, pred_test_gcv),
        test_R2 = r2_score(yte, pred_test_gcv)
      ),
      CV = list(
        config = sprintf("lambda=%.4g", best_cv$lambda),
        AICc = NA_real_,
        CV_RMSE = best_cv$CV_RMSE,
        test_RMSE = rmse(yte, pred_test_cv),
        test_R2 = r2_score(yte, pred_test_cv)
      )
    ),
    cv_fold_rmse = best_cv_fold_rmse,
    gcv_model = gam_gcv,
    cv_model = gam_cv
  )
}

# ------------------------------------------------------------
# 10. Residual polynomial
# ------------------------------------------------------------
get_config_degree <- function(config) {
  as.integer(sub(".*=", "", config))
}

get_poly_residuals <- function(Xtr, ytr, degree) {
  Xp <- make_poly_matrix(Xtr, degree)
  fit <- fit_linear_matrix(Xp, ytr)
  yhat <- predict_linear_matrix(fit, Xp)
  as.numeric(ytr - yhat)
}

# ------------------------------------------------------------
# 11. Analisis utama: semua sheet
# ------------------------------------------------------------
run_full_analysis <- function(data_sheets,
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

  poly_all <- list()
  en_all <- list()
  gam_all <- list()

  for (sh in sheet_names) {
    sp <- splits[[sh]]

    poly_all[[sh]] <- run_polynomial(
      sp$X_train, sp$y_train,
      sp$X_test, sp$y_test,
      folds = folds
    )

    en_all[[sh]] <- run_elasticnet(
      sp$X_train, sp$y_train,
      sp$X_test, sp$y_test,
      folds = folds,
      seed = seed
    )

    gam_all[[sh]] <- run_gam(
      sp$X_train, sp$y_train,
      sp$X_test, sp$y_test,
      folds = folds
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

  list(
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
  )
}

# ------------------------------------------------------------
# 12. Data frame untuk plotting / dashboard
# ------------------------------------------------------------
cv_fold_long <- function(result, model_name) {
  per_sheet <- result$cv_fold_store[[model_name]]
  dplyr::bind_rows(lapply(names(per_sheet), function(sh) {
    data.frame(
      Sheet = sh,
      CV_Fold = seq_along(per_sheet[[sh]]),
      CV_Fold_RMSE = per_sheet[[sh]],
      row.names = NULL
    )
  }))
}

model_metric_long <- function(result) {
  df <- result$best_model_df
  dplyr::bind_rows(
    df |>
      dplyr::transmute(Sheet,
                       Model = "Polynomial",
                       AICc = AICc_Polynomial,
                       CV_RMSE = CV_RMSE_Polynomial),
    df |>
      dplyr::transmute(Sheet,
                       Model = "Elastic Net",
                       AICc = AICc_ElasticNet,
                       CV_RMSE = CV_RMSE_ElasticNet),
    df |>
      dplyr::transmute(Sheet,
                       Model = "GAM",
                       AICc = AICc_GAM,
                       CV_RMSE = CV_RMSE_GAM)
  )
}
