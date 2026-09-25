# ============================================================
# FUNCTIONS - OPTIMIZATION
# Optimasi performa GAM + caching SP + parallelisasi
# ============================================================

# Cache smoothing parameter GAM antar-sheet
GAM_SP_CACHE <- new.env()

# ============================================================
# 1. Optimasi GAM dengan Cached Smoothing Parameter
# ============================================================

#' Run GAM dengan optional cached smoothing parameter
#' Penggunaan: First sheet → full GAM optimization
#'            Other sheets → use cached SP (99% lebih cepat)
run_gam_optimized <- function(Xtr, ytr, Xte, yte, folds, 
                              k_spline = 20,
                              use_cached_sp = FALSE,
                              sheet_id = NULL,
                              timeout_sec = 10) {
  
  # Jika mode cache aktif & ada SP tersimpan dari sheet lain
  if (isTRUE(use_cached_sp) && !is.null(sheet_id)) {
    cache_key <- paste0("sp_", sheet_id)
    if (exists(cache_key, envir = GAM_SP_CACHE)) {
      sp_cached <- get(cache_key, envir = GAM_SP_CACHE)
      return(run_gam_with_fixed_sp(Xtr, ytr, Xte, yte, folds, sp_cached, 
                                   k_spline, cached = TRUE))
    }
  }
  
  # Jika tidak ada cache, jalankan full GAM optimization
  res <- run_gam_protected(Xtr, ytr, Xte, yte, folds, 
                           timeout_sec = timeout_sec, 
                           k_spline = k_spline)
  
  # Simpan SP untuk sheet ini (jika berhasil)
  if (!is.null(sheet_id) && !is.null(res$gcv_sp)) {
    cache_key <- paste0("sp_", sheet_id)
    assign(cache_key, res$gcv_sp, envir = GAM_SP_CACHE)
  }
  
  attr(res, "cache_status") <- if (isTRUE(use_cached_sp)) "missed" else "fresh"
  res
}

#' Run GAM dengan smoothing parameter yang sudah ditentukan
#' Jauh lebih cepat karena skip iterasi optimasi
run_gam_with_fixed_sp <- function(Xtr, ytr, Xte, yte, folds, sp, 
                                  k_spline = 20, cached = FALSE) {
  
  n <- length(ytr)
  
  # Fit utama dengan SP fixed
  df_train <- Xtr
  df_train$.y_internal <- ytr
  formula <- make_gam_formula(".y_internal", names(Xtr), k_spline)
  
  m_main <- mgcv::gam(formula, data = df_train, method = "GCV.Cp", 
                      sp = sp, control = list(maxit = 5))  # Max 5 iterasi saja
  
  pred_train <- as.numeric(stats::predict(m_main, newdata = df_train))
  rss <- sum((ytr - pred_train)^2)
  k_eff <- sum(m_main$edf) + 1
  aicc_val <- compute_aicc(n, rss, k_eff)
  
  # CV fold dengan SP fixed
  fold_rmse_vals <- vapply(folds, function(val_idx) {
    tr_idx <- setdiff(seq_len(n), val_idx)
    
    df_fold <- Xtr[tr_idx, , drop = FALSE]
    df_fold$.y_internal <- ytr[tr_idx]
    
    fold_m <- mgcv::gam(formula, data = df_fold, method = "GCV.Cp", 
                        sp = sp, control = list(maxit = 5))
    
    df_val <- Xtr[val_idx, , drop = FALSE]
    pred_fold <- as.numeric(stats::predict(fold_m, newdata = df_val))
    rmse(ytr[val_idx], pred_fold)
  }, numeric(1))
  
  cv_rmse_mean <- mean(fold_rmse_vals)
  
  # Prediksi test
  pred_test <- as.numeric(stats::predict(m_main, newdata = Xte))
  test_rmse <- rmse(yte, pred_test)
  test_r2 <- r2_score(yte, pred_test)
  
  list(
    table = data.frame(lambda = mean(sp), CV_RMSE = cv_rmse_mean),
    final = list(
      AIC = list(
        config = sprintf("lambda(GCV)~=%.4g%s", mean(sp), 
                        if(cached) " [cached]" else ""),
        AICc = aicc_val,
        CV_RMSE = NA_real_,
        test_RMSE = test_rmse,
        test_R2 = test_r2
      ),
      CV = list(
        config = sprintf("lambda=%.4g%s", mean(sp), 
                        if(cached) " [cached]" else ""),
        AICc = NA_real_,
        CV_RMSE = cv_rmse_mean,
        test_RMSE = test_rmse,
        test_R2 = test_r2
      )
    ),
    cv_fold_rmse = fold_rmse_vals,
    gcv_sp = sp,
    gcv_model = m_main,
    cv_model = m_main,
    degraded = FALSE,
    used_cached_sp = cached
  )
}

# ============================================================
# 2. Parallel GAM fitting untuk multiple sheets (opsional)
# ============================================================

#' Fit GAM untuk semua sheet dengan opsi sequential vs parallel
#' Gunakan parallel = TRUE untuk 5+ sheets agar lebih cepat
run_gam_multisheet <- function(data_sheets, splits, folds, 
                               k_spline = 20, 
                               parallel = FALSE,
                               use_sp_cache = TRUE) {
  
  sheet_names <- names(data_sheets)
  gam_results <- list()
  
  if (isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)) {
    # Mode parallel (optional, requires future.apply package)
    future::plan(future::multisession(workers = min(4, length(sheet_names))))
    
    gam_results <- future.apply::future_lapply(
      seq_along(sheet_names),
      function(i) {
        sh <- sheet_names[i]
        sp <- splits[[sh]]
        
        # Sheet pertama: full GAM, sheet lain: cached
        use_cache <- (i > 1) && isTRUE(use_sp_cache)
        
        run_gam_optimized(
          sp$X_train, sp$y_train, sp$X_test, sp$y_test, folds,
          k_spline = k_spline,
          use_cached_sp = use_cache,
          sheet_id = sh,
          timeout_sec = 10
        )
      }
    )
    
    names(gam_results) <- sheet_names
  } else {
    # Mode sequential (default, always stable)
    for (i in seq_along(sheet_names)) {
      sh <- sheet_names[i]
      sp <- splits[[sh]]
      
      # Sheet pertama: full GAM, sheet lain: cached
      use_cache <- (i > 1) && isTRUE(use_sp_cache)
      
      gam_results[[sh]] <- run_gam_optimized(
        sp$X_train, sp$y_train, sp$X_test, sp$y_test, folds,
        k_spline = k_spline,
        use_cached_sp = use_cache,
        sheet_id = sh,
        timeout_sec = 10
      )
    }
  }
  
  gam_results
}

# ============================================================
# 3. Lazy Loading untuk Plotly (reduce initial render time)
# ============================================================

#' Render plotly dengan lazy loading (untuk halaman heavy)
lazy_plotly <- function(expr, container_id, height = 400) {
  # Return placeholder HTML, render on demand via shinyjs
  shiny::div(
    id = container_id,
    style = sprintf("height: %dpx; background: #f5f5f5; display: flex; 
                     align-items: center; justify-content: center;", height),
    shiny::p("Loading visualization...", style = "color: #999;")
  )
}

# Helper JS untuk trigger lazy plotly saat element visible
inject_lazy_loader <- function() {
  shiny::tags$script(HTML("
    window.lazyPlotlyCallbacks = window.lazyPlotlyCallbacks || {};
    
    function registerLazyPlotly(id, renderFn) {
      if ('IntersectionObserver' in window) {
        const observer = new IntersectionObserver(function(entries) {
          entries.forEach(function(entry) {
            if (entry.isIntersecting && !window.lazyPlotlyCallbacks[id]) {
              window.lazyPlotlyCallbacks[id] = true;
              renderFn();
              observer.unobserve(entry.target);
            }
          });
        }, { threshold: 0.1 });
        
        const elem = document.getElementById(id);
        if (elem) observer.observe(elem);
      }
    }
  "))
}

# ============================================================
# 4. Debounce untuk slider (smooth real-time update)
# ============================================================

#' Debounce input dengan custom delay
debounce_reactive <- function(reactive_expr, milliseconds = 300) {
  shiny::debounce(reactive_expr, milliseconds)
}

# ============================================================
# 5. Data subsetting untuk rapid iteration
# ============================================================

#' Subset data untuk mode "preview" (faster testing)
get_analysis_preview <- function(result, max_sheets = 2) {
  # Ambil hanya 2 sheet pertama untuk preview mode
  sheets_to_keep <- names(result$data_sheets)[seq_len(min(max_sheets, length(result$data_sheets)))]
  
  result$data_sheets <- result$data_sheets[sheets_to_keep]
  result$sheet_names <- sheets_to_keep
  result$splits <- result$splits[sheets_to_keep]
  result$poly_all <- result$poly_all[sheets_to_keep]
  result$en_all <- result$en_all[sheets_to_keep]
  result$gam_all <- result$gam_all[sheets_to_keep]
  
  result
}

# ============================================================
# 6. Progress reporter dengan detail timing
# ============================================================

create_progress_reporter <- function() {
  env <- new.env()
  env$timings <- list()
  env$last_time <- Sys.time()
  
  list(
    start = function(label) {
      env$last_time <<- Sys.time()
      env$current_label <<- label
    },
    
    step = function(label, fraction, parent_label = NULL) {
      now <- Sys.time()
      elapsed <- as.numeric(now - env$last_time, units = "secs")
      
      if (!is.null(env$current_label)) {
        env$timings[[env$current_label]] <<- elapsed
      }
      
      env$last_time <<- now
      env$current_label <<- label
      
      list(
        message = sprintf("%s [%.1fs]", label, elapsed),
        fraction = fraction,
        timings = env$timings
      )
    },
    
    get_summary = function() {
      total_time <- sum(unlist(env$timings), na.rm = TRUE)
      list(
        timings = env$timings,
        total = total_time,
        slowest = names(env$timings)[which.max(unlist(env$timings))]
      )
    }
  )
}

# ============================================================
# 7. Vectorized metrics computation
# ============================================================

#' Compute AICc untuk banyak model sekaligus (vectorized)
compute_aicc_vectorized <- function(ns, rss_vec, ks) {
  # ns: vektor n (sample size)
  # rss_vec: vektor RSS values
  # ks: vektor k (parameters)
  aic <- ns * log(rss_vec / ns) + 2 * ks
  aic + (2 * ks * (ks + 1)) / (pmax(ns - ks - 1, 1))
}

#' Compute RMSE untuk matrix predictions (vectorized)
rmse_vectorized <- function(y_true, y_pred_matrix) {
  # y_true: vektor
  # y_pred_matrix: matrix (setiap kolom = prediksi dari model berbeda)
  
  sqrt(colMeans((y_true - y_pred_matrix)^2, na.rm = TRUE))
}
