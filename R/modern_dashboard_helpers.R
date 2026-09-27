# Reusable helpers for the modern dashboard redesign.
# These helpers are intentionally additive: existing output IDs and calculations
# can be moved into modules without changing their behavior.

pr_card <- function(title, ..., class = NULL) {
  shiny::div(
    class = paste("pr-card", class),
    shiny::tags$div(class = "pr-card-title", title),
    ...
  )
}

pr_status_badge <- function(text, status = c("info", "success", "warning", "danger")) {
  status <- match.arg(status)
  shiny::tags$span(
    class = paste("badge", paste0("text-bg-", status)),
    text
  )
}

pr_metric_row <- function(...) {
  shiny::div(class = "row g-3 pr-metric-row", ...)
}

pr_require_result <- function(result, message = "Analisis sedang disiapkan…") {
  shiny::validate(shiny::need(!is.null(result), message))
  result
}
