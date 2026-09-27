# Modern dashboard redesign

This branch contains the first implementation slice for the dashboard redesign. The existing five pages and their server-side functionality remain unchanged; the new visual system is additive and safe to adopt incrementally.

## Included in this slice

- `www/custom.css`: responsive visual system with modern cards, controls, metrics, equation panels, mobile layout, and optional dark appearance.
- `www/scripts.js`: remembers dark appearance and adds lightweight output-loading hooks without introducing a JavaScript framework.
- `R/modern_dashboard_helpers.R`: reusable UI helpers for later page-by-page extraction.

## Integration

The current `app.R` and `app_improved.R` already use `bslib`. To activate the stylesheet and script in either entry point, add the following inside the existing `page_navbar()` call (typically via `header`):

```r
header = tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$script(src = "scripts.js")
)
```

The next implementation slice can then move each existing page into a module while preserving the current input/output IDs and model functions.

## Compatibility

The redesign intentionally does not change `functions_regresi_ccpp.R`, `functions_dashboard.R`, or `functions_optimization.R`. Existing model selection, GAM caching, fallback behavior, simulation, upload, and five-page navigation remain the source of truth.
