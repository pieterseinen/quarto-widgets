# sunburst_dashboard.R — public API for the sunburstr package
#
# Workflow
# --------
# 1.  Call sunburst_init() once per dashboard instance. Pass your data,
#     hierarchy, filter configuration, and optional category/color settings.
#     The returned `sunburstr_ctx` object, when auto-printed in a Quarto
#     R chunk, embeds all JSON data, dependencies, and the boot script.
#
# 2.  Call individual component functions (sunburst_chart, sunburst_gauge,
#     sunburst_selectors, sunburst_header, sunburst_table, sunburst_plot)
#     in whichever chunks / locations you like.
#
# 3.  Use sunburst_dashboard() as a shortcut that assembles all six
#     components in the default three-column grid layout.
#
# Multiple independent dashboards on one page are supported: just call
# sunburst_init() with a different `id` for each one.

# ══════════════════════════════════════════════════════════════════════════
# Default categories (Dutch public-health style)
# ══════════════════════════════════════════════════════════════════════════
.default_categories <- list(
  list(name = "Geen data",          color = "#bdbdbd", min = NULL),
  list(name = "Ongunstig",          color = "#d73027", min = 0L),
  list(name = "Beetje ongunstiger", color = "#fc8d59", min = 20L),
  list(name = "Gemiddeld",          color = "#fee08b", min = 30L),
  list(name = "Beetje gunstiger",   color = "#91cf60", min = 50L),
  list(name = "Gunstig",            color = "#1a9850", min = 70L)
)

# ══════════════════════════════════════════════════════════════════════════
# sunburst_init
# ══════════════════════════════════════════════════════════════════════════

#' Initialise a sunburst dashboard context
#'
#' Call this **once** per dashboard instance. The returned object embeds the
#' config, wijk JSON data, hierarchy JSON, all JavaScript and CSS dependencies,
#' and a `DOMContentLoaded` boot script into the document when auto-printed.
#'
#' @param data A data frame with one row per (entity × indicator).
#' @param hierarchy A nested list describing the domain/theme/indicator tree.
#' @param filters A list of lists, each with `col` (column name) and `label`.
#'   Defines the filter levels (e.g., gemeente → wijk). Set to `list()` for
#'   no filter dropdowns.
#' @param hierarchy_cols A list of lists with `col` and `label` for each
#'   hierarchy level (typically: domain, theme, indicator).
#' @param score_col The column name in `data` containing the score/value.
#' @param comparison_cols A list of lists with `col` and `label` for reference
#'   lines in the comparison plot (e.g., gemeente_gemiddelde, totaal_gemiddelde).
#' @param categories A list of category definitions, each with `name`, `color`,
#'   and `min` (minimum score for this category; use `NULL` for "no data").
#'   If `NULL`, uses the default Dutch public-health categories.
#' @param default_selection A named list specifying default filter values to
#'   pre-select on load (e.g., `list(gemeente = "Amsterdam")`).
#' @param id HTML id prefix for all elements belonging to this dashboard.
#'
#' @return A `sunburstr_ctx` object. Auto-printing it in a Quarto chunk injects
#'   all necessary HTML/JS.
#' @export
sunburst_init <- function(
    data,
    hierarchy,
    filters           = list(
      list(col = "gemeente", label = "Gemeente"),
      list(col = "wijk",     label = "Wijk")
    ),
    hierarchy_cols    = list(
      list(col = "domain",    label = "Domein"),
      list(col = "theme",     label = "Thema"),
      list(col = "indicator", label = "Indicator")
    ),
    score_col         = "waarde",
    comparison_cols   = list(
      list(col = "gemeente_gemiddelde", label = "Gemeente"),
      list(col = "totaal_gemiddelde",   label = "Nederland")
    ),
    categories        = NULL,
    default_selection = NULL,
    id                = "sunburstr-1"
) {
  # Use default categories if not provided
  cats <- if (is.null(categories)) .default_categories else categories

  # Build config object
  config <- list(
    filters        = filters,
    hierarchyCols  = hierarchy_cols,
    scoreCol       = score_col,
    comparisonCols = comparison_cols,
    categories     = cats,
    defaultSelection = default_selection
  )

  # JSON serialization
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")
  wijk_json <- jsonlite::toJSON(data, dataframe = "rows", auto_unbox = TRUE, null = "null")
  hierarchy_json <- jsonlite::toJSON(hierarchy, auto_unbox = TRUE, pretty = FALSE, null = "null")

  # Build filterSelectors array for the boot script
  n_filters <- length(filters)
  filter_selectors_js <- if (n_filters > 0) {
    paste0("[", paste0('"#', id, '-filter-', seq_len(n_filters) - 1, '"', collapse = ", "), "]")
  } else {
    "[]"
  }

  # DOMContentLoaded boot script (double-quoted JS strings to avoid Quarto &#39; encoding)
  boot_script <- sprintf(
    paste0(
      'document.addEventListener("DOMContentLoaded", function() {',
      '  window.SunburstDashboard.mountSunburstDashboard({',
      '    configScriptId:    "%s-config",',
      '    hierarchyScriptId: "%s-hierarchy-data",',
      '    wijkScriptId:      "%s-wijk-data",',
      '    filterSelectors:   %s,',
      '    sunburstSelector:  "#%s-sunburst",',
      '    gaugeSelector:     "#%s-gauge",',
      '    headerSelector:    "#%s-detail-header",',
      '    tableSelector:     "#%s-table-output",',
      '    plotSelector:      "#%s-plot-output"',
      '  });',
      '});'
    ),
    id, id, id, filter_selectors_js, id, id, id, id, id
  )

  html <- htmltools::tagList(
    .sunburst_dependencies(),
    htmltools::tags$script(id = paste0(id, "-config"),         type = "application/json", htmltools::HTML(config_json)),
    htmltools::tags$script(id = paste0(id, "-hierarchy-data"), type = "application/json", htmltools::HTML(hierarchy_json)),
    htmltools::tags$script(id = paste0(id, "-wijk-data"),      type = "application/json", htmltools::HTML(wijk_json)),
    htmltools::tags$script(htmltools::HTML(boot_script))
  )

  structure(
    html,
    class          = c("sunburstr_ctx", class(html)),
    sunburstr_id   = id,
    sunburstr_config = config
  )
}


# ══════════════════════════════════════════════════════════════════════════
# Individual component functions
# ══════════════════════════════════════════════════════════════════════════

#' Filter dropdowns (0 / 1 / N levels based on config)
#'
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @param filter Optional: return only the select for a specific filter column
#'   (by column name). If `NULL`, returns all filter selects.
#' @return An `htmltools::tagList`.
#' @export
sunburst_selectors <- function(ctx, filter = NULL) {
  .check_ctx(ctx)
  id      <- attr(ctx, "sunburstr_id")
  config  <- attr(ctx, "sunburstr_config")
  filters <- config$filters

  if (is.null(filters) || length(filters) == 0) {
    return(htmltools::tagList())
  }

  if (!is.null(filter)) {
    # Return just one specific filter's select
    idx <- which(vapply(filters, `[[`, "", "col") == filter)
    if (length(idx) == 0) stop("Filter '", filter, "' not found in config.")
    i <- idx[1] - 1L
    return(htmltools::tags$select(id = paste0(id, "-filter-", i)))
  }

  # Return all filter selects
  htmltools::tagList(
    lapply(seq_along(filters), function(i) {
      htmltools::tags$select(id = paste0(id, "-filter-", i - 1L))
    })
  )
}

#' Sunburst ring chart container
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_chart <- function(ctx) {
  .check_ctx(ctx)
  htmltools::div(id = paste0(attr(ctx, "sunburstr_id"), "-sunburst"))
}

#' Category colour gauge container
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_gauge <- function(ctx) {
  .check_ctx(ctx)
  htmltools::div(id = paste0(attr(ctx, "sunburstr_id"), "-gauge"))
}

#' Detail header
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_header <- function(ctx) {
  .check_ctx(ctx)
  htmltools::div(id = paste0(attr(ctx, "sunburstr_id"), "-detail-header"))
}

#' Detail indicator table
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_table <- function(ctx) {
  .check_ctx(ctx)
  htmltools::div(id = paste0(attr(ctx, "sunburstr_id"), "-table-output"))
}

#' Plotly comparison plot
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_plot <- function(ctx) {
  .check_ctx(ctx)
  htmltools::div(id = paste0(attr(ctx, "sunburstr_id"), "-plot-output"))
}


#' Full three-column sunburst dashboard layout
#'
#' Convenience function that assembles all six components in the default
#' three-column CSS grid layout.
#'
#' @param ctx A `sunburstr_ctx` returned by `sunburst_init()`.
#' @return An `htmltools::tag`.
#' @export
sunburst_dashboard <- function(ctx) {
  .check_ctx(ctx)
  config  <- attr(ctx, "sunburstr_config")
  filters <- config$filters

  htmltools::div(
    class = "dashboard-layout",
    htmltools::div(
      class = "left-panel",
      if (length(filters) > 0) htmltools::tags$h3(filters[[1]]$label %||% "Selectie"),
      sunburst_selectors(ctx)
    ),
    htmltools::div(
      class = "sunburst-panel",
      sunburst_chart(ctx),
      sunburst_gauge(ctx)
    ),
    htmltools::div(
      class = "detail-panel",
      sunburst_header(ctx),
      sunburst_table(ctx),
      sunburst_plot(ctx)
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════
# Internal helpers
# ══════════════════════════════════════════════════════════════════════════

`%||%` <- function(a, b) if (is.null(a)) b else a

.check_ctx <- function(ctx) {
  if (!inherits(ctx, "sunburstr_ctx")) {
    stop(
      "Expected a sunburstr_ctx object. ",
      "Did you forget to call sunburst_init() first?",
      call. = FALSE
    )
  }
}

.sunburst_dependencies <- function() {
  # singleton() deduplicates within one renderTags() call.
  # For multi-dashboard documents, print all contexts together in one chunk:
  #   htmltools::tagList(ctx_a, ctx_b, ctx_c)
  # This fires a single renderTags() pass and deduplicates all CDN scripts.
  htmltools::tagList(
    htmltools::singleton(htmltools::tags$script(src = "https://d3js.org/d3.v7.min.js")),
    htmltools::singleton(htmltools::tags$script(src = "https://code.jquery.com/jquery-3.7.1.min.js")),
    htmltools::singleton(htmltools::tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/2.0.0/css/dataTables.dataTables.min.css")),
    htmltools::singleton(htmltools::tags$script(src = "https://cdn.datatables.net/2.0.0/js/dataTables.min.js")),
    htmltools::singleton(htmltools::tags$script(src = "https://cdn.plot.ly/plotly-2.35.2.min.js")),
    htmltools::singleton(htmltools::tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/tom-select/dist/css/tom-select.css")),
    htmltools::singleton(htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/tom-select/dist/js/tom-select.complete.min.js")),
    htmltools::htmlDependency(
      name       = "quartoWidgets",
      version    = "0.2.0",
      src        = system.file("www", package = "quartoWidgets"),
      stylesheet = "custom.css",
      script     = "sunburstr-bundle.js",
      all_files  = FALSE
    )
  )
}
