# widget_data.R — Initialise a quartoWidgets interactive data object

#' Initialise a quartoWidgets interactive data object
#'
#' Prepares a data frame for use with quartoWidgets interactive components.
#' Call this function \strong{once} per widget set in a Quarto document.
#' The returned object, when printed in an R chunk, injects all required
#' JavaScript libraries, CSS, serialised data, and configuration into the
#' HTML output.
#'
#' The hierarchy tree and internal \code{key} column are built automatically
#' from \code{data} and \code{hierarchy_cols} — no separate objects are
#' required.
#'
#' @param data A data frame with one row per entity-indicator combination.
#' @param hierarchy_cols Column specification for the hierarchy levels. Accepts
#'   a named character vector \code{c(domain = "Domein", indicator = "Indicator")},
#'   an unnamed character vector \code{c("domain", "indicator")}, or the legacy
#'   list-of-lists format \code{list(list(col = "domain", label = "Domein"), ...)}.
#' @param filters Column specification for the cascading filter dropdowns.
#'   Accepts the same three formats as \code{hierarchy_cols}.
#' @param dimension_filters Column specification for independent (non-cascading)
#'   filter dimensions such as age group or year.  Accepts the same formats as
#'   \code{filters}.  Dimension filters are optional modifiers — they constrain
#'   the data but do not participate in the cascading filter hierarchy and are
#'   not required before the sunburst populates.  Use
#'   \code{\link{radio_selector}} to render a radio-button selector for a
#'   dimension.
#' @param score_col Score column specification (0–100 numeric). Either a single
#'   column name (e.g. \code{"waarde"}), which is used when all filters are set,
#'   or a named character vector mapping filter column names to score columns
#'   (e.g. \code{c(gemeente = "gemeente_gemiddelde", wijk = "waarde")}). When a
#'   named vector is supplied the sunburst populates as soon as the deepest
#'   filter that has a score column is selected.  This column drives the values
#'   shown in the detail table and comparison bar chart.
#' @param category_col Optional column specification for sunburst
#'   colour/category assignment.  Accepts the same formats as \code{score_col}
#'   (single string or named vector).  When supplied, the sunburst chart uses
#'   this column (instead of \code{score_col}) to determine the category colour
#'   of each indicator.  When \code{NULL} (default), \code{score_col} is used
#'   for both display values and category colours.
#' @param comparison_cols Column specification for reference value columns.
#'   Accepts the same three formats as \code{hierarchy_cols}.
#' @param categories A list of category threshold definitions. \code{NULL} uses
#'   the default six Dutch public-health categories.
#' @param default_selection A named list of filter values to pre-select on load,
#'   e.g. \code{list(gemeente = "Breda")}.
#' @param id A unique HTML id prefix for this widget set. Default \code{"widget-1"}.
#'
#' @return A \code{quarto_widget_data} object (subclass of \code{htmltools::tagList}).
#'   Print it in a Quarto chunk to inject scripts and data into the HTML output.
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(
#'   data            = df,
#'   hierarchy_cols  = c(domain = "Domain", indicator = "Indicator"),
#'   filters         = c(area = "Area"),
#'   score_col       = "score",
#'   id              = "demo"
#' )
#' wd
#' }
#'
#' @export
widget_data <- function(
    data,
    hierarchy_cols  = c(domain    = "Domein",
                        theme     = "Thema",
                        indicator = "Indicator"),
    filters         = c(gemeente = "Gemeente",
                        wijk     = "Wijk"),
    dimension_filters = NULL,
    score_col       = "waarde",
    category_col    = NULL,
    comparison_cols = c(gemeente_gemiddelde = "Gemeente",
                        totaal_gemiddelde   = "Nederland"),
    categories      = NULL,
    default_selection = NULL,
    id              = "widget-1"
) {
  cats <- if (is.null(categories)) .default_categories else categories

  # Normalise all column specs to list(list(col=..., label=...)) internally
  hierarchy_cols    <- .normalise_cols(hierarchy_cols)
  filters           <- .normalise_cols(filters)
  dimension_filters <- .normalise_cols(dimension_filters)
  comparison_cols   <- .normalise_cols(comparison_cols)

  # Auto-compute key column from hierarchy column values
  data <- .add_key_column(data, hierarchy_cols)

  # Auto-build the hierarchy tree from data
  hierarchy <- .build_hierarchy(data, hierarchy_cols)

  # Normalise score_col: a single unnamed string is mapped to the deepest
  # filter level for backward compatibility. A named vector is kept as-is.
  score_col_map <- score_col
  if (is.character(score_col) && length(score_col) == 1L &&
      (is.null(names(score_col)) || !nzchar(names(score_col)))) {
    if (length(filters) > 0) {
      deepest <- filters[[length(filters)]]$col
      score_col_map <- setNames(score_col, deepest)
    }
  }

  # Normalise category_col the same way as score_col.
  category_col_map <- NULL
  if (!is.null(category_col)) {
    category_col_map <- category_col
    if (is.character(category_col) && length(category_col) == 1L &&
        (is.null(names(category_col)) || !nzchar(names(category_col)))) {
      if (length(filters) > 0) {
        deepest <- filters[[length(filters)]]$col
        category_col_map <- setNames(category_col, deepest)
      }
    }
  }

  config <- list(
    id               = id,
    filters          = filters,
    dimensionFilters = dimension_filters,
    hierarchyCols    = hierarchy_cols,
    scoreCol         = as.list(score_col_map),
    categoryCol      = if (!is.null(category_col_map)) as.list(category_col_map) else NULL,
    comparisonCols   = comparison_cols,
    categories       = cats,
    defaultSelection = default_selection
  )

  config_json    <- jsonlite::toJSON(config,    auto_unbox = TRUE, null = "null")
  wijk_json      <- jsonlite::toJSON(data,      dataframe = "rows", auto_unbox = TRUE, null = "null")
  hierarchy_json <- jsonlite::toJSON(hierarchy, auto_unbox = TRUE, pretty = FALSE, null = "null")

  n_filters <- length(filters)
  filter_selectors_js <- if (n_filters > 0) {
    paste0("[", paste0('"#', id, '-filter-', seq_len(n_filters) - 1, '"', collapse = ", "), "]")
  } else {
    "[]"
  }

  boot_script <- .js_on_ready(paste0(
    "  window.QuartoWidgets.mountWidgets(",
    .js_object(
      configScriptId    = .as_js(paste0(id, "-config")),
      hierarchyScriptId = .as_js(paste0(id, "-hierarchy-data")),
      wijkScriptId      = .as_js(paste0(id, "-wijk-data")),
      filterSelectors   = filter_selectors_js,
      sunburstSelector  = .as_js(paste0("#", id, "-sunburst")),
      gaugeSelector     = .as_js(paste0("#", id, "-gauge")),
      headerSelector    = .as_js(paste0("#", id, "-detail-header")),
      tableSelector     = .as_js(paste0("#", id, "-table-output")),
      plotSelector      = .as_js(paste0("#", id, "-plot-output"))
    ),
    ");"
  ))

  html <- htmltools::tagList(
    .quarto_widgets_dependencies(),
    htmltools::tags$script(id = paste0(id, "-config"),         type = "application/json", htmltools::HTML(config_json)),
    htmltools::tags$script(id = paste0(id, "-hierarchy-data"), type = "application/json", htmltools::HTML(hierarchy_json)),
    htmltools::tags$script(id = paste0(id, "-wijk-data"),      type = "application/json", htmltools::HTML(wijk_json)),
    htmltools::tags$script(htmltools::HTML(boot_script))
  )

  structure(
    html,
    class = c("quarto_widget_data", "sunburstr_ctx", class(html)),
    widget_data_id     = id,
    widget_data_config = config,
    widget_data_df     = data,
    sunburstr_id       = id,
    sunburstr_config   = config
  )
}
