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

  # Build config object (id included so JS can register the dashboard globally)
  config <- list(
    id             = id,
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
# geo_prepare  — read any sf-supported geo file → GeoJSON for polygon selector
# ══════════════════════════════════════════════════════════════════════════

#' Prepare geographic data for use in a polygon selector
#'
#' Reads a shapefile, GeoJSON, GeoPackage, or any other format supported by
#' the \code{sf} package, optionally simplifies the geometry, and returns a
#' list ready to pass to \code{\link{sunburst_polygon_selector}}.
#'
#' @param path Path to the file (e.g. "wijken.shp", "wijken.geojson", "wijken.gpkg").
#'   For a directory-based format like shapefile, pass the \code{.shp} file.
#' @param name_col Column whose values will be matched against the dashboard
#'   filter column (e.g. the column containing wijk names so clicks can be
#'   correlated with rows in \code{wijk_indicatoren}).
#' @param extra_cols Additional attribute columns to retain in the GeoJSON
#'   (e.g. a parent-filter column like gemeente name so polygons can be
#'   filtered when a higher-level selector changes).
#' @param simplify_tol Simplification tolerance passed to
#'   \code{sf::st_simplify()} (in the units of the source CRS; set \code{NULL}
#'   to skip simplification). Smaller values retain more detail; larger values
#'   produce smaller files. Default 100 works well for gemeente/wijk level at
#'   national scale.
#' @return A list with \code{$geojson} (character, UTF-8 GeoJSON) and
#'   \code{$name_col}.
#' @export
geo_prepare <- function(
    path,
    name_col,
    extra_cols   = NULL,
    simplify_tol = 100
) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required for geo_prepare(). ",
         "Install with: install.packages('sf')", call. = FALSE)

  dat <- sf::st_read(path, quiet = TRUE)

  # Keep only requested columns
  keep <- unique(c(name_col, extra_cols))
  keep <- keep[keep %in% names(dat)]
  dat  <- dat[, keep, drop = FALSE]

  # Repair any invalid geometries
  dat <- sf::st_make_valid(dat)

  # Simplify
  if (!is.null(simplify_tol) && simplify_tol > 0) {
    dat <- sf::st_simplify(dat, dTolerance = simplify_tol, preserveTopology = TRUE)
  }

  # Reproject to WGS 84 for web maps
  dat <- sf::st_transform(dat, crs = 4326)

  # Convert to GeoJSON string
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(dat, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  geojson <- paste(readLines(tmp, warn = FALSE), collapse = "")

  list(geojson = geojson, name_col = name_col)
}


# ══════════════════════════════════════════════════════════════════════════
# sunburst_polygon_selector  — insert an interactive polygon map
# ══════════════════════════════════════════════════════════════════════════

#' Polygon selector widget
#'
#' Embeds an interactive SVG map whose polygons drive a dashboard filter level.
#' Clicking a polygon is equivalent to selecting a value in the corresponding
#' TomSelect dropdown.
#'
#' @param ctx A \code{sunburstr_ctx} returned by \code{\link{sunburst_init}}.
#' @param geo A \code{geo_data} list returned by \code{\link{geo_prepare}}.
#' @param filter The filter column name this selector controls (must match one
#'   of the \code{col} values in the \code{filters} argument of
#'   \code{sunburst_init()}). E.g. \code{"wijk"}.
#' @param parent_filter Optional. The filter column one level up whose current
#'   value is used to hide/show polygons — e.g. \code{"gemeente"} so that only
#'   polygons for the selected gemeente are displayed. The GeoJSON must contain
#'   a property whose values match the parent filter values in your data.
#' @param geo_name_prop The GeoJSON feature property name whose values
#'   correspond to the \code{filter} column in the dashboard data. Defaults to
#'   \code{geo$name_col} (set via \code{geo_prepare()}).
#' @param geo_parent_prop The GeoJSON feature property name whose values
#'   correspond to the \code{parent_filter} column. Defaults to
#'   \code{parent_filter}.
#' @param show_when_filter Optional filter column that must have a non-null
#'   selection before the polygon selector is shown.
#' @param layered If \code{TRUE}, the selector first renders grouped parent
#'   polygons and drills down to child polygons after a click.
#' @param zoom_to_visible If \code{TRUE}, automatically zoom the map to the
#'   currently visible polygons.
#' @param back_label Button label used to return from the drilled-down layer to
#'   the parent layer.
#' @return An \code{htmltools::tagList}.
#' @export
sunburst_polygon_selector <- function(
    ctx,
    geo,
    filter,
    parent_filter    = NULL,
    geo_name_prop    = NULL,
    geo_parent_prop  = NULL,
    show_when_filter = NULL,
    layered          = FALSE,
    zoom_to_visible  = TRUE,
    back_label       = "Terug naar hoger niveau"
) {
  .check_ctx(ctx)
  id      <- attr(ctx, "sunburstr_id")
  config  <- attr(ctx, "sunburstr_config")

  # Resolve filter index
  filter_cols <- vapply(config$filters, `[[`, "", "col")
  filter_idx  <- match(filter, filter_cols) - 1L
  if (is.na(filter_idx)) {
    stop("Filter '", filter, "' not found in sunburst_init() filters config.", call. = FALSE)
  }

  if (isTRUE(layered) && is.null(parent_filter)) {
    stop("layered = TRUE requires parent_filter to be set.", call. = FALSE)
  }

  geo_name_prop   <- geo_name_prop   %||% geo$name_col
  geo_parent_prop <- geo_parent_prop %||% parent_filter

  as_js <- function(x) {
    if (is.null(x)) "null" else as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }

  geo_script_id <- paste0(id, "-polygon-geo-",      filter_idx)
  div_id        <- paste0(id, "-polygon-selector-", filter_idx)

  boot <- sprintf(
    paste0(
      'document.addEventListener("DOMContentLoaded", function() {',
      '  var db = window.__quartoWidgets && window.__quartoWidgets["%s"];',
      '  if (db) db.addPolygonSelector({',
      '    containerSelector: "%s",',
      '    geoScriptId:       "%s",',
      '    filterLevel:       %d,',
      '    nameProp:          %s,',
      '    parentFilter:      %s,',
      '    parentProp:        %s,',
      '    showWhenFilter:    %s,',
      '    layered:           %s,',
      '    zoomToVisible:     %s,',
      '    backLabel:         %s',
      '  });',
      '});'
    ),
    id, paste0("#", div_id), geo_script_id, filter_idx,
    as_js(geo_name_prop),
    as_js(parent_filter),
    as_js(geo_parent_prop),
    as_js(show_when_filter),
    if (isTRUE(layered)) "true" else "false",
    if (isTRUE(zoom_to_visible)) "true" else "false",
    as_js(back_label)
  )

  htmltools::tagList(
    htmltools::tags$script(
      id   = geo_script_id,
      type = "application/json",
      htmltools::HTML(geo$geojson)
    ),
    htmltools::div(id = div_id, class = "polygon-selector"),
    htmltools::tags$script(htmltools::HTML(boot))
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
