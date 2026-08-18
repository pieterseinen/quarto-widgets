# sunburst_dashboard.R — public API for the quartoWidgets package

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
# Internal helpers
# ══════════════════════════════════════════════════════════════════════════

`%||%` <- function(a, b) if (is.null(a)) b else a

# Normalise a column spec to list(list(col=..., label=...))
# Accepts: named vector c(col="Label"), unnamed vector c("col"),
# or existing list-of-lists format.
# Uses lapply (not mapply) to produce an UNNAMED list so that
# jsonlite::toJSON serialises it as a JSON array, not an object.
.normalise_cols <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())

  # Already in list-of-lists format — return as-is
  if (is.list(x) && length(x) > 0 && is.list(x[[1]])) return(x)

  if (!is.character(x))
    stop("Column specification must be a named character vector or a list of lists.",
         call. = FALSE)

  nms <- names(x) %||% character(length(x))
  lapply(seq_along(x), function(i) {
    nm  <- nms[i]
    val <- x[[i]]
    col   <- if (nzchar(nm)) nm  else val
    label <- if (nzchar(nm)) val else gsub("_", " ", val)
    list(col = col, label = label)
  })
}

# Add (or overwrite) the key column: paste(hierarchy_col_values, sep="|")
.add_key_column <- function(data, hierarchy_cols) {
  col_names <- vapply(hierarchy_cols, `[[`, "", "col")
  data$key  <- do.call(
    paste,
    c(lapply(col_names, function(cn) as.character(data[[cn]])), list(sep = "|"))
  )
  data
}

# Build a hierarchy nested list from data and normalised hierarchy_cols
.build_hierarchy <- function(data, hierarchy_cols) {
  if (length(hierarchy_cols) == 0)
    return(list(name = "", key = "root", children = list()))

  .build_level <- function(subset, level, parent_key) {
    col  <- hierarchy_cols[[level]]$col
    vals <- sort(unique(as.character(subset[[col]])))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    lapply(vals, function(v) {
      key  <- if (nzchar(parent_key)) paste(parent_key, v, sep = "|") else v
      sub2 <- subset[as.character(subset[[col]]) == v, , drop = FALSE]
      if (level == length(hierarchy_cols)) {
        list(name = v, key = key, value = 1L)
      } else {
        list(name = v, key = key,
             children = .build_level(sub2, level + 1L, key))
      }
    })
  }

  list(name = "", key = "root", children = .build_level(data, 1L, ""))
}

.widget_id <- function(widget_data) {
  attr(widget_data, "widget_data_id") %||% attr(widget_data, "sunburstr_id")
}

.widget_config <- function(widget_data) {
  attr(widget_data, "widget_data_config") %||% attr(widget_data, "sunburstr_config")
}

.check_widget_data <- function(widget_data) {
  if (!inherits(widget_data, c("quarto_widget_data", "sunburstr_ctx"))) {
    stop(
      "Expected a quarto_widget_data object. ",
      "Did you forget to call widget_data() first?",
      call. = FALSE
    )
  }
}

.check_ctx <- .check_widget_data

.quarto_widgets_dependencies <- function() {
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
      version    = "0.4.0",
      src        = system.file("www", package = "quartoWidgets"),
      stylesheet = "custom.css",
      script     = "sunburstr-bundle.js",
      all_files  = FALSE
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════
# widget_data
# ══════════════════════════════════════════════════════════════════════════

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
#' @param score_col Name of the numeric score column (0–100). Default \code{"waarde"}.
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
    score_col       = "waarde",
    comparison_cols = c(gemeente_gemiddelde = "Gemeente",
                        totaal_gemiddelde   = "Nederland"),
    categories      = NULL,
    default_selection = NULL,
    id              = "widget-1"
) {
  cats <- if (is.null(categories)) .default_categories else categories

  # Normalise all column specs to list(list(col=..., label=...)) internally
  hierarchy_cols  <- .normalise_cols(hierarchy_cols)
  filters         <- .normalise_cols(filters)
  comparison_cols <- .normalise_cols(comparison_cols)

  # Auto-compute key column from hierarchy column values
  data <- .add_key_column(data, hierarchy_cols)

  # Auto-build the hierarchy tree from data
  hierarchy <- .build_hierarchy(data, hierarchy_cols)

  config <- list(
    id               = id,
    filters          = filters,
    hierarchyCols    = hierarchy_cols,
    scoreCol         = score_col,
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

  boot_script <- sprintf(
    paste0(
      'document.addEventListener("DOMContentLoaded", function() {',
      '  window.QuartoWidgets.mountWidgets({',
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
    sunburstr_id       = id,
    sunburstr_config   = config
  )
}

sunburst_init <- widget_data

# ══════════════════════════════════════════════════════════════════════════
# Individual component functions
# ══════════════════════════════════════════════════════════════════════════

#' Cascading filter dropdowns
#'
#' Renders TomSelect searchable dropdowns driven by the filter configuration
#' passed to \code{\link{widget_data}}.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param filter Optional column name string. When supplied, returns only the
#'   dropdown for that filter column.
#'
#' @return An \code{htmltools::tagList} of \code{<select>} elements, or a
#'   single \code{<select>} when \code{filter} is specified.
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_selectors(wd)
#' widget_selectors(wd, filter = "gemeente")
#' }
#'
#' @export
widget_selectors <- function(widget_data, filter = NULL) {
  .check_widget_data(widget_data)
  id      <- .widget_id(widget_data)
  config  <- .widget_config(widget_data)
  filters <- config$filters

  if (is.null(filters) || length(filters) == 0) return(htmltools::tagList())

  if (!is.null(filter)) {
    idx <- which(vapply(filters, `[[`, "", "col") == filter)
    if (length(idx) == 0) stop("Filter '", filter, "' not found in config.")
    return(htmltools::tags$select(id = paste0(id, "-filter-", idx[1] - 1L)))
  }

  htmltools::tagList(
    lapply(seq_along(filters), function(i)
      htmltools::tags$select(id = paste0(id, "-filter-", i - 1L)))
  )
}

sunburst_selectors <- widget_selectors

#' Sunburst ring chart container
#'
#' Places the \code{<div>} for the three-ring sunburst chart.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div id="<id>-sunburst">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' sunburst_chart(wd)
#' }
#'
#' @export
sunburst_chart <- function(widget_data) {
  .check_widget_data(widget_data)
  htmltools::div(id = paste0(.widget_id(widget_data), "-sunburst"))
}

#' Colour-coded category gauge
#'
#' Places the \code{<div>} for the horizontal category gauge.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div id="<id>-gauge">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_gauge(wd)
#' }
#'
#' @export
widget_gauge <- function(widget_data) {
  .check_widget_data(widget_data)
  htmltools::div(id = paste0(.widget_id(widget_data), "-gauge"))
}

sunburst_gauge <- widget_gauge

#' Detail header
#'
#' Places the \code{<div>} for the detail panel heading.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div id="<id>-detail-header">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_header(wd)
#' }
#'
#' @export
widget_header <- function(widget_data) {
  .check_widget_data(widget_data)
  htmltools::div(id = paste0(.widget_id(widget_data), "-detail-header"))
}

sunburst_header <- widget_header

#' Detail indicator table
#'
#' Places the \code{<div>} for the DataTables detail table.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div id="<id>-table-output">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_table(wd)
#' }
#'
#' @export
widget_table <- function(widget_data) {
  .check_widget_data(widget_data)
  htmltools::div(id = paste0(.widget_id(widget_data), "-table-output"))
}

sunburst_table <- widget_table

#' Comparison bar chart
#'
#' Places the \code{<div>} for the Plotly comparison bar chart.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div id="<id>-plot-output">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_plot(wd)
#' }
#'
#' @export
widget_plot <- function(widget_data) {
  .check_widget_data(widget_data)
  htmltools::div(id = paste0(.widget_id(widget_data), "-plot-output"))
}

sunburst_plot <- widget_plot

#' Default three-column widget layout
#'
#' Convenience wrapper assembling all components into a three-column CSS grid.
#' For custom layouts use the individual component functions directly.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @return An \code{htmltools::tag} (\code{<div class="dashboard-layout">}).
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_layout(wd)
#' }
#'
#' @export
widget_layout <- function(widget_data) {
  .check_widget_data(widget_data)
  config  <- .widget_config(widget_data)
  filters <- config$filters

  htmltools::div(
    class = "dashboard-layout",
    htmltools::div(
      class = "left-panel",
      if (length(filters) > 0) htmltools::tags$h3(filters[[1]]$label %||% "Selectie"),
      widget_selectors(widget_data)
    ),
    htmltools::div(
      class = "sunburst-panel",
      sunburst_chart(widget_data),
      widget_gauge(widget_data)
    ),
    htmltools::div(
      class = "detail-panel",
      widget_header(widget_data),
      widget_table(widget_data),
      widget_plot(widget_data)
    )
  )
}

sunburst_dashboard <- widget_layout

# ══════════════════════════════════════════════════════════════════════════
# geo_prepare
# ══════════════════════════════════════════════════════════════════════════

#' Prepare geographic data for a polygon selector
#'
#' Reads a spatial file (shapefile, GeoJSON, GeoPackage, etc.), optionally
#' simplifies geometries, reprojects to WGS 84, and returns a list ready to
#' pass to \code{\link{polygon_selector}}.
#'
#' When \code{dissolve_by} is provided, the function additionally creates a
#' dissolved (unioned) version of the geometries grouped by that column. This
#' is used by \code{\link{polygon_selector}} in layered mode to show clean
#' parent-level outlines without internal child boundaries.
#'
#' @param path Path to the spatial file (e.g. the \code{.shp} file).
#' @param name_col Attribute column whose values match the filter column values
#'   in the dashboard data.
#' @param extra_cols Additional attribute columns to retain (e.g. a parent
#'   column for layered filtering). Default \code{NULL}.
#' @param dissolve_by Optional column name to dissolve (union) geometries by.
#'   Produces a separate parent-level GeoJSON where all child geometries sharing
#'   the same value are merged into a single polygon per parent group. Typically
#'   this matches the \code{parent_filter} column in \code{\link{polygon_selector}}.
#'   Default \code{NULL} (no dissolve).
#' @param simplify_tol Simplification tolerance in CRS units (metres).
#'   \code{NULL} skips simplification. Default \code{100}.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{geojson}}{Child-level GeoJSON (UTF-8 string).}
#'     \item{\code{name_col}}{The value of \code{name_col}.}
#'     \item{\code{parent_geojson}}{Parent-level dissolved GeoJSON (only when
#'       \code{dissolve_by} is set; \code{NULL} otherwise).}
#'     \item{\code{parent_col}}{The value of \code{dissolve_by} (or \code{NULL}).}
#'   }
#'
#' @details Requires the \pkg{sf} package. When \code{dissolve_by} is set,
#'   \code{sf::st_union()} is used to merge geometries per group, producing
#'   clean outer boundaries without internal borders.
#'
#' @seealso \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' # Single level (wijken only)
#' wijk_geo <- geo_prepare("wijken.shp", name_col = "statnaam",
#'                          extra_cols = "gemeente", simplify_tol = 150)
#'
#' # With dissolved parent level (gemeenten from wijk shapefile)
#' wijk_geo <- geo_prepare("wijken.shp", name_col = "statnaam",
#'                          extra_cols = "gemeente",
#'                          dissolve_by = "gemeente",
#'                          simplify_tol = 150)
#' }
#'
#' @export
geo_prepare <- function(path, name_col, extra_cols = NULL, dissolve_by = NULL,
                        simplify_tol = 100) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required. Install with: install.packages('sf')", call. = FALSE)

  dat  <- sf::st_read(path, quiet = TRUE)
  keep <- unique(c(name_col, extra_cols, dissolve_by))
  keep <- keep[keep %in% names(dat)]
  dat  <- dat[, keep, drop = FALSE]
  dat  <- sf::st_make_valid(dat)

  if (!is.null(simplify_tol) && simplify_tol > 0)
    dat <- sf::st_simplify(dat, dTolerance = simplify_tol, preserveTopology = TRUE)

  dat <- sf::st_transform(dat, crs = 4326)

  # Helper to write sf object to GeoJSON string
  # NOTE: Do NOT use layer_options="RFC7946=YES" here. D3 v4+ geoPath with
  # geoMercator expects CLOCKWISE exterior rings (spherical right-hand rule).
  # RFC7946 forces counter-clockwise, which D3 renders as the polygon
  # complement (a filled rectangle). Winding correction is handled in JS
  # via _fixWinding() using d3.geoArea() detection.
  .to_geojson_string <- function(sf_obj) {
    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    sf::st_write(sf_obj, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    paste(readLines(tmp, warn = FALSE), collapse = "")
  }

  # Child-level GeoJSON
  child_geojson <- .to_geojson_string(dat)

  # Parent-level dissolved GeoJSON (if dissolve_by is set)
  parent_geojson <- NULL
  if (!is.null(dissolve_by) && dissolve_by %in% names(dat)) {
    # Temporarily disable S2 spherical geometry to ensure consistent winding
    # order from st_union() — S2 can produce inverted rings.
    s2_was_active <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(s2_was_active), add = TRUE)

    parent_sf <- do.call(rbind, lapply(split(dat, dat[[dissolve_by]]), function(grp) {
      merged <- sf::st_union(grp)
      row    <- sf::st_sf(
        setNames(data.frame(grp[[dissolve_by]][1L], stringsAsFactors = FALSE), dissolve_by),
        geometry = merged
      )
      row
    }))
    parent_sf <- sf::st_make_valid(parent_sf)
    parent_geojson <- .to_geojson_string(parent_sf)
  }

  list(
    geojson        = child_geojson,
    name_col       = name_col,
    parent_geojson = parent_geojson,
    parent_col     = dissolve_by
  )
}

# ══════════════════════════════════════════════════════════════════════════
# polygon_selector
# ══════════════════════════════════════════════════════════════════════════

#' Interactive polygon map selector
#'
#' Embeds an SVG map whose clickable polygons act as an alternative to the
#' TomSelect filter dropdowns.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param geo A geo list returned by \code{\link{geo_prepare}}.
#' @param filter The filter column name this selector controls.
#' @param parent_filter Optional parent filter column. Required when
#'   \code{layered = TRUE}.
#' @param geo_name_prop GeoJSON property matching \code{filter} column values.
#'   Defaults to \code{geo$name_col}.
#' @param geo_parent_prop GeoJSON property matching \code{parent_filter} column
#'   values. Defaults to \code{parent_filter}.
#' @param show_when_filter Optional filter column name; the map is hidden until
#'   a value is selected for that filter.
#' @param layered Logical. When \code{TRUE} the map shows parent polygons first;
#'   clicking one zooms to its children. Requires \code{parent_filter}.
#' @param default_level Character. Starting level when \code{layered = TRUE}:
#'   \code{"parent"} (default) shows the parent-level map first,
#'   \code{"child"} shows child polygons immediately.
#' @param zoom_to_visible Logical. Fit the map to visible polygons. Default \code{TRUE}.
#' @param back_label Back-navigation button label in layered mode.
#'   Default \code{"Terug naar hoger niveau"}.
#' @param selected_stroke_width Numeric. Stroke width of the selected polygon
#'   outline. The selected polygon is raised on top of its neighbors so its
#'   border is always fully visible. Default \code{2.5}.
#' @param colors Optional named list of hex color strings to customise the
#'   polygon map appearance. Supported keys: \code{fill} (default polygon fill),
#'   \code{stroke} (border color), \code{hover} (fill on mouse hover),
#'   \code{selected} (fill when selected), \code{empty} (fill for polygons
#'   without matching data). Any key not supplied uses the built-in default.
#' @param show_empty_geometries Logical. When \code{TRUE} (default), polygons
#'   without corresponding data rows are shown but greyed out and
#'   non-selectable. When \code{FALSE}, such polygons are hidden entirely.
#'
#' @return An \code{htmltools::tagList} with the embedded GeoJSON script,
#'   map container \code{<div>}, and boot script.
#'
#' @seealso \code{\link{geo_prepare}}, \code{\link{widget_data}}
#'
#' @examples
#' \dontrun{
#' wijk_geo <- geo_prepare("wijken.shp", name_col = "statnaam",
#'                          extra_cols = "gemeente")
#' wd <- widget_data(df, id = "demo")
#' wd
#' polygon_selector(wd, wijk_geo, filter = "wijk", parent_filter = "gemeente",
#'                  layered = TRUE)
#' }
#'
#' @export
polygon_selector <- function(
    widget_data,
    geo,
    filter,
    parent_filter    = NULL,
    geo_name_prop    = NULL,
    geo_parent_prop  = NULL,
    show_when_filter = NULL,
    layered          = FALSE,
    default_level    = "parent",
    zoom_to_visible  = TRUE,
    back_label       = "Terug naar hoger niveau",
    selected_stroke_width = 2.5,
    colors           = NULL,
    show_empty_geometries = TRUE
) {
  .check_widget_data(widget_data)
  id     <- .widget_id(widget_data)
  config <- .widget_config(widget_data)

  filter_cols <- vapply(config$filters, `[[`, "", "col")
  filter_idx  <- match(filter, filter_cols) - 1L
  if (is.na(filter_idx))
    stop("Filter '", filter, "' not found in widget_data() filters config.", call. = FALSE)

  if (isTRUE(layered) && is.null(parent_filter))
    stop("layered = TRUE requires parent_filter to be set.", call. = FALSE)

  geo_name_prop   <- geo_name_prop   %||% geo$name_col
  geo_parent_prop <- geo_parent_prop %||% parent_filter

  as_js <- function(x) {
    if (is.null(x)) "null"
    else as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }

  # Build colors JS object (only emit non-NULL overrides)
  colors_js <- if (!is.null(colors) && is.list(colors)) {
    parts <- vapply(names(colors), function(k) {
      paste0(k, ": ", as_js(colors[[k]]))
    }, "")
    paste0("{", paste(parts, collapse = ", "), "}")
  } else {
    "null"
  }

  geo_script_id <- paste0(id, "-polygon-geo-",      filter_idx)
  div_id        <- paste0(id, "-polygon-selector-", filter_idx)

  # Embed parent-level dissolved GeoJSON if available
  has_parent_geo <- !is.null(geo$parent_geojson)
  parent_geo_script_id <- paste0(id, "-polygon-parent-geo-", filter_idx)

  boot <- sprintf(
    paste0(
      'document.addEventListener("DOMContentLoaded", function() {',
      '  var db = window.__quartoWidgets && window.__quartoWidgets["%s"];',
      '  if (db) db.addPolygonSelector({',
      '    containerSelector:    "%s",',
      '    geoScriptId:          "%s",',
      '    parentGeoScriptId:    %s,',
      '    filterLevel:          %d,',
      '    nameProp:             %s,',
      '    parentFilter:         %s,',
      '    parentProp:           %s,',
      '    showWhenFilter:       %s,',
      '    layered:              %s,',
      '    defaultLevel:         %s,',
      '    zoomToVisible:        %s,',
      '    backLabel:            %s,',
      '    selectedStrokeWidth:  %s,',
      '    colors:               %s,',
      '    showEmptyGeometries:  %s',
      '  });',
      '});'
    ),
    id, paste0("#", div_id), geo_script_id,
    if (has_parent_geo) as_js(parent_geo_script_id) else "null",
    filter_idx,
    as_js(geo_name_prop), as_js(parent_filter), as_js(geo_parent_prop),
    as_js(show_when_filter),
    if (isTRUE(layered)) "true" else "false",
    as_js(default_level),
    if (isTRUE(zoom_to_visible)) "true" else "false",
    as_js(back_label),
    as.character(selected_stroke_width),
    colors_js,
    if (isTRUE(show_empty_geometries)) "true" else "false"
  )

  tags <- list(
    htmltools::tags$script(
      id = geo_script_id, type = "application/json",
      htmltools::HTML(geo$geojson)
    )
  )

  # Add parent GeoJSON script tag if available
  if (has_parent_geo) {
    tags <- c(tags, list(
      htmltools::tags$script(
        id = parent_geo_script_id, type = "application/json",
        htmltools::HTML(geo$parent_geojson)
      )
    ))
  }

  tags <- c(tags, list(
    htmltools::div(id = div_id, class = "polygon-selector"),
    htmltools::tags$script(htmltools::HTML(boot))
  ))

  do.call(htmltools::tagList, tags)
}

sunburst_polygon_selector <- polygon_selector
