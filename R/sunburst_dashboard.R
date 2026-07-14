# sunburst_dashboard.R — public API for the quartoWidgets package
#
# Workflow
# --------
# 1.  Call widget_data() once per interactive widget set. Pass your data
#     frame and column specifications. The hierarchy and key column are
#     built automatically — no separate objects required.
#
# 2.  Call individual component functions (sunburst_chart, widget_gauge,
#     widget_selectors, widget_header, widget_table, widget_plot)
#     in whichever chunks / locations you like.
#
# 3.  Use widget_layout() as a shortcut that assembles the default layout.
#
# Multiple independent widget sets on one page are supported: just call
# widget_data() with a different `id` for each one.

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
#'   Must contain columns named in \code{filters}, \code{hierarchy_cols},
#'   \code{score_col}, and \code{comparison_cols}. An internal \code{key}
#'   column is computed automatically; any existing \code{key} column in
#'   \code{data} is overwritten.
#' @param hierarchy_cols Column specification for the hierarchy levels shown
#'   in the sunburst chart (e.g. domain, theme, indicator from outermost to
#'   innermost ring). Accepts:
#'   \itemize{
#'     \item A \strong{named character vector} where names are column names
#'           and values are display labels:
#'           \code{c(domain = "Domein", theme = "Thema", indicator = "Indicator")}.
#'     \item An \strong{unnamed character vector} where values are column names
#'           and labels are derived automatically:
#'           \code{c("domain", "theme", "indicator")}.
#'     \item The legacy \strong{list-of-lists} format:
#'           \code{list(list(col = "domain", label = "Domein"), ...)}.
#'   }
#' @param filters Column specification for the cascading filter dropdowns
#'   (e.g. gemeente, then wijk). Accepts the same three formats as
#'   \code{hierarchy_cols}. Set to \code{character(0)} or \code{list()} for
#'   no dropdowns.
#' @param score_col Name of the numeric column containing the indicator score
#'   (expected range 0–100). Defaults to \code{"waarde"}.
#' @param comparison_cols Column specification for reference value columns
#'   shown as dashed lines in the comparison bar chart. Accepts the same
#'   three formats as \code{hierarchy_cols}. At most two reference lines are
#'   rendered. Set to \code{character(0)} or \code{list()} for none.
#' @param categories A list of category threshold definitions controlling the
#'   colour mapping of scores. Each element must be a list with \code{name}
#'   (character), \code{color} (CSS hex string), and \code{min} (numeric
#'   lower bound, or \code{NULL} for the "no data" category). If \code{NULL},
#'   defaults to six Dutch public-health categories ranging from
#'   \emph{Ongunstig} (0) to \emph{Gunstig} (70), plus \emph{Geen data}.
#' @param default_selection A named list of filter values to pre-select on
#'   document load. Names must match column names in \code{filters},
#'   e.g. \code{list(gemeente = "Breda")} pre-selects Breda in the first
#'   dropdown.
#' @param id A character string used as the HTML \code{id} prefix for all
#'   elements in this widget set. Must be unique per page. Defaults to
#'   \code{"widget-1"}.
#'
#' @return A \code{quarto_widget_data} object (a subclass of
#'   \code{htmltools::tagList}). Printing it in a Quarto chunk injects:
#'   \itemize{
#'     \item CDN dependencies: D3 v7, Plotly 2, DataTables 2, TomSelect.
#'     \item The quartoWidgets JavaScript bundle and CSS stylesheet.
#'     \item Three \code{<script type="application/json">} tags containing
#'           the serialised configuration, hierarchy, and data rows.
#'     \item A \code{DOMContentLoaded} boot script that mounts all widgets.
#'   }
#'
#' @details
#' \strong{Automatic hierarchy and key} \cr
#' \code{widget_data()} scans the unique value combinations of
#' \code{hierarchy_cols} in \code{data} and constructs the nested hierarchy
#' tree automatically. A \code{key} column is added to the serialised data
#' by concatenating the hierarchy column values with \code{"|"}, e.g.
#' \code{"domain|theme|indicator"}.  You do not need to pre-compute or supply
#' either of these.
#'
#' \strong{Column specification formats} \cr
#' All column specifications (\code{hierarchy_cols}, \code{filters},
#' \code{comparison_cols}) accept three equivalent formats:
#' \preformatted{
#'   # Named vector (recommended)
#'   filters = c(gemeente = "Gemeente", wijk = "Wijk")
#'
#'   # Unnamed vector (labels auto-derived from column names)
#'   filters = c("gemeente", "wijk")
#'
#'   # Legacy list-of-lists (still accepted)
#'   filters = list(list(col = "gemeente", label = "Gemeente"), ...)
#' }
#'
#' \strong{Printing the object} \cr
#' The \code{quarto_widget_data} object must be printed in a chunk without
#' \code{include = FALSE}. A common pattern is to initialise in a setup chunk
#' and print in a dedicated output chunk:
#' \preformatted{
#' ```{r setup, include = FALSE}
#' wd <- widget_data(df, ...)
#' ```
#'
#' ```{r embed-data}
#' wd
#' ```
#' }
#'
#' \strong{Multiple widget sets} \cr
#' Use different \code{id} values and print both objects together to
#' deduplicate CDN scripts:
#' \preformatted{
#'   wd1 <- widget_data(df1, id = "set-a")
#'   wd2 <- widget_data(df2, id = "set-b")
#'   htmltools::tagList(wd1, wd2)
#' }
#'
#' @seealso \code{\link{widget_selectors}}, \code{\link{sunburst_chart}},
#'   \code{\link{widget_gauge}}, \code{\link{widget_header}},
#'   \code{\link{widget_table}}, \code{\link{widget_plot}},
#'   \code{\link{widget_layout}}, \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' library(quartoWidgets)
#'
#' df <- data.frame(
#'   area      = rep(c("North", "South"), each = 2),
#'   domain    = "Health",
#'   indicator = c("Exercise", "Nutrition", "Exercise", "Nutrition"),
#'   score     = c(72, 45, 58, 80),
#'   ref       = 60,
#'   stringsAsFactors = FALSE
#' )
#'
#' wd <- widget_data(
#'   data            = df,
#'   hierarchy_cols  = c(domain = "Domain", indicator = "Indicator"),
#'   filters         = c(area = "Area"),
#'   score_col       = "score",
#'   comparison_cols = c(ref = "Reference"),
#'   id              = "demo"
#' )
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

  # Build config object (id included so JS can register the widget set globally)
  config <- list(
    id               = id,
    filters          = filters,
    hierarchyCols    = hierarchy_cols,
    scoreCol         = score_col,
    comparisonCols   = comparison_cols,
    categories       = cats,
    defaultSelection = default_selection
  )

  # JSON serialization
  config_json    <- jsonlite::toJSON(config,    auto_unbox = TRUE, null = "null")
  wijk_json      <- jsonlite::toJSON(data,      dataframe = "rows", auto_unbox = TRUE, null = "null")
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
#' Renders one or more TomSelect searchable dropdowns driven by the filter
#' configuration passed to \code{\link{widget_data}}. Selecting a value in an
#' upper-level dropdown automatically restricts the options in all lower-level
#' dropdowns (cascade filtering).
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#' @param filter Optional character string. When supplied, only the dropdown
#'   for the matching filter column is returned. Must equal one of the
#'   column names passed to \code{filters} in \code{\link{widget_data}}.
#'   When \code{NULL} (default), all dropdowns are returned as a
#'   \code{tagList}.
#'
#' @return An \code{htmltools::tagList} (all dropdowns) or a single
#'   \code{htmltools::tag} (\code{<select>}) when \code{filter} is specified.
#'
#' @details
#' Dropdowns are styled and enhanced by TomSelect, which is loaded
#' automatically as part of the quartoWidgets CDN dependencies when
#' \code{widget_data} is printed. The placeholder text is derived from the
#' display label of the corresponding filter column.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_selectors(wd)                     # all dropdowns
#' widget_selectors(wd, filter = "gemeente") # only the gemeente dropdown
#' }
#'
#' @export
widget_selectors <- function(widget_data, filter = NULL) {
  .check_widget_data(widget_data)
  id      <- .widget_id(widget_data)
  config  <- .widget_config(widget_data)
  filters <- config$filters

  if (is.null(filters) || length(filters) == 0) {
    return(htmltools::tagList())
  }

  if (!is.null(filter)) {
    idx <- which(vapply(filters, `[[`, "", "col") == filter)
    if (length(idx) == 0) stop("Filter '", filter, "' not found in config.")
    i <- idx[1] - 1L
    return(htmltools::tags$select(id = paste0(id, "-filter-", i)))
  }

  htmltools::tagList(
    lapply(seq_along(filters), function(i) {
      htmltools::tags$select(id = paste0(id, "-filter-", i - 1L))
    })
  )
}

sunburst_selectors <- widget_selectors

#' Sunburst ring chart
#'
#' Places the container \code{<div>} for the three-ring sunburst chart.
#' The chart visualises scores across the hierarchy levels defined in
#' \code{\link{widget_data}} and updates whenever the active filter selection
#' changes.
#'
#' Segments are coloured according to the \code{categories} thresholds.
#' Hovering a segment populates \code{\link{widget_gauge}},
#' \code{\link{widget_header}}, \code{\link{widget_table}}, and
#' \code{\link{widget_plot}}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-sunburst"}, populated by the JavaScript runtime on load.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_gauge}},
#'   \code{\link{widget_layout}}
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
#' Places the container \code{<div>} for the horizontal category gauge.
#' The gauge renders as a row of coloured segments — one per category
#' defined in \code{\link{widget_data}} — with a downward-pointing arrow
#' that animates to the segment matching the currently hovered sunburst node.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-gauge"}, rendered by the JavaScript runtime on load.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{sunburst_chart}},
#'   \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' sunburst_chart(wd)
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
#' Places the container \code{<div>} for the detail panel heading. When the
#' user hovers over a sunburst segment the header is populated with the name
#' of the currently selected entity (e.g. the gemeente and wijk names).
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-detail-header"}, managed by the JavaScript runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_table}},
#'   \code{\link{widget_plot}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_header(wd)
#' widget_table(wd)
#' widget_plot(wd)
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
#' Places the container \code{<div>} for the DataTables detail table.
#' When the user hovers over a sunburst segment, the table shows indicator
#' values for the selected entity, including any reference columns from
#' \code{comparison_cols}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-table-output"}, managed by the JavaScript runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_header}},
#'   \code{\link{widget_plot}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_header(wd)
#' widget_table(wd)
#' widget_plot(wd)
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
#' Places the container \code{<div>} for the Plotly comparison bar chart.
#' When the user hovers over a leaf-level sunburst segment, the chart
#' renders a bar for every entity in the parent filter group, highlighting
#' the selected entity. Dashed reference lines are drawn for each column
#' in \code{comparison_cols}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-plot-output"}, rendered by Plotly via the JavaScript runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_header}},
#'   \code{\link{widget_table}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_header(wd)
#' widget_table(wd)
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
#' Convenience wrapper that assembles all quartoWidgets components into a
#' three-column CSS grid: filter dropdowns on the left, the sunburst chart
#' and gauge in the centre, and the detail header, table, and comparison
#' plot on the right.
#'
#' For custom layouts use the individual component functions directly.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div class="dashboard-layout">})
#'   containing three child \code{<div>} elements styled by the quartoWidgets
#'   CSS stylesheet.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_selectors}},
#'   \code{\link{sunburst_chart}}, \code{\link{widget_gauge}},
#'   \code{\link{widget_header}}, \code{\link{widget_table}},
#'   \code{\link{widget_plot}}
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
# geo_prepare  — read any sf-supported geo file → GeoJSON for polygon selector
# ══════════════════════════════════════════════════════════════════════════

#' Prepare geographic data for a polygon selector
#'
#' Reads a spatial file in any format supported by the \pkg{sf} package
#' (shapefile, GeoJSON, GeoPackage, etc.), optionally simplifies the
#' geometries, reprojects to WGS 84 (EPSG:4326), and returns a list ready
#' to pass directly to \code{\link{polygon_selector}}.
#'
#' @param path Path to the spatial file. For directory-based formats such as
#'   shapefiles, pass the \code{.shp} file path.
#' @param name_col Name of the attribute column whose values are matched
#'   against the filter column in the dashboard data (e.g. \code{"statnaam"}
#'   for wijk names). Values must match those in the corresponding column of
#'   \code{data} passed to \code{\link{widget_data}}.
#' @param extra_cols Character vector of additional attribute columns to
#'   retain in the GeoJSON (e.g. \code{"gemeente"} for parent-level
#'   filtering). Defaults to \code{NULL}.
#' @param simplify_tol Simplification tolerance for \code{sf::st_simplify()}
#'   in CRS units (usually metres). Larger values produce smaller files with
#'   less boundary detail. Set \code{NULL} to skip. Defaults to \code{100}.
#'
#' @return A named list:
#'   \describe{
#'     \item{\code{geojson}}{Single-string UTF-8 GeoJSON ready to embed.}
#'     \item{\code{name_col}}{The value of the \code{name_col} argument,
#'       used by \code{\link{polygon_selector}} as the default GeoJSON
#'       property name.}
#'   }
#'
#' @details
#' Requires the \pkg{sf} package. Install with
#' \code{install.packages("sf")}. Invalid geometries are automatically
#' repaired with \code{sf::st_make_valid()} before simplification.
#'
#' @seealso \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' wijk_geo <- geo_prepare(
#'   path         = "wijken.shp",
#'   name_col     = "statnaam",
#'   extra_cols   = "gemeente",
#'   simplify_tol = 150
#' )
#' }
#'
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

  keep <- unique(c(name_col, extra_cols))
  keep <- keep[keep %in% names(dat)]
  dat  <- dat[, keep, drop = FALSE]

  dat <- sf::st_make_valid(dat)

  if (!is.null(simplify_tol) && simplify_tol > 0) {
    dat <- sf::st_simplify(dat, dTolerance = simplify_tol, preserveTopology = TRUE)
  }

  dat <- sf::st_transform(dat, crs = 4326)

  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(dat, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  geojson <- paste(readLines(tmp, warn = FALSE), collapse = "")

  list(geojson = geojson, name_col = name_col)
}


# ══════════════════════════════════════════════════════════════════════════
# polygon_selector  — insert an interactive polygon map
# ══════════════════════════════════════════════════════════════════════════

#' Interactive polygon map selector
#'
#' Embeds an SVG map whose clickable polygons act as an alternative to the
#' TomSelect filter dropdowns. Clicking a polygon selects that entity at the
#' specified filter level, updating the sunburst chart and detail panel
#' exactly as if the corresponding dropdown value had been chosen.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#' @param geo A geo data list returned by \code{\link{geo_prepare}}.
#' @param filter The filter column name this selector controls. Must match a
#'   column name in \code{filters} passed to \code{\link{widget_data}}.
#' @param parent_filter Optional. The filter column one level up whose
#'   selected value limits the visible polygons. Required when
#'   \code{layered = TRUE}.
#' @param geo_name_prop The GeoJSON feature property matching \code{filter}
#'   column values in the data. Defaults to \code{geo$name_col}.
#' @param geo_parent_prop The GeoJSON feature property matching
#'   \code{parent_filter} column values. Defaults to \code{parent_filter}.
#' @param show_when_filter Optional. A filter column name. The polygon
#'   selector is hidden until a non-empty value is selected for that filter.
#' @param layered Logical. When \code{TRUE} the selector first shows one
#'   polygon per parent value (built from grouped child geometries); clicking
#'   a parent polygon zooms in and switches to child polygons. A back button
#'   returns to the parent level. Requires \code{parent_filter}.
#'   Defaults to \code{FALSE}.
#' @param zoom_to_visible Logical. When \code{TRUE} (default) the map
#'   automatically fits to the currently visible polygons.
#' @param back_label Label for the back-navigation button in layered mode.
#'   Defaults to \code{"Terug naar hoger niveau"}.
#'
#' @return An \code{htmltools::tagList} containing the embedded GeoJSON
#'   script, the map container \code{<div>}, and a boot script.
#'
#' @details
#' \strong{Coordinate system} \cr
#' GeoJSON produced by \code{\link{geo_prepare}} is already in WGS 84.
#' Custom GeoJSON must also be in WGS 84 (EPSG:4326).
#'
#' \strong{Layered mode} \cr
#' Parent polygons are constructed client-side by grouping child features on
#' \code{geo_parent_prop}. Internal boundaries between child polygons remain
#' visible. For clean dissolved boundaries pre-process with
#' \code{sf::st_union()} before calling \code{\link{geo_prepare}}.
#'
#' @seealso \code{\link{geo_prepare}}, \code{\link{widget_data}},
#'   \code{\link{widget_selectors}}
#'
#' @examples
#' \dontrun{
#' wijk_geo <- geo_prepare("wijken.shp", name_col = "statnaam",
#'                          extra_cols = "gemeente")
#' wd <- widget_data(df, id = "demo")
#' wd
#'
#' # Non-layered
#' polygon_selector(wd, wijk_geo, filter = "wijk",
#'                  parent_filter = "gemeente")
#'
#' # Layered with conditional visibility
#' polygon_selector(wd, wijk_geo,
#'   filter           = "wijk",
#'   parent_filter    = "gemeente",
#'   show_when_filter = "gemeente",
#'   layered          = TRUE,
#'   back_label       = "Back to municipalities")
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
    zoom_to_visible  = TRUE,
    back_label       = "Terug naar hoger niveau"
) {
  .check_widget_data(widget_data)
  id      <- .widget_id(widget_data)
  config  <- .widget_config(widget_data)

  filter_cols <- vapply(config$filters, `[[`, "", "col")
  filter_idx  <- match(filter, filter_cols) - 1L
  if (is.na(filter_idx)) {
    stop("Filter '", filter, "' not found in widget_data() filters config.", call. = FALSE)
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

sunburst_polygon_selector <- polygon_selector

# ══════════════════════════════════════════════════════════════════════════
# Internal helpers
# ══════════════════════════════════════════════════════════════════════════

`%||%` <- function(a, b) if (is.null(a)) b else a

# Normalise a column spec to list(list(col=..., label=...))
# Accepts: named vector c(col="Label"), unnamed vector c("col"), or existing list format.
.normalise_cols <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())

  # Already in list-of-lists format
  if (is.list(x) && length(x) > 0 && is.list(x[[1]])) return(x)

  if (!is.character(x))
    stop("Column specification must be a named character vector or a list of lists.",
         call. = FALSE)

  nms <- names(x) %||% character(length(x))
  mapply(function(nm, val) {
    col   <- if (nzchar(nm)) nm  else val
    label <- if (nzchar(nm)) val else gsub("_", " ", val)
    list(col = col, label = label)
  }, nms, x, SIMPLIFY = FALSE)
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
  # singleton() deduplicates within one renderTags() call.
  # For multi-widget documents, print all widget_data objects together in one chunk:
  #   htmltools::tagList(wd_a, wd_b)
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
