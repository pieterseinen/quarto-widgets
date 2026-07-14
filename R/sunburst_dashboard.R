# sunburst_dashboard.R — public API for the quartoWidgets package
#
# Workflow
# --------
# 1.  Call widget_data() once per interactive widget set. Pass your data,
#     hierarchy, filter configuration, and optional category/color settings.
#     The returned `quarto_widget_data` object, when auto-printed in a Quarto
#     R chunk, embeds all JSON data, dependencies, and the boot script.
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
#' @param data A data frame with one row per entity-indicator combination.
#'   Required columns are determined by the values of \code{filters},
#'   \code{hierarchy_cols}, \code{score_col}, and \code{comparison_cols}.
#'   At minimum the data frame must contain:
#'   \itemize{
#'     \item One column per filter level (e.g. \code{gemeente} and
#'           \code{wijk}).
#'     \item One column per hierarchy level (e.g. \code{domain},
#'           \code{theme}, \code{indicator}).
#'     \item A pre-computed \code{key} column equal to the hierarchy column
#'           values joined by \code{"|"} (e.g. \code{"domain|theme|indicator"}).
#'           See Details.
#'     \item The score column specified by \code{score_col}.
#'     \item Optionally, one column per entry in \code{comparison_cols}.
#'   }
#' @param hierarchy A nested list defining the hierarchy tree displayed by
#'   the sunburst chart. The root node must have \code{key = "root"}.
#'   Each non-leaf node is a list with \code{name} (character label),
#'   \code{key} (character, matching the \code{key} column in \code{data}),
#'   and \code{children} (a list of child nodes). Leaf nodes additionally
#'   require \code{value = 1L} to produce equal arc widths.
#' @param filters A list of lists describing the cascading filter dropdowns.
#'   Each inner list must have:
#'   \itemize{
#'     \item \code{col} — column name in \code{data}.
#'     \item \code{label} — display label for the dropdown heading.
#'   }
#'   Selecting a value in filter \eqn{i} constrains available options in
#'   filter \eqn{i+1}. Set to \code{list()} for no dropdowns. Defaults to
#'   gemeente / wijk.
#' @param hierarchy_cols A list of lists identifying the hierarchy columns in
#'   \code{data}. Each inner list must have \code{col} and \code{label}.
#'   The last entry is used as the indicator label in the detail table and
#'   comparison plot. Defaults to domain / theme / indicator.
#' @param score_col Name of the numeric column in \code{data} containing the
#'   indicator score. Scores are expected in the range 0–100. Defaults to
#'   \code{"waarde"}.
#' @param comparison_cols A list of lists identifying reference value columns
#'   to display as dashed lines in the comparison plot. Each inner list must
#'   have \code{col} and \code{label}. At most two reference lines are
#'   rendered. Defaults to gemeente_gemiddelde and totaal_gemiddelde.
#' @param categories A list of category threshold definitions that controls
#'   the colour mapping of scores in the sunburst chart and gauge. Each
#'   element must be a list with:
#'   \itemize{
#'     \item \code{name} — display label for the category.
#'     \item \code{color} — CSS colour string (e.g. \code{"#d73027"}).
#'     \item \code{min} — minimum score (numeric) for this category, or
#'           \code{NULL} for the special "no data" category.
#'   }
#'   If \code{NULL}, defaults to six Dutch public-health categories:
#'   \emph{Geen data}, \emph{Ongunstig} (0), \emph{Beetje ongunstiger} (20),
#'   \emph{Gemiddeld} (30), \emph{Beetje gunstiger} (50),
#'   \emph{Gunstig} (70).
#' @param default_selection A named list of filter values to pre-select on
#'   document load. Names must match \code{col} values in \code{filters},
#'   e.g. \code{list(gemeente = "Amsterdam")} pre-selects Amsterdam in the
#'   first dropdown. If all filter levels are pre-selected the sunburst chart
#'   renders immediately without user interaction.
#' @param id A character string used as the HTML \code{id} prefix for all
#'   elements belonging to this widget set. Must be unique per page when
#'   multiple widget sets are embedded. Allowed characters: letters, digits,
#'   and hyphens. Defaults to \code{"widget-1"}.
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
#' \strong{The key column} \cr
#' The \code{key} column links data rows to hierarchy leaf nodes. It is
#' constructed by concatenating the hierarchy level values with \code{"|"}:
#' \preformatted{
#'   data$key <- paste(data$domain, data$theme, data$indicator, sep = "|")
#' }
#' The same pattern must be used when building the \code{hierarchy} list —
#' each node's \code{key} must equal the prefix up to that level.
#'
#' \strong{Printing the object} \cr
#' The \code{quarto_widget_data} object must be printed in a chunk that does
#' \emph{not} have \code{include = FALSE}. A common pattern is to initialise
#' in a setup chunk (suppressing output) and print in a dedicated chunk:
#' \preformatted{
#' ```{r setup, include = FALSE}
#' wd <- widget_data(df, hierarchy, id = "demo")
#' ```
#'
#' ```{r embed-data}
#' wd
#' ```
#' }
#'
#' \strong{Multiple widget sets} \cr
#' To place two independent widget sets on one page, use different \code{id}
#' values and print both objects together to deduplicate CDN scripts:
#' \preformatted{
#'   wd1 <- widget_data(df1, h1, id = "set-a")
#'   wd2 <- widget_data(df2, h2, id = "set-b")
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
#' # --- Minimal hierarchy (two levels: domain > indicator) ---
#' hierarchy <- list(
#'   name = "", key = "root",
#'   children = list(
#'     list(name = "Health", key = "Health",
#'       children = list(
#'         list(name = "Exercise", key = "Health|Exercise", value = 1L),
#'         list(name = "Nutrition", key = "Health|Nutrition", value = 1L)
#'       )
#'     )
#'   )
#' )
#'
#' # --- Matching data frame ---
#' df <- data.frame(
#'   area      = rep(c("North", "South"), each = 2),
#'   domain    = "Health",
#'   indicator = c("Exercise", "Nutrition", "Exercise", "Nutrition"),
#'   key       = c("Health|Exercise", "Health|Nutrition",
#'                 "Health|Exercise", "Health|Nutrition"),
#'   score     = c(72, 45, 58, 80),
#'   ref       = 60,
#'   stringsAsFactors = FALSE
#' )
#'
#' wd <- widget_data(
#'   data           = df,
#'   hierarchy      = hierarchy,
#'   filters        = list(list(col = "area", label = "Area")),
#'   hierarchy_cols = list(list(col = "domain",    label = "Domain"),
#'                         list(col = "indicator", label = "Indicator")),
#'   score_col      = "score",
#'   comparison_cols = list(list(col = "ref", label = "Reference")),
#'   id             = "demo"
#' )
#' }
#'
#' @export
widget_data <- function(
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
    id                = "widget-1"
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
    widget_data_id = id,
    widget_data_config = config,
    sunburstr_id = id,
    sunburstr_config = config
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
#'   \code{col} values passed to \code{filters} in \code{\link{widget_data}}.
#'   When \code{NULL} (default), all dropdowns are returned as a
#'   \code{tagList}.
#'
#' @return An \code{htmltools::tagList} (all dropdowns) or a single
#'   \code{htmltools::tag} (\code{<select>}) when \code{filter} is specified.
#'   Each \code{<select>} element has an \code{id} of the form
#'   \code{"<id>-filter-<n>"} where \code{<id>} is the widget set id and
#'   \code{<n>} is the zero-based filter index.
#'
#' @details
#' Dropdowns are styled and enhanced by TomSelect, which is loaded
#' automatically as part of the quartoWidgets CDN dependencies when
#' \code{widget_data} is printed. The placeholder text is derived from the
#' \code{label} field of the corresponding filter definition.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
#' wd  # embed in a Quarto chunk
#'
#' # All dropdowns
#' widget_selectors(wd)
#'
#' # Only the first-level dropdown
#' widget_selectors(wd, filter = "gemeente")
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

sunburst_selectors <- widget_selectors

#' Sunburst ring chart
#'
#' Places the container \code{<div>} for the three-ring sunburst chart.
#' The chart visualises scores across the hierarchy levels defined in
#' \code{\link{widget_data}} and updates whenever the active filter selection
#' changes.
#'
#' Segments are coloured according to the \code{categories} thresholds.
#' Hovering a segment displays a tooltip with the node name and score, moves
#' the pointer on \code{\link{widget_gauge}}, and populates
#' \code{\link{widget_header}}, \code{\link{widget_table}}, and
#' \code{\link{widget_plot}}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-sunburst"}. The div is populated by the JavaScript runtime
#'   when the document loads.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_gauge}},
#'   \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#' defined in \code{\link{widget_data}} — with a downward-pointing arrow that
#' animates to the segment matching the currently hovered sunburst node.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-gauge"}. The gauge is rendered by the JavaScript runtime
#'   when the document loads.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{sunburst_chart}},
#'   \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#' Places the container \code{<div>} for the detail panel header. When the
#' user hovers over a sunburst segment the header is populated with the name
#' of the currently selected entity derived from the active filter values
#' (e.g. the gemeente and wijk names).
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-detail-header"}. The content is managed by the JavaScript
#'   runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_table}},
#'   \code{\link{widget_plot}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#' Places the container \code{<div>} for the detail indicator table. When the
#' user hovers over a sunburst segment the table is populated with the
#' indicators belonging to the hovered node for the currently selected entity.
#' The table is rendered by DataTables and includes the score and any
#' reference values defined in \code{comparison_cols}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-table-output"}. Content is managed by the JavaScript runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_header}},
#'   \code{\link{widget_plot}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#' When the user hovers over a leaf-level sunburst segment (an individual
#' indicator) the chart renders a bar for every entity in the currently
#' active parent filter group, highlighting the selected entity. Dashed
#' reference lines are drawn for each column in \code{comparison_cols}.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#'
#' @return An \code{htmltools::tag} (\code{<div>}) with the id
#'   \code{"<id>-plot-output"}. The chart is rendered by Plotly via the
#'   JavaScript runtime.
#'
#' @seealso \code{\link{widget_data}}, \code{\link{widget_header}},
#'   \code{\link{widget_table}}, \code{\link{widget_layout}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#' and gauge in the centre, and the detail header, table, and comparison plot
#' on the right.
#'
#' For custom layouts — different column widths, additional text, or
#' components placed in separate Quarto columns or tabs — use the individual
#' component functions directly instead.
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
#' wd <- widget_data(df, hierarchy, id = "demo")
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
#'   shapefiles, pass the \code{.shp} file path. Accepts any format
#'   recognised by \code{sf::st_read()}.
#' @param name_col Name of the attribute column whose values are matched
#'   against the filter column in the dashboard data. For example, if the
#'   polygon selector controls the \code{"wijk"} filter, \code{name_col}
#'   should be the column containing wijk names (e.g. \code{"statnaam"}).
#'   Values in this column must match the corresponding values in
#'   \code{data} passed to \code{\link{widget_data}}.
#' @param extra_cols Character vector of additional attribute columns to
#'   retain in the output GeoJSON. Typically used to keep a parent-level
#'   column (e.g. \code{"gemeente"}) so that polygons can be filtered when
#'   a higher-level dropdown changes. Defaults to \code{NULL} (no extra
#'   columns).
#' @param simplify_tol Simplification tolerance passed to
#'   \code{sf::st_simplify()} in the units of the source CRS (usually metres
#'   for national projections). Larger values produce smaller, less detailed
#'   geometries. Set to \code{NULL} to skip simplification. Defaults to
#'   \code{100}, which works well for gemeente/wijk boundaries at national
#'   scale.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{geojson}}{A single-string UTF-8 GeoJSON
#'       \code{FeatureCollection} ready to embed in HTML.}
#'     \item{\code{name_col}}{The value of the \code{name_col} argument,
#'       used by \code{\link{polygon_selector}} as the default GeoJSON
#'       property name.}
#'   }
#'
#' @details
#' Requires the \pkg{sf} package. If \pkg{sf} is not installed an
#' informative error is raised. Install it with:
#' \preformatted{install.packages("sf")}
#'
#' Invalid geometries are automatically repaired with
#' \code{sf::st_make_valid()} before simplification.
#'
#' @seealso \code{\link{polygon_selector}}
#'
#' @examples
#' \dontrun{
#' wijk_geo <- geo_prepare(
#'   path         = "path/to/wijken.shp",
#'   name_col     = "statnaam",
#'   extra_cols   = "gemeente",
#'   simplify_tol = 150
#' )
#'
#' # wijk_geo$geojson  — the GeoJSON string
#' # wijk_geo$name_col — "statnaam"
#'
#' wd <- widget_data(df, hierarchy, id = "demo")
#' wd
#' polygon_selector(wd, geo = wijk_geo, filter = "wijk",
#'                  parent_filter = "gemeente")
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
# polygon_selector  — insert an interactive polygon map
# ══════════════════════════════════════════════════════════════════════════

#' Interactive polygon map selector
#'
#' Embeds an SVG map in the document whose clickable polygons act as an
#' alternative to the TomSelect filter dropdowns. Clicking a polygon selects
#' that entity at the specified filter level, updating the sunburst chart and
#' detail panel exactly as if the corresponding dropdown value had been chosen.
#'
#' @param widget_data A \code{quarto_widget_data} object returned by
#'   \code{\link{widget_data}}.
#' @param geo A geo data list returned by \code{\link{geo_prepare}}.
#' @param filter The filter column name this selector controls. Must match one
#'   of the \code{col} values in the \code{filters} argument of
#'   \code{\link{widget_data}}. E.g. \code{"wijk"}.
#' @param parent_filter Optional. The filter column one level up whose
#'   selected value is used to show or hide polygons. E.g. \code{"gemeente"}
#'   restricts the visible polygons to those belonging to the currently
#'   selected gemeente. Required when \code{layered = TRUE}.
#' @param geo_name_prop The GeoJSON feature property whose values correspond
#'   to the \code{filter} column in the data. Defaults to \code{geo$name_col}
#'   as set by \code{\link{geo_prepare}}.
#' @param geo_parent_prop The GeoJSON feature property whose values
#'   correspond to \code{parent_filter}. Defaults to the value of
#'   \code{parent_filter}.
#' @param show_when_filter Optional. A filter column name. The polygon
#'   selector is hidden until a non-empty value is selected for that filter
#'   level in the associated dropdown. Useful to avoid showing an empty or
#'   confusing map before the user has narrowed the context.
#' @param layered Logical. When \code{TRUE} the selector operates in two
#'   levels: it initially renders one polygon per unique value of
#'   \code{parent_filter} (derived by grouping the child geometries). Clicking
#'   a parent polygon zooms in and switches to the child polygons for that
#'   parent. A configurable back button returns to the parent level.
#'   Requires \code{parent_filter} to be set. Defaults to \code{FALSE}.
#' @param zoom_to_visible Logical. When \code{TRUE} (default) the map
#'   automatically pans and zooms to fit the currently visible polygons
#'   whenever the visible set changes (e.g. after a parent filter selection
#'   or a level transition in layered mode).
#' @param back_label Character string for the back-navigation button shown in
#'   layered mode. Defaults to \code{"Terug naar hoger niveau"}.
#'
#' @return An \code{htmltools::tagList} containing:
#'   \itemize{
#'     \item A \code{<script type="application/json">} tag with the embedded
#'           GeoJSON.
#'     \item A \code{<div class="polygon-selector">} placeholder.
#'     \item A \code{<script>} boot snippet that attaches the map to the
#'           running widget set after \code{DOMContentLoaded}.
#'   }
#'
#' @details
#' \strong{Coordinate system} \cr
#' The GeoJSON produced by \code{\link{geo_prepare}} is already in WGS 84
#' (longitude/latitude), which is what the D3 Mercator projection expects.
#' If you supply your own GeoJSON it must also be in WGS 84.
#'
#' \strong{Layered mode — parent polygon construction} \cr
#' When \code{layered = TRUE} the parent-level polygons are constructed
#' client-side by grouping child features on \code{geo_parent_prop} and
#' collecting their geometries into a \code{GeometryCollection}. Internal
#' boundaries between child polygons may therefore remain visible within a
#' parent region. For fully dissolved boundaries, pre-process the shapefile
#' in R using \code{sf::st_union()} grouped by the parent column before
#' calling \code{\link{geo_prepare}}.
#'
#' \strong{Interaction model} \cr
#' Polygon clicks and dropdown selections are kept in sync via a shared
#' EventBus. Clicking a polygon has the same effect as selecting the
#' corresponding value in the TomSelect dropdown, and vice-versa.
#'
#' @seealso \code{\link{geo_prepare}}, \code{\link{widget_data}},
#'   \code{\link{widget_selectors}}
#'
#' @examples
#' \dontrun{
#' library(quartoWidgets)
#'
#' wijk_geo <- geo_prepare(
#'   path       = "wijken.shp",
#'   name_col   = "statnaam",
#'   extra_cols = "gemeente"
#' )
#'
#' wd <- widget_data(df, hierarchy, id = "demo")
#' wd
#'
#' # Non-layered: show wijk polygons, filter by selected gemeente
#' polygon_selector(
#'   wd,
#'   geo           = wijk_geo,
#'   filter        = "wijk",
#'   parent_filter = "gemeente"
#' )
#'
#' # Layered: start at gemeente level, drill into wijk on click
#' polygon_selector(
#'   wd,
#'   geo              = wijk_geo,
#'   filter           = "wijk",
#'   parent_filter    = "gemeente",
#'   show_when_filter = "gemeente",
#'   layered          = TRUE,
#'   zoom_to_visible  = TRUE,
#'   back_label       = "Back to municipalities"
#' )
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

  # Resolve filter index
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
  #   htmltools::tagList(widget_data_a, widget_data_b, widget_data_c)
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
      version    = "0.3.0",
      src        = system.file("www", package = "quartoWidgets"),
      stylesheet = "custom.css",
      script     = "sunburstr-bundle.js",
      all_files  = FALSE
    )
  )
}
