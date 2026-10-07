# widget_components.R — Individual UI component functions

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

#' Sunburst ring chart container
#'
#' Places the \code{<div>} for the three-ring sunburst chart with optional
#' visual customisation.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param colors Optional named list to customise segment colours. Supported keys:
#'   \describe{
#'     \item{\code{stroke}}{Border colour between segments (default \code{"#ffffff"}).}
#'     \item{\code{no_data}}{Segment colour when no score available (default \code{"#bdbdbd"}).}
#'   }
#' @param label_color Text colour for segment labels. Default \code{"#333333"}.
#' @param font_size Base font size in px for labels. When \code{NULL} (default), font
#'   size is calculated automatically from available segment width. When a numeric value
#'   is provided, it is used as the base size (ring 1 uses base, ring 2 uses base - 2).
#'
#' @return An \code{htmltools::tagList} with an optional options script and the
#'   chart \code{<div>}.
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' sunburst_chart(wd)
#'
#' # Custom styling
#' sunburst_chart(wd, colors = list(stroke = "#eeeeee"),
#'                label_color = "#000000", font_size = 11)
#' }
#'
#' @export
sunburst_chart <- function(widget_data, colors = NULL, label_color = NULL,
                           font_size = NULL) {
  .check_widget_data(widget_data)
  id <- .widget_id(widget_data)

  opts <- list()
  if (!is.null(colors) && is.list(colors)) opts$colors <- colors
  if (!is.null(label_color))               opts$labelColor <- label_color
  if (!is.null(font_size))                 opts$fontSize <- font_size

  div_tag <- htmltools::div(id = paste0(id, "-sunburst"))

  if (length(opts) > 0L) {
    opts_json <- as.character(jsonlite::toJSON(opts, auto_unbox = TRUE, null = "null"))
    htmltools::tagList(
      htmltools::tags$script(
        id = paste0(id, "-sunburst-opts"),
        type = "application/json",
        htmltools::HTML(opts_json)
      ),
      div_tag
    )
  } else {
    div_tag
  }
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

#' Detail indicator table
#'
#' Places the \code{<div>} for the DataTables detail table.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param clickable_selector Logical. When \code{TRUE}, indicator rows in the
#'   table become clickable.  Clicking a row selects the corresponding indicator
#'   in the sunburst chart and shows the comparison plot, just like clicking an
#'   outer ring slice.  Default \code{FALSE}.
#' @return An \code{htmltools::tagList}.
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_table(wd)
#' widget_table(wd, clickable_selector = TRUE)
#' }
#'
#' @export
widget_table <- function(widget_data, clickable_selector = FALSE) {
  .check_widget_data(widget_data)
  id <- .widget_id(widget_data)
  div_id <- paste0(id, "-table-output")

  tags <- list(
    htmltools::div(
      id = div_id,
      `data-clickable-selector` = if (isTRUE(clickable_selector)) "true" else NULL
    )
  )
  do.call(htmltools::tagList, tags)
}

#' Comparison bar chart
#'
#' Places the \code{<div>} for the Plotly comparison bar chart.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param colors Optional named list of hex colour strings for the bar chart.
#'   Supported keys: \code{bar} (default bar colour, default \code{"#CFCFCF"}),
#'   \code{highlight} (selected entity bar colour, default \code{"#2C7FB8"}).
#' @param filtered_comparison Logical. When \code{TRUE}, the comparison plot
#'   interacts with \code{\link{widget_plot_filter}}: only the active entity
#'   and the entities selected in the filter are shown.  Default \code{FALSE}.
#' @param y_scale Optional numeric vector of length 2 giving the fixed y-axis
#'   range, e.g. \code{c(0, 100)}.  When \code{NULL} (default) the y-axis
#'   auto-scales to the data in the plot.
#' @return An \code{htmltools::tagList}.
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_plot(wd)
#' widget_plot(wd, y_scale = c(0, 100))
#' widget_plot(wd, colors = list(bar = "#bdbdbd", highlight = "#e6550d"))
#' widget_plot(wd, filtered_comparison = TRUE)
#' }
#'
#' @seealso \code{\link{widget_plot_filter}}
#'
#' @export
widget_plot <- function(widget_data, colors = NULL, filtered_comparison = FALSE,
                        y_scale = NULL) {
  .check_widget_data(widget_data)
  id <- .widget_id(widget_data)
  div_id <- paste0(id, "-plot-output")

  # Build options object: merge bar/highlight colours with y-scale
  opts <- list()
  if (!is.null(colors) && is.list(colors)) opts <- c(opts, colors)
  if (!is.null(y_scale)) opts$yScale <- y_scale

  tags <- list()
  if (length(opts) > 0L) {
    opts_json <- as.character(jsonlite::toJSON(opts, auto_unbox = TRUE, null = "null"))
    tags <- c(tags, list(
      htmltools::tags$script(
        id = paste0(div_id, "-opts"),
        type = "application/json",
        htmltools::HTML(opts_json)
      )
    ))
  }
  tags <- c(tags, list(
    htmltools::div(
      id = div_id,
      `data-filtered-comparison` = if (isTRUE(filtered_comparison)) "true" else NULL
    )
  ))
  do.call(htmltools::tagList, tags)
}

#' Comparison plot filter control
#'
#' Renders a searchable multi-select list (powered by TomSelect) that lets the
#' end user choose which comparison regions are visible in the
#' \code{\link{widget_plot}} bar chart.  Selected items can be removed with a
#' close button, similar to the selectize.js pattern.
#'
#' The available options are automatically populated from the comparison data:
#' all regions except the currently active (highlighted) one are offered.  The
#' active region always remains visible in the plot regardless of the filter.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param label Optional label displayed above the filter control.
#'   Default \code{"Vergelijkingsgebieden"}.
#' @param placeholder Placeholder text shown when no items are selected.
#'   Default \code{"Selecteer gebieden..."}.
#'
#' @return An \code{htmltools::tagList} containing the filter container and a
#'   boot script that wires it to the widget event bus.
#'
#' @seealso \code{\link{widget_plot}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df, id = "demo")
#' wd
#' widget_plot_filter(wd)
#' widget_plot(wd, filtered_comparison = TRUE)
#' }
#'
#' @export
widget_plot_filter <- function(
    widget_data,
    label       = "Vergelijkingsgebieden",
    placeholder = "Selecteer gebieden..."
) {
  .check_widget_data(widget_data)
  id     <- .widget_id(widget_data)
  div_id <- paste0(id, "-plot-filter")

  boot <- .js_on_ready(paste0(
    '  var db = window.__quartoWidgets && window.__quartoWidgets["', id, '"];\n',
    "  if (db) db.addPlotFilter(",
    .js_object(
      containerSelector = .as_js(paste0("#", div_id)),
      placeholder       = .as_js(placeholder)
    ),
    ");"
  ))

  htmltools::tagList(
    htmltools::div(
      id = div_id, class = "plot-filter",
      if (!is.null(label)) htmltools::tags$label(class = "plot-filter-label", label),
      htmltools::tags$select(
        id       = paste0(div_id, "-select"),
        multiple = "multiple",
        placeholder = placeholder
      )
    ),
    htmltools::tags$script(htmltools::HTML(boot))
  )
}

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
