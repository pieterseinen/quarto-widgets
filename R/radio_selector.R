# radio_selector.R — Radio-button selector for dimension filters

#' Radio-button selector for a dimension filter
#'
#' Renders a group of radio buttons for one of the independent dimension
#' filters declared in \code{\link{widget_data}(dimension_filters = ...)}.
#' Selecting a value constrains the dashboard data to rows matching that
#' value, without affecting the cascading geographic filters.
#'
#' @param widget_data A \code{quarto_widget_data} object from \code{\link{widget_data}}.
#' @param dimension The dimension filter column name (must match one of the
#'   columns specified in \code{dimension_filters}).
#' @param label Optional label shown above the radio group. Defaults to the
#'   label defined in \code{dimension_filters}.
#' @param default Optional default value. When \code{NULL} (default), the
#'   first value is automatically selected so the dimension is never
#'   unfiltered.
#'
#' @return An \code{htmltools::tagList} with the radio-button container
#'   and a boot script that attaches it to the widget EventBus.
#'
#' @seealso \code{\link{widget_data}}
#'
#' @examples
#' \dontrun{
#' wd <- widget_data(df,
#'   filters          = c(gemeente = "Gemeente", wijk = "Wijk"),
#'   dimension_filters = c(leeftijdsgroep = "Leeftijdsgroep"),
#'   id = "demo"
#' )
#' wd
#' radio_selector(wd, dimension = "leeftijdsgroep")
#' }
#'
#' @export
radio_selector <- function(
    widget_data,
    dimension,
    label   = NULL,
    default = NULL
) {
  .check_widget_data(widget_data)
  id     <- .widget_id(widget_data)
  config <- .widget_config(widget_data)
  df     <- attr(widget_data, "widget_data_df")

  # Find the dimension in config$dimensionFilters
  dim_filters <- config$dimensionFilters
  dim_cols    <- vapply(dim_filters, `[[`, "", "col")
  dim_idx     <- match(dimension, dim_cols)
  if (is.na(dim_idx))
    stop("Dimension '", dimension, "' not found in widget_data() dimension_filters config.",
         call. = FALSE)

  dim_label <- label %||% dim_filters[[dim_idx]]$label

  # Collect unique values from the data
  vals <- sort(unique(as.character(df[[dimension]])))
  vals <- vals[!is.na(vals) & nzchar(vals)]

  # Auto-select the first value when no explicit default is provided
  if (is.null(default) && length(vals) > 0L) default <- vals[1L]

  div_id <- paste0(id, "-radio-", dimension)

  # Build radio button HTML
  radios <- lapply(vals, function(v) {
    input_id <- paste0(div_id, "-", gsub("[^a-zA-Z0-9]", "-", v))
    is_default <- !is.null(default) && v == default
    htmltools::tags$label(
      class = "radio-selector-option",
      htmltools::tags$input(
        type = "radio", name = div_id, value = v,
        `data-dimension` = dimension,
        # NA produces the bare boolean attribute; NULL omits it entirely
        checked = if (is_default) NA else NULL
      ),
      htmltools::tags$span(v)
    )
  })

  boot <- sprintf(
    paste0(
      'document.addEventListener("DOMContentLoaded", function() {',
      '  var db = window.__quartoWidgets && window.__quartoWidgets["%s"];',
      '  if (db) db.addRadioSelector({',
      '    containerSelector: "%s",',
      '    dimension:         %s,',
      '    defaultValue:      %s',
      '  });',
      '});'
    ),
    id, paste0("#", div_id), .as_js(dimension), .as_js(default)
  )

  htmltools::tagList(
    htmltools::div(
      id = div_id, class = "radio-selector",
      if (!is.null(dim_label)) htmltools::tags$div(class = "radio-selector-label", dim_label),
      do.call(htmltools::tagList, radios)
    ),
    htmltools::tags$script(htmltools::HTML(boot))
  )
}
