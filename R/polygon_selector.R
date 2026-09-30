# polygon_selector.R — Interactive polygon map selector

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
#' @param suppress_mismatched_polygon_warning Logical. When \code{FALSE}
#'   (default), a warning is emitted during rendering for every parent whose
#'   child-level data values do not match any polygon name in the GeoJSON.
#'   Set to \code{TRUE} to silence these warnings (e.g. when the mismatch is
#'   expected).
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
    show_empty_geometries = TRUE,
    enable_zoom      = FALSE,
    suppress_mismatched_polygon_warning = FALSE
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

  # default_level is only meaningful in layered mode; force 'child' otherwise
  # to prevent _getParentDataValues() from returning an empty set.
  if (!isTRUE(layered)) default_level <- "child"

  geo_name_prop   <- geo_name_prop   %||% geo$name_col
  geo_parent_prop <- geo_parent_prop %||% parent_filter

  # ── Validate that geo object has usable feature names ──
  if (is.null(geo$feature_names) || length(geo$feature_names) == 0L)
    stop("[polygon_selector] The geo object has no feature names (feature_names is empty).\n",
         "  This usually means geo_prepare() was called with a name_col that did not\n",
         "  exist in the shapefile, so no properties were retained in the GeoJSON.\n",
         "  Re-run geo_prepare() with a valid name_col.",
         call. = FALSE)

  # ── Validate geo_name_prop matches a GeoJSON property ──
  if (!is.null(geo$geo_properties) && !geo_name_prop %in% geo$geo_properties)
    stop("[polygon_selector] geo_name_prop = \"", geo_name_prop,
         "\" is not a property in the GeoJSON produced by geo_prepare().\n",
         "  Available GeoJSON properties: ",
         paste(geo$geo_properties, collapse = ", "), "\n",
         "  The geo object was prepared with name_col = \"", geo$name_col, "\".",
         call. = FALSE)

  # ── Validate geo_parent_prop matches a GeoJSON property (when parent_filter is set) ──
  if (!is.null(parent_filter) && !is.null(geo_parent_prop) &&
      !is.null(geo$geo_properties) && !geo_parent_prop %in% geo$geo_properties)
    stop("[polygon_selector] geo_parent_prop = \"", geo_parent_prop,
         "\" is not a property in the GeoJSON produced by geo_prepare().\n",
         "  Available GeoJSON properties: ",
         paste(geo$geo_properties, collapse = ", "), "\n",
         "  Did you forget to include \"", geo_parent_prop,
         "\" in extra_cols when calling geo_prepare()?",
         call. = FALSE)

  # ── Render-time validation: warn about data that does not map to polygons ──
  widget_df <- attr(widget_data, "widget_data_df")
  if (!isTRUE(suppress_mismatched_polygon_warning) &&
      !is.null(widget_df) && !is.null(geo$feature_names) &&
      !is.null(parent_filter) && parent_filter %in% names(widget_df) &&
      filter %in% names(widget_df)) {
    polygon_names   <- geo$feature_names
    polygon_parents <- geo$feature_parents
    parent_groups   <- split(widget_df, widget_df[[parent_filter]])

    for (parent_value in names(parent_groups)) {
      child_values <- unique(as.character(parent_groups[[parent_value]][[filter]]))
      child_values <- child_values[!is.na(child_values) & nzchar(child_values)]

      # Only compare against polygons belonging to THIS parent
      parent_polygons <- if (!is.null(polygon_parents))
        unique(polygon_names[polygon_parents == parent_value])
      else
        unique(polygon_names)

      matched_values   <- child_values[child_values %in% parent_polygons]
      unmatched_values <- child_values[!child_values %in% parent_polygons]

      if (length(unmatched_values) > 0L) {
        sample_text <- paste(utils::head(unmatched_values, 10), collapse = ", ")
        if (length(unmatched_values) > 10L)
          sample_text <- paste0(sample_text, " (and ", length(unmatched_values) - 10L, " more)")
        consequence_msg <- if (length(matched_values) == 0L)
          "The parent polygon will appear empty (greyed out)."
        else
          paste0(length(matched_values), " value(s) did match; the parent polygon will be shown.")
        warning(
          "[quartoWidgets] polygon_selector: ", length(unmatched_values), " of ",
          length(child_values), " '", filter, "' values in ", parent_filter,
          " = \"", parent_value, "\" do not match any polygon in the GeoJSON.\n",
          "  Unmatched: ", sample_text, "\n",
          "  Available polygons for this ", parent_filter, ": ",
          paste(parent_polygons, collapse = ", "), "\n",
          "  ", consequence_msg, "\n",
          "  Set suppress_mismatched_polygon_warning = TRUE to silence this warning.",
          call. = FALSE
        )
      }
    }
  }

  # Build colors JS object (only emit non-NULL overrides)
  colors_js <- if (!is.null(colors) && is.list(colors)) {
    parts <- vapply(names(colors), function(k) {
      paste0(k, ": ", .as_js(colors[[k]]))
    }, "")
    paste0("{", paste(parts, collapse = ", "), "}")
  } else {
    "null"
  }

  geo_script_id        <- paste0(id, "-polygon-geo-",       filter_idx)
  div_id               <- paste0(id, "-polygon-selector-",  filter_idx)
  has_parent_geo       <- !is.null(geo$parent_geojson)
  parent_geo_script_id <- paste0(id, "-polygon-parent-geo-", filter_idx)

  boot <- .js_on_ready(paste0(
    '  var db = window.__quartoWidgets && window.__quartoWidgets["', id, '"];\n',
    "  if (db) db.addPolygonSelector(",
    .js_object(
      containerSelector    = .as_js(paste0("#", div_id)),
      geoScriptId          = .as_js(geo_script_id),
      parentGeoScriptId    = if (has_parent_geo) .as_js(parent_geo_script_id) else "null",
      filterLevel          = as.character(filter_idx),
      nameProp             = .as_js(geo_name_prop),
      parentFilter         = .as_js(parent_filter),
      parentProp           = .as_js(geo_parent_prop),
      showWhenFilter       = .as_js(show_when_filter),
      layered              = if (isTRUE(layered)) "true" else "false",
      defaultLevel         = .as_js(default_level),
      zoomToVisible        = if (isTRUE(zoom_to_visible)) "true" else "false",
      backLabel            = .as_js(back_label),
      selectedStrokeWidth  = as.character(selected_stroke_width),
      colors               = colors_js,
      showEmptyGeometries  = if (isTRUE(show_empty_geometries)) "true" else "false",
      enableZoom           = if (isTRUE(enable_zoom)) "true" else "false"
    ),
    ");"
  ))

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
