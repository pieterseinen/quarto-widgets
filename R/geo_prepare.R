# geo_prepare.R — Prepare geographic data for polygon selectors

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

  # ── Validate that requested columns exist in the shapefile ──
  geom_col   <- attr(dat, "sf_column") %||% "geometry"
  avail_cols <- setdiff(names(dat), geom_col)

  if (!name_col %in% avail_cols)
    stop("[geo_prepare] name_col = \"", name_col, "\" not found in shapefile.\n",
         "  Available columns: ", paste(avail_cols, collapse = ", "),
         call. = FALSE)

  if (!is.null(extra_cols)) {
    missing_extra <- extra_cols[!extra_cols %in% avail_cols]
    if (length(missing_extra) > 0L)
      stop("[geo_prepare] extra_cols not found in shapefile: ",
           paste0('"', missing_extra, '"', collapse = ", "), "\n",
           "  Available columns: ", paste(avail_cols, collapse = ", "),
           call. = FALSE)
  }

  if (!is.null(dissolve_by) && !dissolve_by %in% avail_cols)
    stop("[geo_prepare] dissolve_by = \"", dissolve_by,
         "\" not found in shapefile.\n",
         "  Available columns: ", paste(avail_cols, collapse = ", "),
         call. = FALSE)

  keep <- unique(c(name_col, extra_cols, dissolve_by))
  keep <- keep[keep %in% names(dat)]
  dat  <- dat[, keep, drop = FALSE]
  dat  <- sf::st_make_valid(dat)

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

  # === Child-level GeoJSON ===
  # Simplify individual child features, then transform to WGS84
  child_dat <- dat
  if (!is.null(simplify_tol) && simplify_tol > 0)
    child_dat <- sf::st_simplify(child_dat, dTolerance = simplify_tol, preserveTopology = TRUE)
  child_dat <- sf::st_transform(child_dat, crs = 4326)
  child_geojson <- .to_geojson_string(child_dat)

  # === Parent-level dissolved GeoJSON (if dissolve_by is set) ===
  # IMPORTANT: Union the ORIGINAL (unsimplified) features first, then simplify
  # the union result. If we simplify before union, adjacent polygons develop
  # gaps at shared borders which become holes after st_union().
  parent_geojson <- NULL
  if (!is.null(dissolve_by) && dissolve_by %in% names(dat)) {
    # Temporarily disable S2 spherical geometry to ensure consistent winding
    # order from st_union() — S2 can produce inverted rings.
    s2_was_active <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(s2_was_active), add = TRUE)

    # Union from unsimplified features (in original projected CRS)
    parent_sf <- do.call(rbind, lapply(split(dat, dat[[dissolve_by]]), function(grp) {
      merged <- sf::st_union(grp)
      row    <- sf::st_sf(
        setNames(data.frame(grp[[dissolve_by]][1L], stringsAsFactors = FALSE), dissolve_by),
        geometry = merged
      )
      row
    }))
    parent_sf <- sf::st_make_valid(parent_sf)

    # Simplify the union result (still in original CRS so dTolerance is meters)
    if (!is.null(simplify_tol) && simplify_tol > 0)
      parent_sf <- sf::st_simplify(parent_sf, dTolerance = simplify_tol, preserveTopology = TRUE)

    # Transform to WGS84 for GeoJSON output
    parent_sf <- sf::st_transform(parent_sf, crs = 4326)
    parent_geojson <- .to_geojson_string(parent_sf)
  }

  list(
    geojson         = child_geojson,
    name_col        = name_col,
    parent_geojson  = parent_geojson,
    parent_col      = dissolve_by,
    feature_names   = as.character(sf::st_drop_geometry(child_dat)[[name_col]]),
    feature_parents = if (!is.null(dissolve_by) && dissolve_by %in% names(child_dat))
                        as.character(sf::st_drop_geometry(child_dat)[[dissolve_by]]) else NULL,
    geo_properties  = keep
  )
}
