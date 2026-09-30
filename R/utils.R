# utils.R — Internal helpers and constants for the quartoWidgets package

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

# Convert an R value to a JavaScript literal string for use in sprintf() boot scripts.
# NULL becomes "null"; everything else is JSON-serialised with auto_unbox.
.as_js <- function(x) {
  if (is.null(x)) "null"
  else as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

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
