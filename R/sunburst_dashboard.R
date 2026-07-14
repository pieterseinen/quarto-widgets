.normalise_cols <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())

  # Already in list-of-lists format
  if (is.list(x) && length(x) > 0 && is.list(x[[1]])) return(x)

  if (!is.character(x))
    stop("Column specification must be a named character vector or a list of lists.",
         call. = FALSE)

  nms <- names(x) %||% character(length(x))
  # Use lapply (not mapply) to produce an UNNAMED list, which jsonlite
  # serialises as a JSON array instead of an object.
  lapply(seq_along(x), function(i) {
    nm  <- nms[i]
    val <- x[[i]]
    col   <- if (nzchar(nm)) nm  else val
    label <- if (nzchar(nm)) val else gsub("_", " ", val)
    list(col = col, label = label)
  })
}
