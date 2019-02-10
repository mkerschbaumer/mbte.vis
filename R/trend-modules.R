# extract fitting quosure from trend module
tm_extract_quo <- function(x, ...) {
  UseMethod("tm_extract_quo")
}

# implementation for default trend module
#' @export
tm_extract_quo.dtm <- function(x, ...) {
  x$fit_quo
}

#' @importFrom purrr map
tm_extract_quos <- function(l) {
  map(l, tm_extract_quo)
}

tm_extract_store <- function(x, ...) {
  UseMethod("tm_extract_store")
}

# implementation for default trend module
#' @export
tm_extract_store.dtm <- function(x, ...) {
  x$store
}

# extract coefficient stores form trend module
#' @importFrom purrr map
tm_extract_stores <- function(l) {
  map(l, tm_extract_store)
}
