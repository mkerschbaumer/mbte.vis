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

# set the fitting quosure of a trend module (needed by tm_intialize())
tm_set_quo <- function(x, value, ...) {
  UseMethod("tm_set_quo")
}

#' @export
tm_set_quo.dtm <- function(x, value, ...) {
  x$fit_quo <- value
  x
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

# Check if the fitting quosure is a function. In that case assume, that the
# function produces a fitting quosure if it is called it with the following
# arguments: f(id, coef_store)
#' @importFrom rlang is_quosure
tm_initialize <- function(x, id) {
  fit_quo <- tm_extract_quo(x)

  if (is.function(fit_quo)) {
    coef_store <- tm_extract_store(x)

    # use function to generate quosure
    fit_quo <- fit_quo(id, coef_store)
    x <- tm_set_quo(x, fit_quo)
  }

  # ensure, that a quosure is produced in any case
  stopifnot(is_quosure(fit_quo))
  x
}

