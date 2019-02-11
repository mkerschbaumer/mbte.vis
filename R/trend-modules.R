#' Trend modules for coefficient based fitting
#'
#' Trend modules are wrappers around
#' \code{\link[mbte:fitting-helpers]{trend fitting helpers}} in the mbte-
#' package. They are meant to be used with \code{\link{mbte_coef_fit}}.
#'
#' A fitting module consists of the following components:
#' \describe{
#'   \item{fitting quosure}{Adapted fitting quosure, based on the correspoding
#'     fitting helper in the mbte-package.}
#'   \item{coefficient store}{Stores fit coefficients.}
#' }
#'
#' @param coef_store A \code{\link[mbte.vis:coef_store]{coefficient-store}} to
#'   record fit parameters.
#'
#' @examples
#' # use dataset from the mbte package
#' library(mbte)
#' data(filtered_signals)
#'
#' # perform fitting (record the coefficients)
#' mbte_coef_fit(filtered_signals, lin = tm_linear(), log = tm_logistic())
#'
#' # equivalent using the mbte-package (don't store coefficients)
#' mbte_fit(filtered_signals, lin = !!tr_linear(), log = !!tr_logistic)
#'
#' @name trend-modules
NULL

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

