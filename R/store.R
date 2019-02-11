#' Store for fit-coefficients
#'
#' For an interactive analysis of fits using \code{\link{mbte_visualize_coef}},
#' filtering based on model coefficients may be required. The mentioned options
#' below are available for storing model coefficients.
#'
#' A coefficient store should provide the following functions (accessible via
#' the \code{$}-operator):
#' \describe{
#'   \item{add_row(...)}{Add model coefficients (ellipsis must be named).}
#'   \item{get_tibble()}{Return a \code{\link[tibble]{tibble}} with the added
#'     model coefficients (the order, in which the coefficients were added, is
#'     preserved).}
#' }
#'
#' @name coef_store
NULL

#' @describeIn  coef_store Store fitting-coefficients using a closure (rely on
#'   lexical scoping).
#' @examples
#' # create closure-based coefficient-store
#' store <- cl_store()
#'
#' # store coefficients
#' store$add_row(A = 10, B = -5, C = 0)
#' store$add_row(A = -1, B = 3, C = 1)
#'
#' # retrieve tibble containing coefficients
#' store$get_tibble()
#'
#' @importFrom dplyr bind_rows
#' @importFrom rlang list2
#' @export
cl_store <- function() {
  lst <- list()

  list(
    add_row = function(...) {
      lst <<- append(lst, list(list2(...)))
      invisible(NULL)
    },
    get_tibble = function() {
      bind_rows(lst)
    }
  )
}
