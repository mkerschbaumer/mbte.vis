#' A wrapper to include fit coefficients as a separate column
#'
#' \code{\link[mbte:mbte_fit]{mbte_fit}} is wrapped. Currently, the column
#' \code{coefficients} is added to the resulting tibble.
#'
#' @inheritParams mbte::mbte_fit
#' @param ... Trend modules to use.
#'
#' @note It is assumed, that each trend module adds coefficients to the
#'   corresponding coefficient store exactly once. If fitting fails, NA's
#'   should be added.
#'
#' @return A \code{\link[mbte:tbl_mbte]{tbl_mbte}} with fit coefiicients added.
#' 
#' @importFrom dplyr "%>%" arrange group_by select summarize_all
#' @importFrom mbte mbte_fit
#' @importFrom purrr imap map_dfr
#' @importFrom rlang list2
#' @export
mbte_coef_fit <- function(x, ...) {
  l <- imap(list2(...), tm_initialize)

  # extract fitting quosures and coefficient stores from trend modules
  fitting_quos <- tm_extract_quos(l)
  stores <- tm_extract_stores(l)

  fit_result <- mbte_fit(x, !!!fitting_quos)

  # extract fit coefficients
  coefs <- stores %>%
    map_dfr(~.x$get_tibble()) %>% # extract fit coefficients
    arrange(row_nr) %>%
    group_by(row_nr) %>%
    # make sure only one entry per column is present
    # NOTE: It is assumed, that fitting quosures perform one add_row()-call
    # (regarding the coefficient store).
    summarize_all(keep_nonNA) %>%
    {
      split(select(., -row_nr), .$row_nr)
    }

  # Make sure, that the added column has the rigth length (it is expected,
  # that fitting modules produce NA's, in order to keep the number
  # of rows for the final result consistent).
  if (!identical(length(coefs), nrow(x))) {
    stop("Number of rows for coefficients inconsistent")
  }
  fit_result$coefficients <- coefs

  fit_result
}

# fix R CMD CHECK issues:
# - `.`: magrittr pipe
# - `row_nr`: column added by trend modules in this package (rownr of the
#     corresponding fit)
globalVariables(c(".", "row_nr"))

