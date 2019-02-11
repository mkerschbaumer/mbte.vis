#' A wrapper to include fit coefficients as a separate column
#'
#' \code{\link[mbte:mbte_fit]{mbte_fit}} is wrapped. Currently, the column
#' \code{coefficients} is added to the resulting tibble.
#'
#' @inheritParams mbte::mbte_fit
#' @param ... Trend modules to use.
#'
#' @return A \code{\link[mbte:tbl_mbte]{tbl_mbte}} with fit coefiicients added.
#' 
#' @importFrom dplyr "%>%" arrange select
#' @importFrom mbte mbte_fit
#' @importFrom purrr map_dfr
#' @export
mbte_coef_fit <- function(x, ...) {
  l <- list(...)

  # extract fitting quosures and coefficient stores from trend modules
  fitting_quos <- tm_extract_quos(l)
  stores <- tm_extract_stores(l)

  fit_result <- mbte_fit(x, !!!fitting_quos)

  # extract fit coefficients
  coefs <- stores %>%
    map_dfr(~.x$get_tibble()) %>% # extract fit coefficients
    arrange(row_nr) %>%
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

