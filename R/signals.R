# signal-related functions

# rearrange signals-table according to fit-performance
#' @importFrom dplyr arrange desc right_join select
#' @importFrom magrittr "%>%"
#' @importFrom mbte is_tbl_mbte mbte_reconstruct
rearrange_signals <- function(signals, metrics) {
  stopifnot(is_tbl_mbte(signals))

  # only keep signal_id-column and result (join afterwards should only add
  # the result-column)
  metrics <- select(metrics, signal_id, result)

  # add result column and reorder rows
  signals %>%
    right_join(metrics, by = "signal_id") %>%
    arrange(-desc(result)) %>%
    select(-result) %>%
    mbte_reconstruct(signals)
}

# identical to rearrange_signals() - takes reactives as input (verified via
# is.reactive())
#' @importFrom shiny is.reactive req
rearrange_signals_rv <- function(signals, metrics) {
  stopifnot(is.reactive(signals), is.reactive(metrics))

  signals <- signals()
  metrics <- metrics()
  req(signals, metrics)

  rearrange_signals(signals, metrics)
}

# modify fits-table: only keep target-fit (e.g. "lm")
#' @importFrom dplyr mutate
#' @importFrom mbte mbte_reconstruct
#' @importFrom purrr map
#' @importFrom rlang is_scalar_character
split_fits <- function(fits_tbl, target) {
  # symbol of fits-column
  fits_col <- attr(fits_tbl, "fits")
  stopifnot(is_scalar_character(target))

  fits_tbl %>%
    mutate(!!fits_col := map(!!fits_col, ~{
      # only keep target column (e.g. "lm"); assumption: target column is
      # present
      .x[target]
    })) %>%
    mbte_reconstruct(fits)
}

#' @importFrom shiny is.reactive req
split_fits_rv <- function(fits_tbl, target) {
  stopifnot(is.reactive(fits_tbl))
  fits_tbl <- fits_tbl()
  req(fits_tbl)

  split_fits(fits_tbl, target)
}
