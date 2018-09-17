# fit-related functions

# rearrange signals-table according to fit-performance
#' @importFrom dplyr arrange desc right_join select
#' @importFrom magrittr "%>%"
#' @importFrom mbte is_tbl_mbte mbte_reconstruct
#' @importFrom rlang syms
rearrange_fits <- function(fits, metrics, by = "signal_id") {
  stopifnot(is_tbl_mbte(fits))

  # only keep signal_id-column and result (join afterwards should only add
  # the result-column)
  by_syms <- syms(by)
  metrics <- select(metrics, !!!by_syms, result)

  # add result column and reorder rows
  fits %>%
    right_join(metrics, by = by) %>%
    arrange(-desc(result)) %>%
    select(-result) %>%
    mbte_reconstruct(fits)
}

# identical to rearrange_signals() - takes reactives as input (verified via
# is.reactive())
#' @importFrom shiny is.reactive reactive req
rearrange_fits_rv <- function(fits, metrics) {
  stopifnot(is.reactive(fits), is.reactive(metrics))

  reactive({
    fits <- fits()
    metrics <- metrics()
    req(fits, metrics)

    rearrange_fits(fits, metrics)
  })
}
