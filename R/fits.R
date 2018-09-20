# fit-related functions

# filter and rearrange signals-table according to fit-performance (fits are
# removed if no corresponding metric-result is present)
#' @importFrom dplyr arrange desc inner_join select
#' @importFrom magrittr "%>%"
#' @importFrom mbte is_tbl_mbte mbte_reconstruct
#' @importFrom rlang syms
filter_rearrange_fits <- function(fits, metrics, by = "signal_id") {
  stopifnot(is_tbl_mbte(fits))

  # only keep signal_id-column and result (join afterwards should only add
  # the result-column)
  by_syms <- syms(by)
  metrics <- select(metrics, !!!by_syms, result)

  # add result column and reorder rows
  fits %>%
    inner_join(metrics, by = by) %>%
    arrange(-desc(result)) %>%
    select(-result) %>%
    mbte_reconstruct(fits)
}

# identical to filter_rearrange_fits() - takes reactives as input (verified via
# is.reactive())
#' @importFrom shiny is.reactive reactive req
filter_rearrange_fits_rv <- function(fits, metrics, ...) {
  stopifnot(is.reactive(fits), is.reactive(metrics))

  reactive({
    fits <- fits()
    metrics <- metrics()
    req(fits, metrics)

    filter_rearrange_fits(fits, metrics, ...)
  })
}

# combine fits from individual trend-modules (assume, that a reactiveValues is
# returned by each server function; an element named "filtered", being a
# reactive, must be present)
#
# NOTE: it is assumed, that the column "signal_id" is present
#' @importFrom magrittr "%>%"
#' @importFrom mbte mbte_reconstruct
#' @importFrom purrr map_dfr
#' @importFrom shiny is.reactive is.reactivevalues
combine_fits <- function(trend_rv) {
  stopifnot(is.reactivevalues(trend_rv))

  reactive({
    # needed for mbte_reconstruct()
    stopifnot(length(trend_rv) != 0)
    # NOTE: elements of trend_rv are reactives
    reference <- (trend_rv[[names(trend_rv)[1]]])$filtered
    stopifnot(is.reactive(reference))
    reference <- reference()
    fits <- attr(reference, "fits")

    trend_rv %>%
      names() %>%
      map_dfr(~{
        filtered_tbl_r <- trend_rv[[.x]]$filtered
        stopifnot(is.reactive(filtered_tbl_r))
        filtered_tbl_r()
      }) %>% # name of trend module
      # ensure a tbl_mbte is returned
      mbte_reconstruct(reference)
  })
}
