# splitting module


# modify fits-table: only keep target-fit (e.g. "lm")
# operate on tibbles
#' @importFrom dplyr mutate
#' @importFrom mbte mbte_reconstruct
#' @importFrom purrr map
#' @importFrom rlang is_scalar_character
split_fits_impl <- function(fits_tbl, target) {
  # symbol of fits-column
  fits_col <- attr(fits_tbl, "fits")
  stopifnot(is_scalar_character(target))

  fits_tbl %>%
    mutate(!!fits_col := map(!!fits_col, ~{
      # only keep target column (e.g. "lm"); assumption: target column is
      # present
      .x[target]
    })) %>%
    mbte_reconstruct(fits_tbl)
}

# perform fit-splitting for trend-name (e.g. only keep fits for "lm"-fits)
#' @importFrom rlang is_scalar_character
#' @importFrom shiny is.reactive reactive req
#' @importFrom shiny.plugin plugin_new_default
split_fits_inner_module <- function(trend_name) {
  stopifnot(is_scalar_character(trend_name) && nzchar(trend_name))

  plugin_new_default(
    id = trend_name,
    server = function(input, output, session, fits_tbl) {
      stopifnot(is.reactive(fits_tbl))

      reactive({
        fits_tbl <- fits_tbl()
        req(fits_tbl)

        split_fits_impl(fits_tbl, trend_name)
      })
    }
  )
}

# pass character vector and invoke inner fit-splitting module for every
# specified target (e.g. "lin", "exp", "sig")
#' @importFrom purrr map
#' @importFrom shiny is.reactive req
#' @importFrom shiny.plugin plugin_create_server plugin_new_default
split_fits_module <- function(targets) {
  stopifnot(is.character(targets))

  # create subplugins
  subplugins <- map(targets, split_fits_inner_module)

  plugin_new_default(
    id = "split_fits",
    server = function(input, output, session, fits_tbl) {
      stopifnot(is.reactive(fits_tbl))

      plugin_create_server(
        !!!subplugins,
        additional_params = list(fits_tbl = fits_tbl)
      )
    }
  )
}
