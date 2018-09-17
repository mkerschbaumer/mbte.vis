# extract fitting quosure from trend module
tm_extract_quo <- function(x, ...) {
  UseMethod("tm_extract_quo")
}

# implementation for default trend module
tm_extract_quo.dtm <- function(x, ...) {
  x$fit_quo
}

#' @importFrom purrr map
tm_extract_quos <- function(l) {
  map(l, tm_extract_quo)
}

# extract plugin
tm_extract_plugin <- function(x, ...) {
  UseMethod("tm_extract_plugin")
}

# implementation for default trend module
tm_extract_plugin.dtm <- function(x, ...) {
  x$plugin
}

#' @importFrom purrr map
tm_extract_plugins <- function(l) {
  map(l, tm_extract_plugin)
}

#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @importFrom shiny need reactive validate
#' @importFrom shiny.plugin plugin_extract_id
setup_trend_module <- function(module, fits_r, metrics_r) {
  # extract shiny.plugin
  plugin <- tm_extract_plugin(module)
  # ID / name of trend-fitting helper (e.g. 'lin' etc.)
  trend_name <- plugin_extract_id(plugin)

  # only keep metrics for used algorithm/trend fitting helper (e.g. 'lin')
  filtered_metrics <- reactive({
    filtered <- metrics_r() %>%
      filter(fit == !!trend_name)

    # assert, that at least 1 fit is present
    validate(need(nrow(filtered) != 0, "Filtering produces empty metrics-table."))
    filtered
  })

  # split fits (only keep fits for current trend fitting module)
  split_fits <- reactive({
    fits_tbl <- fits_r()
    req(fits_tbl)

    split_fits_impl(fits_tbl, trend_name)
  })

  # filter/rearrange fits according to fit performance
  rearranged_fits <- rearrange_fits_rv(split_fits, filtered_metrics)

  # invoke shiny.plugin (pass filtered/rearranged fits instead of all fits)
  callModule(plugin_extract_server(plugin), trend_name,
    fits = rearranged_fits
  )
}

# NOTE: trend modules passed via ellipsis
#' @importFrom purrr invoke map
#' @importFrom rlang list2
#' @importFrom shiny reactiveValues
setup_trend_modules <- function(fits_r, metrics_r, ...) {
  # trend modules
  modules <- list2(...)

  # invoke servers of trend modules
  invoked_servers <- map(modules, setup_trend_module,
    fits_r = fits_r,
    metrics_r = metrics_r
  )

  invoke(reactiveValues, invoked_servers)
}
