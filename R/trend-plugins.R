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
#' @importFrom shiny.plugin plugin_extract_id plugin_extract_server plugin_new_default
wrap_plugin <- function(plugin, fits_r, metrics_r) {
  # ID / name of trend-fitting helper (e.g. 'lin' etc.)
  trend_name <- plugin_extract_id(plugin)

  # split fits (only keep fits for current trend fitting module)
  split_fits <- reactive({
    fits_tbl <- fits_r()
    req(fits_tbl)

    split_fits_impl(fits_tbl, trend_name)
  })

  # only keep metrics for used algorithm/trend fitting helper (e.g. 'lin')
  filtered_metrics <- reactive({
    filtered <- metrics_r() %>%
      filter(fit == !!trend_name)

    # assert, that at least 1 fit is present
    validate(need(nrow(filtered) != 0, "Filtering produces empty metrics-table."))
    filtered
  })

  # filter/rearrange fits according to fit performance
  rearranged_fits <- rearrange_fits_rv(split_fits, filtered_metrics)

  # create wrapper sgda.plugin: use same ID but modified server function (pass
  # `rearranged_fits` to original server function)
  #
  # NOTE: the wrapped plugin is only needed for the call to
  # plugin_create_server() in setup_trend_modules()
  plugin_new_default(
    plugin_extract_id(plugin),
    server = function(input, output, session) {
      server_fun <- plugin_extract_server(plugin)
      server_fun(input, output, session, fits = rearranged_fits)
    }
  )
}

# NOTE: trend modules passed via ellipsis
#' @importFrom purrr invoke map
#' @importFrom rlang list2
#' @importFrom shiny getDefaultReactiveDomain reactiveValues
setup_tm_servers <- function(fits_r, metrics_r, ...) {
  # trend modules
  modules <- list2(...) %>%
    map(wrap_plugin, fits_r = fits_r, metrics_r = metrics_r)

  # use a custom namespace to avoid naming conflicts (e.g. a trend-module has
  # the same name as a reactive in the global namespace)
  session <- getDefaultReactiveDomain()
  plugin_create_server(!!!modules, session = session$makeScope("wrappers"))
}

#' @importFrom purrr as_mapper map
#' @importFrom rlang list2 new_environment
init_trend_modules <- function(...) {
  # interpret ellipsis as module-builder functions
  modules <- list2(...)
  modules <- map(modules, as_mapper)

  # create environment for storing coefficients
  coef_env <- new_environment()

  # invoke model-building functions with `coef_env`
  map(modules, ~.x(coef_env))
}
