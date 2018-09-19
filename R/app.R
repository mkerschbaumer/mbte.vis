#' @importFrom shiny NS
#' @importFrom shiny.plugin plugin_create_ui
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#'   dashboardSidebar
app_ui <- function(trend_plugins) {
  dashboardPage(
    dashboardHeader(title = "Fits explorer"),
    dashboardSidebar(),
    dashboardBody(
      plugin_create_ui(!!!trend_plugins, ns = NS("wrappers"))
    ),
    skin = "black"
  )
}

#' @importFrom mbte is_tbl_mbte
#' @importFrom shiny reactive
app_server <- function(trend_plugins, fits, metrics) {
  stopifnot(is_tbl_mbte(fits))

  function(input, output, session) {
    fits_r <- reactive(fits)
    metrics_r <- reactive(metrics)

    trend_rv <- setup_tm_servers(fits_r, metrics_r, !!!trend_plugins)

    filtered_combined <- combine_fits(trend_rv)

    # NOTE: 'fit' used for compatibility with metrics
    rearranged <- rearrange_fits_rv(filtered_combined, metrics_r,
      by = c("signal_id", "fit")
    )
  }
}

#' Run shiny app
#' @importFrom rlang list2
#' @importFrom shiny shinyApp
#' @export
run_app <- function(fits, metrics, ...) {
  trend_plugins <- tm_extract_plugins(list2(...))
  ui <- app_ui(trend_plugins)
  server <- app_server(trend_plugins, fits, metrics)
  shinyApp(ui, server)
}
