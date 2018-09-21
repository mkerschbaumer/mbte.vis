# insert JS code for determining window size; NOTE: solution by Xiongbing Jin
#' @importFrom shiny tags
window_size_js <- function() {
  tags$head(
    tags$script('
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
    ')
  )
}

#' @importFrom shiny NS column
#' @importFrom shiny.plugin plugin_create_ui
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#'   dashboardSidebar
app_ui <- function(trend_plugins) {
  dashboardPage(
    dashboardHeader(title = "Fits explorer"),
    dashboardSidebar(
      disable = TRUE
    ),
    dashboardBody(
      window_size_js(),
      column(3,
        metrics_hist_ui("metrics_filter"),
        plugin_create_ui(!!!trend_plugins, ns = NS("wrappers"))
      ),
      column(9,
        best_fit_vis_ui("bestFitVis")
      )
    ),
    skin = "black"
  )
}

#' @importFrom mbte is_tbl_mbte
#' @importFrom shiny callModule reactive throttle
app_server <- function(trend_plugins, fits, metrics) {
  stopifnot(is_tbl_mbte(fits))

  function(input, output, session) {
    fits_r <- reactive(fits)
    metrics_r_raw <- reactive(metrics)

    metrics_r <- callModule(metrics_hist_server, "metrics_filter",
      metrics = metrics_r_raw
    )

    trend_rv <- setup_tm_servers(fits_r, metrics_r, !!!trend_plugins)

    filtered_combined <- combine_fits(trend_rv)

    # NOTE: 'fit' used for compatibility with metrics
    rearranged <- filter_rearrange_fits_rv(filtered_combined, metrics_r,
      by = c("signal_id", "fit")
    )

    height <- reactive(input$dimension[2])
    throttle(height, 300)

    callModule(best_fit_vis_server, "bestFitVis",
      rearranged = rearranged, height = height
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
