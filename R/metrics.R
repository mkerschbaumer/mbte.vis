#' @importFrom shiny NS plotOutput selectInput uiOutput
#' @importFrom shinydashboard box
metrics_hist_ui <- function(id) {
  ns <- NS(id)

  box(
    title = "Metric filtering",
    width = 4,
    selectInput(ns("metric_to_use"), "Metric to use:", character(0)),
    plotOutput(ns("metric_hist")),
    uiOutput(ns("metric_slider"))
  )
}

#' @importFrom shiny is.reactive observe reactive renderUI req sliderInput
#'   updateSelectInput
#' @importFrom tibble is_tibble
metrics_hist_server <- function(input, output, session, metrics) {
  stopifnot(is.reactive(metrics))

  # change selection options for metrics to use if metrics-table changes
  observe({
    metrics <- metrics()
    req(metrics)
    available_metrics <- unique(metrics$metric)
    updateSelectInput(session, "metric_to_use",
      choices = available_metrics, selected = NULL
    )
  })

  # only keep desired metric (e.g. 'nrmse')
  metrics_filtered <- reactive({
    metrics <- metrics()
    desired_metric <- input$metric_to_use
    req(metrics, desired_metric)
    validate(
      need(
        desired_metric %in% metrics$metric,
        message = "Chosen metric must be available in metric-table."
      )
    )

    metrics %>%
      filter(metric == !!desired_metric)
  })

  output$metric_hist <- renderPlot({
    metrics <- metrics_filtered()
    req(metrics)
    stopifnot(is_tibble(metrics))
    hist(metrics$result)
  })

  output$metric_slider <- renderUI({
    metrics <- metrics_filtered()
    req(metrics)
    slider_min <- signif(min(metrics$result, na.rm = TRUE), 3)
    slider_max <- signif(max(metrics$result, na.rm = TRUE), 3)
    sliderInput(
      inputId = session$ns("metric_limits"),
      label = "Metric-range",
      min = slider_min,
      max = slider_max,
      value = c(slider_min, slider_max),
      width = "90%"
    )
  })

  # filter metrics-table according to range selected by the user
  metrics_r <- reactive({
    metrics <- metrics_filtered()
    metric_range <- input$metric_limits
    req(metrics, metric_range)
    metric_min <- metric_range[1]
    metric_max <- metric_range[2]

    metrics %>%
      filter(result >= !!metric_min & result <= !!metric_max)
  })

  # return reactive
  metrics_r
}
