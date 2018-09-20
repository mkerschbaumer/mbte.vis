# only visualise best performing fit
#' @importFrom shiny NS uiOutput
best_fit_vis_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("ui"))
}

#' @importFrom ggplot2 aes geom_point geom_path ggplot
#' @importFrom mbte mbte_panel_plot
#' @importFrom rlang sym
#' @importFrom shiny is.reactive plotOutput renderPlot renderUI req selectInput
#'   tagList
best_fit_vis_server <- function(input, output, session, rearranged,
                                height = "800px") {
  stopifnot(is.reactive(rearranged))

  fits_plot <- reactive({
    rearranged <- rearranged()
    req(rearranged)
    validate(
      need(nrow(rearranged) != 0, "Empty dataset: no data to plot aviablable")
    )

    # use column "fit_modified" as color-column if present (column "fit" is
    # fallback)
    color_col <- if ("fit_modified" %in% colnames(rearranged)) {
      sym("fit_modified")
    } else {
      sym("fit")
    }

    time <- attr(rearranged, "time")
    value <- attr(rearranged, "value")

    mbte_panel_plot(
      rearranged,
      expr = {
        ggplot(.u_signals, aes(!!time, !!value)) +
          geom_point() +
          geom_path(aes(color = !!color_col), data = .u_fits)
      },
      signal_id
    )
  })

  output$ui <- renderUI({
    ns <- session$ns
    fits_plot <- fits_plot()
    req(fits_plot)

    tagList(
      selectInput(ns("plotSelector"), "Plot to show", seq_along(fits_plot)),
      plotOutput(ns("plot"), height = height)
    )
  })

  output$plot <- renderPlot({
    choice <- input$plotSelector
    fits_plot <- fits_plot()
    req(choice, fits_plot)
    fits_plot[[as.integer(choice)]]
  })
}
