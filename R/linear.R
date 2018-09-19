#' @importFrom tibble tibble
tm_linear <- function(id) {
  function(coef_env) {
    coef_env[[id]] <- tibble(
      slope = numeric(),
      rel_slope = numeric(),
      intercept = numeric(),
      rel_intercept = numeric()
    )

    structure(
      list(
        fit_quo = tm_linear_gen_quo(id, coef_env), # modified fitting quosure
        # actual display plugin (below)
        plugin = tm_linear_gen_plugin(id, coef_env)
      ),
      # default trend module
      class = c("dtm", "list")
    )
  }
}

#' @importFrom dplyr bind_cols filter select
#' @importFrom mbte mbte_reconstruct
#' @importFrom rlang is_expression is_scalar_character
#' @importFrom shiny callModule need NS p plotOutput reactive reactiveValues
#'   renderPlot renderUI req selectInput strong textInput uiOutput validate
#' @importFrom shiny.plugin plugin_new_default
#' @importFrom shinydashboard box
tm_linear_gen_plugin <- function(id, coef_env) {
  stopifnot(is_scalar_character(id))

  plugin_new_default(
    id = id,
    ui = function(id) {
      ns <- NS(id)

      box(
        title = "Linear trend",
        selectInput(ns("to_display"), "Parameter to display",
          choices = c("rel_slope", "slope", "rel_intercept", "intercept"),
          selected = "rel_slope"
        ),
        plotOutput(ns("plot")),
        expr_input_ui(ns("filter_expr"), label = "Filter expression"),
        uiOutput(ns("remaining_samples"))
      )
    },
    server = function(input, output, session, fits) {
      coefs <- reactive({ coef_env[[id]] })

      # combine fits with coefficients
      combined <- reactive({
        coefs <- coefs()
        fits <- fits()
        req(coefs, fits)

        fits %>%
          bind_cols(coefs) %>%
          mbte_reconstruct(fits)
      })

      # data for the histogram (selected parameter)
      plot_dataset <- reactive({
        coefs <- coefs()
        relative <- input$relative
        choice <- input$to_display
        req(coefs, choice)
        coefs[[choice]]
      })

      # draw histogram
      output$plot <- renderPlot({
        plot_dataset <- plot_dataset()
        req(plot_dataset)
        hist(plot_dataset)
      })

      # invoke server of plugin for filtering-expression
      filter_expr <- callModule(expr_input_server, "filter_expr")

      # perform filtering based on coefficients
      filtered <- reactive({
        filter_expr <- filter_expr()
        stopifnot(is_expression(filter_expr))

        combined <- combined()
        coefs <- coefs()
        req(combined, coefs)
        cols_to_drop <- colnames(coefs)
        tryCatch(
          combined %>%
            filter(!!filter_expr) %>% # use user-provided expression
            select(-!!cols_to_drop) %>% # remove coefficient-columns
            mbte_reconstruct(combined),
          error = function(e) {
            validate(
              need(FALSE, paste("error while evaluating filter expression", e))
            )
          }
        )
      })

      # show text indicating how many samples are selected
      output$remaining_samples <- renderUI({
        fits <- fits()
        filtered <- filtered()
        total_fits <- nrow(fits)
        selected_fits <- nrow(filtered)
        p(strong(selected_fits), "/", total_fits, " fits selected.")
      })

      # return reactiveValues at the end
      rv <- reactiveValues()
      rv$filtered <- filtered

      rv
    },
    displayname = "Linear trend",
    classes = "trend-filter"
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_linear
#' @importFrom rlang quo
tm_linear_gen_quo <- function(id, coef_env) {
  stopifnot(is.environment(coef_env))

  quo({
    fit <- !!tr_linear()

    coefs <- coefficients(fit)
    signal_max <- max(.signal[[.value_sym]])
    slope <- coefs[as.character(.time_sym)]
    rel_slope <- slope / signal_max
    intercept <- coefs["(Intercept)"]
    rel_intercept <- intercept / signal_max
    coef_env[[id]] <- tibble::add_row(coef_env[[id]],
      slope = slope,
      rel_slope = rel_slope,
      intercept = intercept,
      rel_intercept = rel_intercept
    )

    fit
  })
}
