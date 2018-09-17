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

#' @importFrom rlang is_scalar_character
#' @importFrom shiny NS plotOutput tabPanel
#' @importFrom shinydashboard tabBox
tm_linear_gen_plugin <- function(id, coef_env) {
  stopifnot(is_scalar_character(id))

  plugin_new_default(
    id = id,
    ui = function(id) {
      ns <- NS(id)

      tabBox(
        title = "Linear trend",
        tabPanel(
          title = "Slope",
          plotOutput(ns("slope"))
        ),
        tabPanel(
          title = "Intercept",
          plotOutput(ns("intercept"))
        )
      )
    },
    server = function(input, output, session, fits) {
      coefs <- reactive({ coef_env[[id]] })

      output$slope <- renderPlot({
        hist(coefs()$rel_slope, breaks = 20)
      })

      output$intercept <- renderPlot({
        hist(coefs()$rel_intercept, breaks = 20)
      })
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
