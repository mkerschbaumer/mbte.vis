# return a reactive, that extracts a target from a list, environment or
# reactiveValues using the `[[`-operator
#' @importFrom rlang is_scalar_character
#' @importFrom shiny reactive
r_extractor <- function(store, target) {
  stopifnot(
    is.list(store) || is.environment(store) || is.reactivevalues(store),
    is_scalar_character(target) && nzchar(target)
  )

  reactive({
    store[[target]]
  })
}

# construct shiny.plugin for expression input (parse expression)
#' @importFrom shiny NS textInput
expr_input_ui <- function(id, ...) {
  ns <- NS(id)

  textInput(ns("expr_str"), ...)
}

#' @importFrom rlang parse_expr
#' @importFrom shiny isTruthy need reactive throttle validate
expr_input_server <- function(input, output, session, throttle_delay = 200,
                              default_value = TRUE) {
  expr <- reactive({
    expr_str <- input$expr_str
    if (isTruthy(expr_str)) {
      tryCatch(parse_expr(expr_str), error = function(e) {
        validate(need(FALSE, "error while parsing expression"))
      })
    } else {
      # return default value / expression
      default_value
    }
  })

  throttle(expr, throttle_delay)

  # return parsed expression
  expr
}

#' @importFrom shiny NS plotOutput tagList uiOutput
hist_filter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    plotOutput(ns("plot")),
    expr_input_ui(ns("filter_expr"), label = "Filter expression"),
    uiOutput(ns("selected_obs"))
  )
}

# The dataset to filter is passed as a tibble. `hist_col` is the name of the
# column used for the histogram.
#' @importFrom dplyr filter
#' @importFrom mbte mbte_reconstruct
#' @importFrom rlang is_expression
#' @importFrom shiny is.reactive need p reactive renderPlot req strong validate
hist_filter_server <- function(input, output, session, dataset, hist_col) {
  stopifnot(is.reactive(dataset), is.reactive(hist_col))

  # values for the histogram
  hist_values <- reactive({
    dataset <- dataset()
    hist_col <- hist_col()
    req(dataset, hist_col)
    stopifnot(is.character(hist_col))
    validate(
      need(nrow(dataset) != 0, "No data to plot available.")
    )

    dataset[[hist_col]]
  })

  # draw histogram
  output$plot <- renderPlot({
    hist_values <- hist_values()
    req(hist_values)
    hist(hist_values)
  })

  # invoke server of plugin for filtering-expression
  filter_expr <- callModule(expr_input_server, "filter_expr")

  # perform filtering based on user-provided expression
  filtered <- reactive({
    filter_expr <- filter_expr()
    stopifnot(is_expression(filter_expr))

    dataset <- dataset()
    req(dataset)
    tryCatch(
      dataset %>%
        filter(!!filter_expr) %>% # use user-provided expression
        mbte_reconstruct(dataset),
      error = function(e) {
        validate(
          need(FALSE, paste("error while evaluating filter expression", e))
        )
      }
    )
  })

  # show text indicating how many observations are selected
  output$selected_obs <- renderUI({
    dataset <- dataset() # unfiltered dataset
    filtered <- filtered()
    req(dataset, filtered)
    n_total <- nrow(dataset)
    n_selected <- nrow(filtered)
    p(strong(n_selected), "/", n_total, " observations selected.")
  })

  # return `filtered` (reactive)
  filtered
}
