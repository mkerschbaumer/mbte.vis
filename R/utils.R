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
