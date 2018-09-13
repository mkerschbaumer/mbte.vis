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
