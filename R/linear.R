tm_linear <- function(id = "lin", coef_store = cl_store()) {
  structure(
    list(
      fit_quo = tm_linear_gen_quo(coef_store), # modified fitting quosure
      # actual display plugin (below)
      plugin = tm_linear_gen_plugin(id, coef_store)
    ),
    # default trend module
    class = c("dtm", "list")
  )
}

#' @importFrom dplyr if_else mutate
#' @importFrom purrr map2_chr
tm_linear_gen_plugin <- function(id, coef_store) {
  coef_filter_plugin(
    id = id,
    coef_store = coef_store,
    title = "Linear trend",
    selected_par = "rel_slope",
    tr_fun = function(x) {
      fit_mapper <- function(rel_slope, fit) {
        direction <- if_else(rel_slope > 0, "growth", "decay", "unknown")
        paste(fit, direction, sep = "_")
      }

      x %>%
        mutate(fit_modified = map2_chr(rel_slope, fit, fit_mapper))
    }
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_linear
#' @importFrom rlang quo
tm_linear_gen_quo <- function(coef_store) {
  quo({
    fit <- !!tr_linear()

    coefs <- coefficients(fit)
    signal_max <- max(.signal[[.value_sym]])
    slope <- coefs[as.character(.time_sym)]
    rel_slope <- slope / signal_max
    intercept <- coefs["(Intercept)"]
    rel_intercept <- intercept / signal_max
    coef_store$add_row(
      slope = slope,
      rel_slope = rel_slope,
      intercept = intercept,
      rel_intercept = rel_intercept
    )

    fit
  })
}
