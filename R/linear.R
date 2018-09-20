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

#' @importFrom dplyr if_else mutate
#' @importFrom purrr map2_chr
tm_linear_gen_plugin <- function(id, coef_env) {
  coef_filter_plugin(
    id = id,
    coef_env = coef_env,
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
