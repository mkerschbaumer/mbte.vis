#' @importFrom tibble tibble
tm_logistic <- function(id) {
  function(coef_env) {
    coef_env[[id]] <- tibble(
      A = numeric(),
      rel_A = numeric(),
      B = numeric(),
      rel_B = numeric(),
      C = numeric(),
      rel_C = numeric(),
      D = numeric(),
      rel_D = numeric()
    )

    structure(
      list(
        fit_quo = tr_logistic_gen_quo(id, coef_env), # modified fitting quosure
        # actual display plugin (below)
        plugin = tr_logistic_gen_plugin(id, coef_env)
      ),
      # default trend module
      class = c("dtm", "list")
    )
  }
}

#' @importFrom dplyr if_else mutate
#' @importFrom purrr pmap_chr
tr_logistic_gen_plugin <- function(id, coef_env) {
  coef_filter_plugin(
    id = id,
    coef_env = coef_env,
    title = "Logistic trend",
    selected_par = "B",
    tr_fun = function(x) {
      fit_mapper <- function(rel_A, rel_B, fit) {
        direction <- if_else((rel_A * rel_B) > 0, "growth", "decay", "unknown")
        paste(fit, direction, sep = "_")
      }

      x %>%
        mutate(fit_modified = pmap_chr(list(rel_A, rel_B, fit), fit_mapper))
    }
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_logistic
#' @importFrom rlang quo
tr_logistic_gen_quo <- function(id, coef_env) {
  stopifnot(is.environment(coef_env))

  quo({
    # initialize fit parameters to `NA`
    A <- NA
    B <- NA
    C <- NA
    D <- NA

    safe_fit <- function() {
      signal_max <- max(.signal[[.value_sym]])

      # ensure that the fit-coefficients are added to `coef_env` in any case
      # (even if errors are encountered)
      on.exit({
        coef_env[[id]] <- tibble::add_row(coef_env[[id]],
          A = A,
          rel_A = A / signal_max,
          B = B,
          rel_B = B / signal_max,
          C = C,
          rel_C = C / signal_max,
          D = D,
          rel_D = D / signal_max
        )
      })

      fit <- !!tr_logistic()

      # override defaults with actual fit coefficients (fit successful)
      coefs <- coefficients(fit)
      A <- coefs["A"]
      B <- coefs["B"]
      C <- coefs["C"]
      D <- coefs["D"]

      # return fit
      fit
    }

    safe_fit()
  })
}
