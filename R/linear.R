tm_linear <- function(id = "lin", coef_store = cl_store()) {
  structure(
    list(
      fit_quo = tm_linear_gen_quo(coef_store), # modified fitting quosure
      store = coef_store
    ),
    # default trend module
    class = c("dtm", "list")
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
      rownr = .row_nr,
      slope = slope,
      rel_slope = rel_slope,
      intercept = intercept,
      rel_intercept = rel_intercept
    )

    fit
  })
}
