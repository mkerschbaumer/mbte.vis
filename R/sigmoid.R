tm_logistic <- function(id = "sig", coef_store = cl_store()) {
  structure(
    list(
      fit_quo = tr_logistic_gen_quo(coef_store), # modified fitting quosure
      store = coef_store
    ),
    # default trend module
    class = c("dtm", "list")
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_logistic
#' @importFrom rlang quo
tr_logistic_gen_quo <- function(coef_store) {
  quo({
    # initialize fit parameters to `NA`
    A <- NA
    B <- NA
    C <- NA
    D <- NA

    safe_fit <- function() {
      signal_max <- max(.signal[[.value_sym]])

      # ensure that the fit-coefficients are added to `coef_store` in any case
      # (even if errors are encountered)
      on.exit({
        coef_store$add_row(
          rownr = .row_nr,
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
