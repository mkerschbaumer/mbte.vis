tm_logistic <- function(coef_store = cl_store()) {
  structure(
    list(
      fit_quo = tr_logistic_gen_quo,
      store = coef_store
    ),
    # default trend module
    class = c("dtm", "list")
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_logistic
#' @importFrom rlang quo
tr_logistic_gen_quo <- function(id, coef_store) {
  # add symbols for coefficient store
  sym_A <- gen_prefixed_sym(id, "A")
  sym_rel_A <- gen_prefixed_sym(id, "rel_A")
  sym_B <- gen_prefixed_sym(id, "B")
  sym_rel_B <- gen_prefixed_sym(id, "rel_B")
  sym_C <- gen_prefixed_sym(id, "C")
  sym_rel_C <- gen_prefixed_sym(id, "rel_C")
  sym_D <- gen_prefixed_sym(id, "D")
  sym_rel_D <- gen_prefixed_sym(id, "rel_D")

  quo({
    # initialize fit parameters to `NA`
    A <- B <- C <- D <- NA

    safe_fit <- function() {
      signal_max <- max(.signal[[.value_sym]])

      # ensure that the fit-coefficients are added to `coef_store` in any case
      # (even if errors are encountered)
      on.exit({
        coef_store$add_row(
          row_nr = .row_nr,
          !!sym_A := A,
          !!sym_rel_A := A / signal_max,
          !!sym_B := B,
          !!sym_rel_B := B / signal_max,
          !!sym_C := C,
          !!sym_rel_C := C / signal_max,
          !!sym_D := D,
          !!sym_rel_D := D / signal_max
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
