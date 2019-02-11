#' @include trend-modules.R
#' @describeIn trend-modules Fit a linear trend using
#' \code{\link[mbte]{tr_linear}}. Fit coefficients:
#' \itemize{
#'   \item prefix_slope
#'   \item prefix_rel_slope (relative slope - normed to signal maximum)
#'   \item prefix_intercept
#'   \item prefix_rel_intercept
#' }
#'
#' @export
tm_linear <- function(coef_store = cl_store()) {
  structure(
    list(
      fit_quo = tm_linear_gen_quo,
      store = coef_store
    ),
    # default trend module
    class = c("dtm", "list")
  )
}

# modify fitting quosure for linear trend (store coefficients)
#' @importFrom mbte tr_linear
#' @importFrom rlang quo
tm_linear_gen_quo <- function(id, coef_store) {
  sym_slope <- gen_prefixed_sym(id, "slope")
  sym_rel_slope <- gen_prefixed_sym(id, "rel_slope")
  sym_intercept <- gen_prefixed_sym(id, "intercept")
  sym_rel_intercept <- gen_prefixed_sym(id, "rel_intercept")

  quo({
    fit <- !!tr_linear()

    coefs <- coefficients(fit)
    signal_max <- max(.signal[[.value_sym]])
    slope <- coefs[as.character(.time_sym)]
    rel_slope <- slope / signal_max
    intercept <- coefs["(Intercept)"]
    rel_intercept <- intercept / signal_max
    coef_store$add_row(
      row_nr = .row_nr,
      !!sym_slope := slope,
      !!sym_rel_slope := rel_slope,
      !!sym_intercept := intercept,
      !!sym_rel_intercept := rel_intercept
    )

    fit
  })
}
