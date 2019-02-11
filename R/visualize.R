# unnest coefficients column
# TODO: remove passing of column name (implement mechanism similar to the one
# used in the main mbte-package)
#' @importFrom dplyr "%>%"
#' @importFrom mbte mbte_reconstruct
#' @importFrom rlang sym
#' @importFrom tidyr unnest
mbte_unnest_coefficients <- function(x, colname = "coefficients") {
  colname <- sym(colname)
  x %>%
    unnest(!!colname) %>%
    mbte_reconstruct(x)
}

#' Visualizae coefficient-based fits using trelliscopejs package
#'
#' @inheritParams mbte::mbte_fit
#' @param ... Columns to use for facetting (see
#'   \code{\link[trelliscopejs]{facet_trelliscope}}).
#'
#' @importFrom dplyr filter left_join mutate rename select
#' @importFrom ggplot2 aes geom_path geom_point ggplot scale_color_brewer theme
#'   theme_bw
#' @importFrom mbte mbte_reconstruct mbte_unnest_signals mbte_unnest_fits
#' @importFrom purrr reduce
#' @importFrom rlang ensyms expr new_formula
#' @importFrom trelliscopejs facet_trelliscope
#' @export
mbte_visualize_coef <- function(x, ...) {
  # columns for facet grouping
  group_cols <- ensyms(...)

  # TODO: use exported function from mbte package
  time_col <- mbte:::attr_time(x)
  value_col <- mbte:::attr_value(x)

  # unnest needed list columns
  # TODO: remove hardcoded column name
  coefficients <- x %>%
    select(!!!group_cols, coefficients) %>%
    mbte_unnest_coefficients("coefficients")
  x <- x %>%
    select(-coefficients) %>%
    mbte_reconstruct(x)
  signals <- mbte_unnest_signals(x)
  fits <- rename(mbte_unnest_fits(x), .predicted = !!value_col)

  # combine datasets
  res <- signals %>%
    left_join(fits, by = c(as.character(group_cols), as.character(time_col))) %>%
    left_join(coefficients, by = as.character(group_cols)) %>%
    # remove rows, where no prediction is present
    filter(!is.na(.predicted))

  # build formula for facetting
  facet_rhs <- reduce(group_cols, ~expr(!!.x + !!.y))
  facet_formula <- new_formula(NULL, facet_rhs)

  # plot using ggplot2 and trelliscopejs
  ggplot(res, aes(!!time_col, color = fit)) +
    geom_point(aes(y = !!value_col), color = "black") +
    geom_path(aes(y = .predicted)) +
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "none") +
    facet_trelliscope(facet_formula, "Fits", group = "fit",
      nrow = 2, ncol = 5, scales = "free"
    )
}

