# A helper function to ensure, that value is a character vector of length 1.
assert_is_string <- function(value) {
  stopifnot(is.character(value), length(value) == 1)
}

#' @importFrom rlang sym
gen_prefixed_sym <- function(prefix, name) {
  assert_is_string(prefix)
  assert_is_string(name)
  sym(paste(prefix, name, sep = "_"))
}

