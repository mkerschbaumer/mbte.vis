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

# Only return the entries of a numeric vector, which are not NA.
# If x is not a numeric vector or of length 0, NA is returned.
keep_nonNA <- function(x) {
  is_na <- is.na(x)

  if (all(is_na) || !is.numeric(x) || length(x) == 0) {
    NA
  } else {
    x[!is_na]
  }
}

