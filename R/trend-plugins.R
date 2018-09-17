# extract fitting quosure from trend module
tm_extract_quo <- function(x, ...) {
  UseMethod("tm_extract_quo")
}

# implementation for default trend module
tm_extract_quo.dtm <- function(x, ...) {
  x$fit_quo
}

# extract plugin
tm_extract_plugin <- function(x, ...) {
  UseMethod("tm_extract_plugin")
}

# implementation for default trend module
tm_extract_plugin.dtm <- function(x, ...) {
  x$plugin
}
