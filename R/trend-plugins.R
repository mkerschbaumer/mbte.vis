# extract fitting quosure from trend module
tm_extract_quo <- function(x, ...) {
  UseMethod("tm_extract_quo")
}

# implementation for default trend module
tm_extract_quo.dtm <- function(x, ...) {
  x$fit_quo
}

#' @importFrom purrr map
tm_extract_quos <- function(l) {
  map(l, tm_extract_quo)
}

# extract plugin
tm_extract_plugin <- function(x, ...) {
  UseMethod("tm_extract_plugin")
}

# implementation for default trend module
tm_extract_plugin.dtm <- function(x, ...) {
  x$plugin
}

#' @importFrom purrr map
tm_extract_plugins <- function(l) {
  map(l, tm_extract_plugin)
}

#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @importFrom shiny need reactive validate
#' @importFrom shiny.plugin plugin_extract_id plugin_extract_server plugin_new_default
wrap_plugin <- function(plugin, fits_r, metrics_r) {
  # ID / name of trend-fitting helper (e.g. 'lin' etc.)
  trend_name <- plugin_extract_id(plugin)

  # split fits (only keep fits for current trend fitting module)
  split_fits <- reactive({
    fits_tbl <- fits_r()
    req(fits_tbl)

    split_fits_impl(fits_tbl, trend_name)
  })

  # only keep metrics for used algorithm/trend fitting helper (e.g. 'lin')
  filtered_metrics <- reactive({
    filtered <- metrics_r() %>%
      filter(fit == !!trend_name)

    # assert, that at least 1 fit is present
    validate(need(nrow(filtered) != 0, "Filtering produces empty metrics-table."))
    filtered
  })

  # create wrapper sgda.plugin: use same ID but modified server function (pass
  # `split_fits` to original server function)
  #
  # NOTE: the wrapped plugin is only needed for the call to
  # plugin_create_server() in setup_trend_modules()
  plugin_new_default(
    plugin_extract_id(plugin),
    server = function(input, output, session) {
      server_fun <- plugin_extract_server(plugin)
      server_fun(input, output, session, fits = split_fits,
        metrics = filtered_metrics
      )
    }
  )
}

# NOTE: trend modules passed via ellipsis
#' @importFrom purrr invoke map
#' @importFrom rlang list2
#' @importFrom shiny getDefaultReactiveDomain reactiveValues
setup_tm_servers <- function(fits_r, metrics_r, ...) {
  # trend modules
  modules <- list2(...) %>%
    map(wrap_plugin, fits_r = fits_r, metrics_r = metrics_r)

  # use a custom namespace to avoid naming conflicts (e.g. a trend-module has
  # the same name as a reactive in the global namespace)
  session <- getDefaultReactiveDomain()
  plugin_create_server(!!!modules, session = session$makeScope("wrappers"))
}

#' @importFrom purrr as_mapper map
#' @importFrom rlang list2 new_environment
init_trend_modules <- function(...) {
  # interpret ellipsis as module-builder functions
  modules <- list2(...)
  modules <- map(modules, as_mapper)

  # create environment for storing coefficients
  coef_env <- new_environment()

  # invoke model-building functions with `coef_env`
  map(modules, ~.x(coef_env))
}

# Generate a shiny.plugin, which allows coefficient-based filtering (provided,
# that the corresponding fitting quosure stores fit-coefficients as a tibble in
# `coef_env`). This function is intended to generate the shiny.plugin for
# trend-modules.
#' @importFrom dplyr bind_cols filter select
#' @importFrom mbte mbte_reconstruct
#' @importFrom rlang is_expression is_scalar_character
#' @importFrom shiny callModule need NS p plotOutput reactive reactiveValues
#'   renderPlot renderUI req selectInput strong textInput uiOutput validate
#' @importFrom shiny.plugin plugin_new_default
#' @importFrom shinydashboard box
#' @importFrom tibble is_tibble
coef_filter_plugin <- function(id, coef_env, title = id,
                               selected_par = NULL, displayname = id) {
  stopifnot(is_scalar_character(id))

  plugin_new_default(
    id = id,
    ui = function(id) {
      ns <- NS(id)

      box(
        title = title,
        width = 4,
        uiOutput(ns("param_selector")),
        plotOutput(ns("plot")),
        expr_input_ui(ns("filter_expr"), label = "Filter expression"),
        uiOutput(ns("remaining_samples"))
      )
    },
    server = function(input, output, session, fits, metrics) {
      coefs <- reactive({ coef_env[[id]] })

      # combine fits with coefficients
      combined <- reactive({
        coefs <- coefs()
        fits <- fits()
        req(coefs, fits)

        stopifnot(is_tibble(coefs))
        fits %>%
          bind_cols(coefs) %>%
          mbte_reconstruct(fits)
      })

      output$param_selector <- renderUI({
        coefs <- coefs()
        req(coefs)
        selectInput(
          session$ns("to_display"), "Parameter to display",
          choices = colnames(coefs), selected = selected_par
        )
      })

      # filtering based on metrics provided by the user; filter/rearrange
      # according to signal_id present in both datasets (rearranging based on
      # computed metric)
      metric_filtered <- filter_rearrange_fits_rv(combined, metrics)

      # data for the histogram (selected parameter)
      plot_dataset <- reactive({
        metric_filtered <- metric_filtered()
        relative <- input$relative
        choice <- input$to_display
        req(metric_filtered, choice)
        metric_filtered[[choice]]
      })

      # draw histogram
      output$plot <- renderPlot({
        plot_dataset <- plot_dataset()
        req(plot_dataset)
        hist(plot_dataset)
      })

      # invoke server of plugin for filtering-expression
      filter_expr <- callModule(expr_input_server, "filter_expr")

      # perform filtering based on coefficients
      coefs_filtered <- reactive({
        filter_expr <- filter_expr()
        stopifnot(is_expression(filter_expr))

        metric_filtered <- metric_filtered()
        coefs <- coefs()
        req(metric_filtered, coefs)
        cols_to_drop <- colnames(coefs)
        tryCatch(
          metric_filtered %>%
            filter(!!filter_expr) %>% # use user-provided expression
            select(-!!cols_to_drop) %>% # remove coefficient-columns
            mbte_reconstruct(metric_filtered),
          error = function(e) {
            validate(
              need(FALSE, paste("error while evaluating filter expression", e))
            )
          }
        )
      })

      # show text indicating how many samples are selected (first metric-based-
      # and then coefficient-based filtering is performed)
      output$remaining_samples <- renderUI({
        coefs_filtered <- coefs_filtered()
        metric_filtered <- metric_filtered()
        total_fits <- nrow(metric_filtered)
        selected_fits <- nrow(coefs_filtered)
        p(strong(selected_fits), "/", total_fits, " fits selected.")
      })

      # return reactiveValues at the end
      rv <- reactiveValues()
      rv$filtered <- coefs_filtered

      rv
    },
    displayname = displayname,
    classes = "trend-filter"
  )
}
