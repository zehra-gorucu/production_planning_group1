# =============================================================================
# Seasonal Factors - "quick and dirty" method   (Forecasting > Stationary series)
# Nahmias & Olsen Section 2.9 (Example 2.6); lecture slides 55-60
# For a seasonal series WITHOUT trend, season length N:
#   1. Compute the sample mean of all data.
#   2. Divide each observation by the mean (a factor for every observation).
#   3. Average the factors of like periods -> c_1, ..., c_N  (they add up to N).
#   Forecast for a period in position i of the season = mean * c_i.
# The data must start with the first period of a season.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------
fc_seasonal_factors <- function(D, N) {
  m <- floor(length(D) / N)                 # number of complete seasons used
  used <- D[seq_len(m * N)]
  mean_all <- mean(used)
  ratios <- matrix(used / mean_all, nrow = N)   # rows: period of season, cols: season
  factors <- rowMeans(ratios)
  list(N = N, m = m, mean = mean_all, ratios = ratios, factors = factors)
}

fc_season_position <- function(t, N) ((t - 1) %% N) + 1

# UI ---------------------------------------------------------------------------
seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Seasonal Factors (quick and dirty)"),
      fc_data_ui(ns, default_example = "Bridge traffic, 4 weeks x 5 days (Book Example 2.6)"),
      hr(),
      radioButtons(ns("mode"), "How should the season length N be chosen?",
                   choices = c("I will enter N" = "manual",
                               "Suggest N from the autocorrelations" = "best")),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "Season length N (periods in one season)", value = 5, min = 2, step = 1),
        helpText("For example 4 for quarterly data, 12 for monthly data, 5 for working days.")
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        helpText("The lag k (2 ≤ k ≤ n/2) with the largest autocorrelation",
                 "r_k is suggested as the season length.")
      ),
      helpText("The first observation must be the first period of a season.",
               "At least two complete seasons are needed."),
      hr(),
      fc_common_ui(ns, horizon_default = NA,
                   horizon_label = "Number of future periods (empty = one season)")
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(
          h4("Seasonal factors"),
          div(style = "overflow-x: auto;", tableOutput(ns("factor_table"))),
          plotOutput(ns("factor_plot"), height = "260px")
        ),
        method = tagList(
          p("1. Compute the mean \\(\\bar{D}\\) of all observations (complete seasons)."),
          p("2. Divide every observation by the mean: \\(D_t / \\bar{D}\\)."),
          p("3. Average the ratios of like periods in each season to get",
            "\\(c_1, \\dots, c_N\\), with \\(\\sum_{i=1}^{N} c_i = N\\)."),
          p("Forecast for a period in position i of the season: \\(F = \\bar{D}\\, c_i\\).",
            "Deseasonalized series: \\(D_t / c_i\\)."),
          p("Errors are measured on the same data that was used to estimate the",
            "factors (in-sample fit).")
        ))
    )
  )
}

# Server -----------------------------------------------------------------------
seasonal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      from <- fc_int(input$from, 1)
      notes <- character(0)
      search <- NULL

      if (identical(input$mode, "best")) {
        suggestion <- fc_suggest_season_length(D)
        N <- suggestion$N
        r <- suggestion$acf
        notes <- c(notes, sprintf("Suggested season length N = %d (largest autocorrelation r_%d = %.3f).",
                                  N, N, r[N]))
        search <- list(
          title = "Autocorrelation function (used to suggest N)",
          caption = "Autocorrelation r_k for each lag k. The highlighted lag is used as N.",
          plot = function() {
            cols <- ifelse(seq_along(r) == N, "#D95F02", "#9DB4CF")
            barplot(r, names.arg = seq_along(r), col = cols, border = NA, las = 1,
                    xlab = "Lag k", ylab = "r_k", main = sprintf("Suggested N = %d", N))
            abline(h = 0, col = "grey40")
          },
          table = data.frame("Lag k" = seq_along(r), "r_k" = r, check.names = FALSE)
        )
      } else {
        N <- fc_int(input$N, 5, 2)
      }
      validate(need(n >= 2 * N, sprintf(
        "At least two complete seasons (2 x %d = %d observations) are needed; the series has %d.",
        N, 2 * N, n)))

      sf <- fc_seasonal_factors(D, N)
      if (n > sf$m * N) notes <- c(notes, sprintf(
        "Only the %d complete seasons (periods 1 to %d) were used to estimate the factors.",
        sf$m, sf$m * N))
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))

      h <- fc_int(input$h, N)
      pos <- fc_season_position(seq_len(n + h), N)
      c_t <- sf$factors[pos]
      F <- sf$mean * c_t[seq_len(n)]
      future <- sf$mean * c_t[n + seq_len(h)]

      list(label = sprintf("Seasonal factors (N = %d)", N), series_name = s$name,
           D = D, F = F, future = future, from = from,
           extra = data.frame("Position in season" = as.integer(pos),
                              "Seasonal factor (c)" = c_t,
                              "Deseasonalized (D / c)" = c(D / c_t[seq_len(n)], rep(NA, h)),
                              check.names = FALSE),
           notes = notes, search = search, sf = sf)
    })

    output$factor_table <- renderTable({
      sf <- res()$sf
      tab <- data.frame("Period of season" = as.character(seq_len(sf$N)), check.names = FALSE)
      for (j in seq_len(sf$m)) tab[[paste("Season", j, "(D / mean)")]] <- sf$ratios[, j]
      tab[["Seasonal factor c"]] <- sf$factors
      tab[["Forecast (mean x c)"]] <- sf$mean * sf$factors
      total <- as.list(rep(NA, ncol(tab)))
      names(total) <- names(tab)
      total[[1]] <- "Sum"
      total[["Seasonal factor c"]] <- sum(sf$factors)
      rbind(tab, as.data.frame(total, check.names = FALSE))
    }, digits = 4, na = "")

    output$factor_plot <- renderPlot({
      sf <- res()$sf
      barplot(sf$factors, names.arg = seq_len(sf$N), col = "#9DB4CF", border = NA, las = 1,
              xlab = "Period of season", ylab = "Seasonal factor",
              main = sprintf("Seasonal factors (mean = %s)", fc_fmt(sf$mean, 3)))
      abline(h = 1, lty = 2, col = "#D95F02")
    })

    fc_results_server(output, session, res, "seasonal_factors")
  })
}
