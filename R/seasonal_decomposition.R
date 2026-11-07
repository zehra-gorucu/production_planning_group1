# =============================================================================
# Classical Seasonal Decomposition with Moving Averages
# Multiplicative model: D_t = T_t * C_t * I_t
# Additive model:       D_t = T_t + C_t + I_t
#
# A centred moving average estimates the trend-cycle.  Ratios (or differences)
# at like positions in the season are averaged and normalized to obtain the
# seasonal figures.  A least-squares line fitted to the deseasonalized series is
# then extrapolated and reseasonalized to produce forecasts.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------

fc_centered_moving_average <- function(D, N) {
  N <- as.integer(N)
  if (N < 2) stop("Season length N must be at least 2.")
  if (length(D) < 2 * N) stop("At least two complete seasons are needed.")

  # An even season needs a 2 x N centred moving average.  These are the same
  # default weights used by stats::decompose().
  weights <- if (N %% 2 == 0) {
    c(0.5, rep(1, N - 1), 0.5) / N
  } else {
    rep(1 / N, N)
  }
  as.numeric(stats::filter(D, filter = weights, sides = 2))
}

fc_seasonal_decomposition <- function(D, N,
                                      type = c("multiplicative", "additive")) {
  type <- match.arg(type)
  D <- as.numeric(D)
  N <- as.integer(N)
  n <- length(D)
  if (N < 2) stop("Season length N must be at least 2.")
  if (n < 2 * N) stop("At least two complete seasons are needed.")
  if (type == "multiplicative" && any(D <= 0)) {
    stop("The multiplicative model requires strictly positive observations.")
  }

  moving_average <- fc_centered_moving_average(D, N)
  position <- ((seq_len(n) - 1L) %% N) + 1L
  raw_seasonal <- if (type == "multiplicative") {
    D / moving_average
  } else {
    D - moving_average
  }
  raw_seasonal[!is.finite(raw_seasonal)] <- NA_real_

  seasonal_figure <- vapply(seq_len(N), function(i) {
    values <- raw_seasonal[position == i]
    values <- values[is.finite(values)]
    if (length(values) == 0) NA_real_ else mean(values)
  }, numeric(1))
  if (anyNA(seasonal_figure)) {
    stop("The series does not provide a centred moving average for every seasonal position.")
  }

  seasonal_figure <- if (type == "multiplicative") {
    seasonal_figure / mean(seasonal_figure)
  } else {
    seasonal_figure - mean(seasonal_figure)
  }
  seasonal <- seasonal_figure[position]
  deseasonalized <- if (type == "multiplicative") D / seasonal else D - seasonal
  regression <- fc_regression(deseasonalized)
  trend <- regression$a + regression$b * seq_len(n)
  fitted <- if (type == "multiplicative") trend * seasonal else trend + seasonal
  irregular <- if (type == "multiplicative") {
    D / (moving_average * seasonal)
  } else {
    D - moving_average - seasonal
  }
  irregular[!is.finite(irregular)] <- NA_real_

  list(
    N = N, type = type, moving_average = moving_average,
    raw_seasonal = raw_seasonal, factors = seasonal_figure,
    seasonal = seasonal, deseasonalized = deseasonalized,
    regression = regression, trend = trend, fitted = fitted,
    irregular = irregular
  )
}

fc_decomposition_forecast <- function(fit, n, h) {
  future_t <- n + seq_len(h)
  future_position <- ((future_t - 1L) %% fit$N) + 1L
  future_trend <- fit$regression$a + fit$regression$b * future_t
  if (fit$type == "multiplicative") {
    future_trend * fit$factors[future_position]
  } else {
    future_trend + fit$factors[future_position]
  }
}

# UI ---------------------------------------------------------------------------

seasonal_decomposition_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Seasonal Decomposition with Moving Averages"),
      fc_data_ui(ns, default_example = "Turner Industries quarterly sales (Slides)"),
      hr(),
      radioButtons(
        ns("type"), "Seasonal model",
        choices = c("Multiplicative (D = trend x seasonal x irregular)" = "multiplicative",
                    "Additive (D = trend + seasonal + irregular)" = "additive")
      ),
      radioButtons(
        ns("mode"), "How should the season length N be chosen?",
        choices = c("I will enter N" = "manual",
                    "Suggest N from the autocorrelations" = "best")
      ),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "Season length N", value = 4, min = 2, step = 1),
        helpText("For example: 4 for quarterly data, 12 for monthly data, 5 for working days.")
      ),
      helpText("The first observation must be the first position of a season.",
               "At least two complete seasons are needed."),
      hr(),
      fc_common_ui(ns, horizon_default = NA,
                   horizon_label = "Number of future periods (empty = one season)")
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(
          h4("Decomposition diagnostics"),
          plotOutput(ns("component_plot"), height = "560px"),
          h4("Seasonal figures"),
          div(style = "overflow-x: auto;", tableOutput(ns("factor_table")))
        ),
        method = tagList(
          p("1. Estimate the trend-cycle with a centred moving average."),
          p("2. Remove the moving average from each observation by division",
            "(multiplicative) or subtraction (additive)."),
          p("3. Average like seasonal positions and normalize the figures so that",
            "their mean is 1 (multiplicative) or 0 (additive)."),
          p("4. Deseasonalize the observations and fit the least-squares line",
            "\\(T_t = a + b t\\)."),
          p("5. Forecast by reseasonalizing the extrapolated trend:",
            "\\(F_t = T_t C_t\\) or \\(F_t = T_t + C_t\\).")
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

seasonal_decomposition_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      notes <- character(0)
      search <- NULL

      if (identical(input$mode, "best")) {
        suggestion <- fc_suggest_season_length(D)
        N <- suggestion$N
        r <- suggestion$acf
        notes <- c(notes, sprintf(
          "Suggested season length N = %d (largest autocorrelation r_%d = %.3f).",
          N, N, r[N]
        ))
        search <- list(
          title = "Autocorrelation function (used to suggest N)",
          caption = "The highlighted lag is used as the season length.",
          plot = function() {
            cols <- ifelse(seq_along(r) == N, "#D95F02", "#9DB4CF")
            barplot(r, names.arg = seq_along(r), col = cols, border = NA, las = 1,
                    xlab = "Lag k", ylab = "r_k", main = sprintf("Suggested N = %d", N))
            abline(h = 0, col = "grey40")
          },
          table = data.frame("Lag k" = seq_along(r), "r_k" = r, check.names = FALSE)
        )
      } else {
        N <- fc_int(input$N, 4, 2)
      }

      validate(need(n >= 2 * N, sprintf(
        "At least two complete seasons (2 x %d = %d observations) are needed; the series has %d.",
        N, 2 * N, n
      )))
      validate(need(input$type != "multiplicative" || all(D > 0),
                    "The multiplicative model requires strictly positive observations."))

      fit <- tryCatch(
        fc_seasonal_decomposition(D, N, input$type),
        error = function(e) validate(need(FALSE, conditionMessage(e)))
      )
      h <- fc_int(input$h, N)
      from <- fc_int(input$from, 1)
      future <- fc_decomposition_forecast(fit, n, h)
      all_t <- seq_len(n + h)
      all_position <- ((all_t - 1L) %% N) + 1L
      future_trend <- fit$regression$a + fit$regression$b * (n + seq_len(h))
      all_trend <- c(fit$trend, future_trend)
      all_seasonal <- fit$factors[all_position]

      notes <- c(notes, sprintf(
        "Deseasonalized trend: D = %s + %s t.",
        fc_fmt(fit$regression$a, 3), fc_fmt(fit$regression$b, 3)
      ))
      if (n %% N != 0) notes <- c(notes, sprintf(
        "The final %d observation(s) form an incomplete season; available centred ratios were still used.",
        n %% N
      ))
      if (s$dropped > 0) notes <- c(notes, sprintf(
        "%d empty / non-numeric cells were ignored.", s$dropped
      ))

      raw_label <- if (fit$type == "multiplicative") {
        "Raw seasonal ratio (D / moving average)"
      } else {
        "Raw seasonal difference (D - moving average)"
      }
      irregular_label <- if (fit$type == "multiplicative") {
        "Irregular (D / moving average / seasonal)"
      } else {
        "Irregular (D - moving average - seasonal)"
      }
      extra <- data.frame(
        "Position in season" = all_position,
        "Centred moving average" = c(fit$moving_average, rep(NA, h)),
        "placeholder raw" = c(fit$raw_seasonal, rep(NA, h)),
        "Seasonal figure" = all_seasonal,
        "Deseasonalized" = c(fit$deseasonalized, rep(NA, h)),
        "Linear trend" = all_trend,
        "placeholder irregular" = c(fit$irregular, rep(NA, h)),
        check.names = FALSE
      )
      names(extra)[3] <- raw_label
      names(extra)[7] <- irregular_label

      list(
        label = sprintf("Seasonal decomposition (%s, N = %d)", fit$type, N),
        series_name = s$name, D = D, F = fit$fitted, future = future,
        from = from, extra = extra, notes = notes, search = search, fit = fit
      )
    })

    output$factor_table <- renderTable({
      fit <- res()$fit
      n <- length(fit$raw_seasonal)
      seasons <- ceiling(n / fit$N)
      padded <- c(fit$raw_seasonal, rep(NA, seasons * fit$N - n))
      raw_matrix <- matrix(padded, nrow = fit$N)
      tab <- data.frame("Position in season" = seq_len(fit$N), check.names = FALSE)
      raw_name <- if (fit$type == "multiplicative") "ratio" else "difference"
      for (j in seq_len(seasons)) {
        tab[[sprintf("Season %d %s", j, raw_name)]] <- raw_matrix[, j]
      }
      tab[["Normalized seasonal figure"]] <- fit$factors
      total <- as.list(rep(NA, ncol(tab)))
      names(total) <- names(tab)
      total[[1]] <- if (fit$type == "multiplicative") "Sum (must equal N)" else "Sum (must equal 0)"
      total[["Normalized seasonal figure"]] <- sum(fit$factors)
      rbind(tab, as.data.frame(total, check.names = FALSE))
    }, digits = 4, na = "")

    output$component_plot <- renderPlot({
      r <- res()
      fit <- r$fit
      n <- length(r$D)
      old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
      on.exit(par(old_par), add = TRUE)

      plot(seq_len(n), r$D, type = "o", pch = 19, col = "#3B3B3B",
           xlab = "Period", ylab = r$series_name, main = "Observed and moving average")
      lines(seq_len(n), fit$moving_average, lwd = 2, col = "#2C7BB6")
      legend("topleft", bty = "n", legend = c("Observed", "Centred MA"),
             col = c("#3B3B3B", "#2C7BB6"), pch = c(19, NA), lty = c(1, 1))

      plot(seq_len(n), fit$raw_seasonal, type = "o", pch = 19, col = "#6A8E3A",
           xlab = "Period", ylab = if (fit$type == "multiplicative") "Ratio" else "Difference",
           main = "Raw seasonal estimates")
      abline(h = if (fit$type == "multiplicative") 1 else 0, lty = 2, col = "grey40")

      plot(seq_len(n), fit$deseasonalized, type = "o", pch = 19, col = "#3B3B3B",
           xlab = "Period", ylab = "Deseasonalized", main = "Deseasonalized series and trend")
      lines(seq_len(n), fit$trend, lwd = 2, col = "#D95F02")

      barplot(fit$factors, names.arg = seq_len(fit$N), col = "#9DB4CF", border = NA,
              xlab = "Position in season", ylab = "Seasonal figure", main = "Seasonal figures")
      abline(h = if (fit$type == "multiplicative") 1 else 0, lty = 2, col = "#D95F02")
    })

    fc_results_server(output, session, res, "seasonal_decomposition")
  })
}
