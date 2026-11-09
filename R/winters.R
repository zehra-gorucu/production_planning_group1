# =============================================================================
# Winters' Method (triple exponential smoothing)
#
# Multiplicative:
#   L_t = alpha * (D_t / C_{t-N}) + (1-alpha) * (L_{t-1} + B_{t-1})
#   B_t = beta  * (L_t - L_{t-1}) + (1-beta) * B_{t-1}
#   C_t = gamma * (D_t / L_t) + (1-gamma) * C_{t-N}
#   F_{t+h} = (L_t + h B_t) * C_{t-N+h}
#
# Additive replaces division/multiplication with subtraction/addition.
# Initial values come from a moving-average decomposition of complete seasons.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------

fc_winters_initial <- function(D, N, type = c("multiplicative", "additive"),
                               seasons = 2) {
  type <- match.arg(type)
  N <- as.integer(N)
  seasons <- max(2L, as.integer(seasons))
  k <- min(floor(length(D) / N), seasons) * N
  if (k < 2 * N) stop("At least two complete seasons are needed for initialization.")
  fit <- fc_seasonal_decomposition(D[seq_len(k)], N, type)
  regression <- fc_regression(fit$deseasonalized)
  list(
    period = k,
    level = regression$a + regression$b * k,
    trend = regression$b,
    seasonal = fit$factors,
    regression = regression
  )
}

# Winters's own initialization (Nahmias Section 2.9, Example 2.8).
#   V_i = mean of season i,  G0 = (V_m - V_1) / ((m - 1) N),
#   S0  = V_m + G0 (N - 1) / 2   (value of the line through the V's at the end
#                                 of the initialization data),
#   c_t = D_t / (V_i - ((N + 1) / 2 - j) G0) for period j of season i,
#   averaged over like periods and normed to add up to N.
fc_winters_initial_book <- function(D, N, type = c("multiplicative", "additive"),
                                    seasons = 2) {
  type <- match.arg(type)
  N <- as.integer(N)
  m <- min(floor(length(D) / N), max(2L, as.integer(seasons)))
  if (m < 2) stop("At least two complete seasons are needed for initialization.")
  k <- m * N
  M <- matrix(D[seq_len(k)], nrow = N)          # rows: period j, columns: season i
  V <- colMeans(M)
  G0 <- (V[m] - V[1]) / ((m - 1) * N)
  S0 <- V[m] + G0 * (N - 1) / 2
  line <- outer(seq_len(N), seq_len(m), function(j, i) V[i] - ((N + 1) / 2 - j) * G0)
  raw <- if (type == "multiplicative") M / line else M - line
  c0 <- rowMeans(raw)
  c0 <- if (type == "multiplicative") c0 * N / sum(c0) else c0 - mean(c0)
  list(period = k, level = S0, trend = G0, seasonal = c0, season_means = V,
       regression = list(a = S0 - G0 * k, b = G0))
}

fc_winters <- function(D, N, alpha, beta, gamma, initial, h = 1,
                       type = c("multiplicative", "additive")) {
  type <- match.arg(type)
  D <- as.numeric(D)
  N <- as.integer(N)
  n <- length(D)
  k <- as.integer(initial$period)
  if (k < 2 * N || k >= n) {
    stop("The initialization period must contain at least two seasons and leave observations to fit.")
  }
  if (length(initial$seasonal) != N) stop("Exactly N initial seasonal figures are required.")
  if (type == "multiplicative" && any(D <= 0)) {
    stop("The multiplicative model requires strictly positive observations.")
  }
  pars <- c(alpha, beta, gamma)
  if (any(!is.finite(pars)) || any(pars <= 0) || any(pars > 1)) {
    stop("alpha, beta and gamma must be greater than 0 and at most 1.")
  }

  position <- ((seq_len(n) - 1L) %% N) + 1L
  level <- rep(NA_real_, n)
  trend <- rep(NA_real_, n)
  seasonal <- rep(NA_real_, n)
  forecast <- rep(NA_real_, n)

  # Show the initialization estimates in the result table.  Forecast errors
  # begin only after period k, so these values never count as predictions.
  init_t <- seq_len(k)
  level[init_t] <- initial$regression$a + initial$regression$b * init_t
  trend[init_t] <- initial$trend
  seasonal[init_t] <- initial$seasonal[position[init_t]]

  level_prev <- initial$level
  trend_prev <- initial$trend
  latest_seasonal <- as.numeric(initial$seasonal)

  for (t in (k + 1L):n) {
    pos <- position[t]
    old_seasonal <- latest_seasonal[pos]
    if (!is.finite(old_seasonal) ||
        (type == "multiplicative" && abs(old_seasonal) < .Machine$double.eps)) {
      stop("A seasonal figure became zero or non-finite.")
    }

    if (type == "multiplicative") {
      forecast[t] <- (level_prev + trend_prev) * old_seasonal
      level[t] <- alpha * (D[t] / old_seasonal) +
        (1 - alpha) * (level_prev + trend_prev)
    } else {
      forecast[t] <- level_prev + trend_prev + old_seasonal
      level[t] <- alpha * (D[t] - old_seasonal) +
        (1 - alpha) * (level_prev + trend_prev)
    }
    trend[t] <- beta * (level[t] - level_prev) + (1 - beta) * trend_prev
    if (type == "multiplicative") {
      if (!is.finite(level[t]) || abs(level[t]) < .Machine$double.eps) {
        stop("The level became zero or non-finite in the multiplicative model.")
      }
      seasonal[t] <- gamma * (D[t] / level[t]) + (1 - gamma) * old_seasonal
    } else {
      seasonal[t] <- gamma * (D[t] - level[t]) + (1 - gamma) * old_seasonal
    }
    latest_seasonal[pos] <- seasonal[t]
    level_prev <- level[t]
    trend_prev <- trend[t]
  }

  future_step <- seq_len(h)
  future_position <- ((n + future_step - 1L) %% N) + 1L
  future <- if (type == "multiplicative") {
    (level_prev + future_step * trend_prev) * latest_seasonal[future_position]
  } else {
    level_prev + future_step * trend_prev + latest_seasonal[future_position]
  }

  list(
    level = level, trend = trend, seasonal = seasonal, F = forecast,
    future = future, latest_seasonal = latest_seasonal,
    start = k + 1L, initial = initial
  )
}

fc_winters_search <- function(D, N, initial, from, criterion,
                              type = c("multiplicative", "additive")) {
  type <- match.arg(type)
  score <- function(alpha, beta, gamma) {
    value <- tryCatch({
      fit <- fc_winters(D, N, alpha, beta, gamma, initial, h = 1, type = type)
      fc_errors(D, fit$F, from)[[criterion]]
    }, error = function(e) NA_real_)
    if (is.na(value) || !is.finite(value)) Inf else value
  }

  evaluate_grid <- function(values) {
    grid <- expand.grid(alpha = values, beta = values, gamma = values)
    grid$value <- mapply(score, grid$alpha, grid$beta, grid$gamma)
    grid
  }

  coarse_values <- seq(0.1, 1, by = 0.1)
  coarse <- evaluate_grid(coarse_values)
  if (!any(is.finite(coarse$value))) stop("No parameter combination produced a valid error measure.")
  coarse_best <- coarse[which.min(coarse$value), ]

  fine_values <- function(x) {
    unique(pmin(1, pmax(0.01, round(x + seq(-0.1, 0.1, by = 0.02), 2))))
  }
  fine <- expand.grid(
    alpha = fine_values(coarse_best$alpha),
    beta = fine_values(coarse_best$beta),
    gamma = fine_values(coarse_best$gamma)
  )
  fine$value <- mapply(score, fine$alpha, fine$beta, fine$gamma)
  if (!any(is.finite(fine$value))) stop("The fine parameter search did not produce a valid model.")
  fine <- fine[order(fine$value), ]
  best <- fine[1, ]

  heat <- stats::aggregate(value ~ alpha + beta, data = coarse, FUN = min)
  list(
    alpha = best$alpha, beta = best$beta, gamma = best$gamma,
    coarse = coarse, heat = heat, fine = fine
  )
}

# UI ---------------------------------------------------------------------------

winters_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Winters' Method"),
      fc_data_ui(ns, default_example = "Turner Industries quarterly sales (Slides)"),
      hr(),
      radioButtons(
        ns("type"), "Seasonal model",
        choices = c("Multiplicative" = "multiplicative", "Additive" = "additive")
      ),
      radioButtons(
        ns("season_mode"), "How should the season length N be chosen?",
        choices = c("I will enter N" = "manual",
                    "Suggest N from the autocorrelations" = "best")
      ),
      conditionalPanel(
        "input.season_mode == 'manual'", ns = ns,
        numericInput(ns("N"), "Season length N", value = 4, min = 2, step = 1)
      ),
      hr(),
      radioButtons(
        ns("parameter_mode"), "How should alpha, beta and gamma be chosen?",
        choices = c("I will enter them" = "manual", "Find the best values" = "best")
      ),
      conditionalPanel(
        "input.parameter_mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "alpha (level)", value = 0.2, min = 0.01, max = 1, step = 0.05),
        numericInput(ns("beta"), "beta (trend)", value = 0.1, min = 0.01, max = 1, step = 0.05),
        numericInput(ns("gamma"), "gamma (seasonality)", value = 0.1, min = 0.01, max = 1, step = 0.05)
      ),
      conditionalPanel(
        "input.parameter_mode == 'best'", ns = ns,
        fc_criterion_ui(ns),
        helpText("A 0.10 grid is searched first, followed by a 0.02 grid around the best point.")
      ),
      hr(),
      numericInput(ns("init_seasons"), "Complete seasons used for initialization",
                   value = 2, min = 2, step = 1),
      radioButtons(
        ns("init"), "Initial level and trend",
        choices = c("Regression on the deseasonalized initialization data" = "regression",
                    "Winters's method: season averages V1, V2 (book, Example 2.8)" = "book",
                    "Enter level and trend at the end of initialization" = "manual")
      ),
      conditionalPanel(
        "input.init == 'manual'", ns = ns,
        numericInput(ns("level0"), "Initial level at the end of initialization", value = 140),
        numericInput(ns("trend0"), "Initial trend per period", value = 3)
      ),
      helpText("Regression and manual options estimate the initial seasonal figures by",
               "moving-average decomposition. Winters's method divides each observation by",
               "the line through the season averages. At least one observation must remain",
               "after initialization."),
      hr(),
      fc_common_ui(ns, horizon_default = NA,
                   horizon_label = "Number of future periods (empty = one season)")
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(
          h4("Initialization"),
          tableOutput(ns("init_table")),
          div(style = "overflow-x: auto;", tableOutput(ns("init_seasonal_table"))),
          h4("Smoothed states"),
          plotOutput(ns("state_plot"), height = "420px")
        ),
        method = tagList(
          p("Winters' method updates a level, a trend and one seasonal figure for",
            "each position of the season."),
          p("For the multiplicative model:"),
          p("$$L_t = \\alpha(D_t/C_{t-N}) + (1-\\alpha)(L_{t-1}+B_{t-1})$$"),
          p("$$B_t = \\beta(L_t-L_{t-1}) + (1-\\beta)B_{t-1}$$"),
          p("$$C_t = \\gamma(D_t/L_t) + (1-\\gamma)C_{t-N}$$"),
          p("$$F_{t+h} = (L_t+hB_t)C_{t-N+h}$$"),
          p("The additive model uses subtraction when removing seasonality and addition",
            "when reseasonalizing. Forecast errors begin after the initialization period.")
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

winters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      notes <- character(0)
      search <- NULL

      if (identical(input$season_mode, "best")) {
        suggestion <- fc_suggest_season_length(D)
        N <- suggestion$N
        r <- suggestion$acf
        notes <- c(notes, sprintf(
          "Suggested season length N = %d (largest autocorrelation r_%d = %.3f).",
          N, N, r[N]
        ))
      } else {
        N <- fc_int(input$N, 4, 2)
      }
      validate(need(input$type != "multiplicative" || all(D > 0),
                    "The multiplicative model requires strictly positive observations."))
      validate(need(n >= 2 * N + 1, sprintf(
        "At least two complete seasons plus one fitting observation are needed (minimum %d; series has %d).",
        2 * N + 1, n
      )))

      init_seasons <- fc_int(input$init_seasons, 2, 2)
      complete_seasons <- floor(n / N)
      validate(need(init_seasons < complete_seasons || init_seasons * N < n,
                    "Initialization uses all observations. Reduce the number of initialization seasons."))
      initial <- tryCatch(
        if (identical(input$init, "book")) {
          fc_winters_initial_book(D, N, input$type, init_seasons)
        } else {
          fc_winters_initial(D, N, input$type, init_seasons)
        },
        error = function(e) validate(need(FALSE, conditionMessage(e)))
      )
      if (identical(input$init, "book")) {
        init_source <- sprintf("Winters's method: season means %s, G0 = (V_m - V_1) / ((m - 1) N), S0 = V_m + G0 (N - 1) / 2",
                               paste(fc_fmt(initial$season_means, 2), collapse = ", "))
      } else if (identical(input$init, "manual")) {
        validate(need(is.finite(input$level0) && is.finite(input$trend0),
                      "Please enter finite initial level and trend values."))
        initial$level <- input$level0
        initial$trend <- input$trend0
        initial$regression$a <- initial$level - initial$trend * initial$period
        initial$regression$b <- initial$trend
        init_source <- "Entered manually at the end of initialization"
      } else {
        init_source <- "Regression on deseasonalized initialization data"
      }

      h <- fc_int(input$h, N)
      from <- fc_int(input$from, 1)
      if (identical(input$parameter_mode, "best")) {
        criterion <- input$crit
        sr <- tryCatch(
          fc_winters_search(D, N, initial, from, criterion, input$type),
          error = function(e) validate(need(FALSE, conditionMessage(e)))
        )
        alpha <- sr$alpha
        beta <- sr$beta
        gamma <- sr$gamma
        top <- head(sr$fine[is.finite(sr$fine$value), ], 10)
        names(top) <- c("alpha", "beta", "gamma", criterion)
        notes <- c(notes, sprintf(
          "Best alpha = %.2f, beta = %.2f, gamma = %.2f (minimum %s).",
          alpha, beta, gamma, criterion
        ))
        search <- list(
          title = paste("Search for alpha, beta and gamma (criterion:", criterion, ")"),
          caption = paste(
            "The heat map shows the smallest coarse-grid error over gamma for each",
            "(alpha, beta) pair. The table lists the 10 best fine-grid combinations."
          ),
          plot = function() {
            values <- sort(unique(sr$heat$alpha))
            Z <- matrix(NA_real_, nrow = length(values), ncol = length(values))
            for (i in seq_len(nrow(sr$heat))) {
              ai <- match(sr$heat$alpha[i], values)
              bi <- match(sr$heat$beta[i], values)
              Z[ai, bi] <- sr$heat$value[i]
            }
            image(values, values, Z, col = hcl.colors(40, "YlOrRd", rev = TRUE),
                  xlab = "alpha", ylab = "beta", las = 1,
                  main = sprintf("Minimum %s over gamma", criterion))
            contour(values, values, Z, add = TRUE, col = "grey30", labcex = 0.7)
            points(alpha, beta, pch = 19, cex = 1.8, col = "#2C7BB6")
          },
          table = top
        )
      } else {
        alpha <- input$alpha
        beta <- input$beta
        gamma <- input$gamma
        validate(need(
          all(is.finite(c(alpha, beta, gamma))) &&
            all(c(alpha, beta, gamma) > 0) && all(c(alpha, beta, gamma) <= 1),
          "alpha, beta and gamma must be greater than 0 and at most 1."
        ))
      }

      fit <- tryCatch(
        fc_winters(D, N, alpha, beta, gamma, initial, h, input$type),
        error = function(e) validate(need(FALSE, conditionMessage(e)))
      )
      if (s$dropped > 0) notes <- c(notes, sprintf(
        "%d empty / non-numeric cells were ignored.", s$dropped
      ))
      notes <- c(notes, sprintf(
        "Periods 1 to %d initialize the model; one-step forecast errors start at period %d.",
        initial$period, fit$start
      ))

      all_t <- seq_len(n + h)
      all_position <- ((all_t - 1L) %% N) + 1L
      future_seasonal <- fit$latest_seasonal[all_position[n + seq_len(h)]]
      deseasonalized <- if (input$type == "multiplicative") {
        D / fit$seasonal
      } else {
        D - fit$seasonal
      }
      extra <- data.frame(
        "Position in season" = all_position,
        "Level (L)" = c(fit$level, rep(NA, h)),
        "Trend (B)" = c(fit$trend, rep(NA, h)),
        "Seasonal (C)" = c(fit$seasonal, future_seasonal),
        "Deseasonalized" = c(deseasonalized, rep(NA, h)),
        check.names = FALSE
      )

      list(
        label = sprintf(
          "Winters %s (N = %d, alpha = %.2f, beta = %.2f, gamma = %.2f)",
          input$type, N, alpha, beta, gamma
        ),
        series_name = s$name, D = D, F = fit$F, future = fit$future,
        from = from, extra = extra, notes = notes, search = search,
        fit = fit, initial = initial, init_source = init_source, N = N,
        alpha = alpha, beta = beta, gamma = gamma, type = input$type
      )
    })

    output$init_table <- renderTable({
      r <- res()
      data.frame(
        Source = r$init_source,
        "Last initialization period" = r$initial$period,
        "Initial level" = r$initial$level,
        "Initial trend" = r$initial$trend,
        check.names = FALSE
      )
    }, digits = 4)

    output$init_seasonal_table <- renderTable({
      r <- res()
      data.frame(
        "Position in season" = seq_len(r$N),
        "Initial seasonal figure" = r$initial$seasonal,
        "Latest seasonal figure" = r$fit$latest_seasonal,
        check.names = FALSE
      )
    }, digits = 4)

    output$state_plot <- renderPlot({
      r <- res()
      fit <- r$fit
      n <- length(r$D)
      old_par <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
      on.exit(par(old_par), add = TRUE)

      plot(seq_len(n), fit$level, type = "l", lwd = 2, col = "#2C7BB6",
           xlab = "Period", ylab = "State", main = "Level and trend states")
      lines(seq_len(n), fit$trend, lwd = 2, col = "#D95F02")
      abline(v = r$initial$period + 0.5, lty = 3, col = "grey40")
      legend("topleft", bty = "n", legend = c("Level", "Trend"),
             col = c("#2C7BB6", "#D95F02"), lty = 1, lwd = 2)

      plot(seq_len(n), fit$seasonal, type = "o", pch = 19, col = "#6A8E3A",
           xlab = "Period", ylab = "Seasonal state", main = "Seasonal state by period")
      abline(h = if (r$type == "multiplicative") 1 else 0, lty = 2, col = "grey40")
      abline(v = r$initial$period + 0.5, lty = 3, col = "grey40")
    })

    fc_results_server(output, session, res, "winters")
  })
}
