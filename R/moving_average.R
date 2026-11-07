# =============================================================================
# Moving Average - MA(N)            (Forecasting > Stationary series)
# Nahmias & Olsen Section 2.7; lecture slides 28-31
#   F_t = (D_{t-1} + D_{t-2} + ... + D_{t-N}) / N
# Multiple-step-ahead forecasts equal the one-step-ahead forecast.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------
fc_ma <- function(D, N) {
  n <- length(D)
  F <- rep(NA_real_, n)
  if (n > N) for (t in (N + 1):n) F[t] <- mean(D[(t - N):(t - 1)])
  list(F = F, next_value = mean(D[(n - N + 1):n]))
}

# UI ---------------------------------------------------------------------------
moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Moving Average"),
      fc_data_ui(ns, default_example = "Wallace Garden shed sales (Slides)"),
      hr(),
      radioButtons(ns("mode"), "How should N be chosen?",
                   choices = c("I will enter N" = "manual", "Find the best N" = "best")),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "N (number of past periods)", value = 3, min = 1, step = 1)
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try", value = 6, min = 2, step = 1),
        fc_criterion_ui(ns)
      ),
      hr(),
      fc_common_ui(ns)
    ),
    mainPanel(
      fc_results_ui(ns, method = tagList(
        p("The forecast is the arithmetic average of the N most recent observations:"),
        p("$$F_{t} = \\frac{1}{N}\\sum_{i=t-N}^{t-1} D_i$$"),
        p("Because the series is assumed stationary, the forecast for every future",
          "period equals the next-period forecast. Error: \\(e_t = F_t - D_t\\)."),
        p("Best N: every N from 1 to the largest N is evaluated over the same",
          "periods (those that have a forecast for the largest N), so the",
          "comparison is fair.")
      ))
    )
  )
}

# Server -----------------------------------------------------------------------
moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      h <- fc_int(input$h, 1)
      from <- fc_int(input$from, 1)
      notes <- character(0)
      search <- NULL

      if (identical(input$mode, "best")) {
        crit <- input$crit
        Nmax <- min(fc_int(input$Nmax, 6, 2), n - 2)
        validate(need(Nmax >= 1, "The series is too short to search for N."))
        Ns <- seq_len(Nmax)
        common_from <- max(from, Nmax + 1)
        scores <- t(vapply(Ns, function(N) {
          e <- fc_errors(D, fc_ma(D, N)$F, common_from)
          c(MAD = e$MAD, MSE = e$MSE, MAPE = e$MAPE)
        }, numeric(3)))
        tab <- data.frame(N = as.integer(Ns), scores, check.names = FALSE)
        N <- Ns[which.min(tab[[crit]])]
        from <- common_from
        notes <- c(notes, sprintf(
          "Best N = %d (minimum %s over periods %d to %d).", N, crit, common_from, n))
        search <- list(
          title = paste("Search for the best N (criterion:", crit, ")"),
          caption = sprintf("All N values are compared over periods %d to %d.", common_from, n),
          plot = function() fc_plot_search_bar(tab$N, tab[[crit]], N, "N", crit),
          table = tab
        )
      } else {
        N <- fc_int(input$N, 3)
        validate(need(N < n, sprintf("N must be smaller than the number of observations (%d).", n)))
      }

      fit <- fc_ma(D, N)
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))

      list(label = sprintf("MA(%d)", N), series_name = s$name,
           D = D, F = fit$F, future = rep(fit$next_value, h), from = from,
           extra = NULL, notes = notes, search = search)
    })

    fc_results_server(output, session, res, "moving_average")
  })
}
