# =============================================================================
# Simple Exponential Smoothing      (Forecasting > Stationary series)
# Nahmias & Olsen Section 2.7 (Example 2.3); lecture slides 39-44
#   F_t = alpha * D_{t-1} + (1 - alpha) * F_{t-1}  =  F_{t-1} - alpha * e_{t-1}
# The initial value F_1 = S_0 is needed to start the method.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------
fc_ses <- function(D, alpha, S0) {
  n <- length(D)
  F <- numeric(n)
  F[1] <- S0
  if (n > 1) for (t in 2:n) F[t] <- alpha * D[t - 1] + (1 - alpha) * F[t - 1]
  list(F = F, next_value = alpha * D[n] + (1 - alpha) * F[n])
}

# UI ---------------------------------------------------------------------------
exp_smoothing_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Simple Exponential Smoothing"),
      fc_data_ui(ns, default_example = "Engine failures (Book Example 2.2)"),
      hr(),
      radioButtons(ns("mode"), "How should α (alpha) be chosen?",
                   choices = c("I will enter α" = "manual", "Find the best α" = "best")),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "Smoothing constant α (0 < α ≤ 1)",
                     value = 0.1, min = 0.01, max = 1, step = 0.05),
        helpText("Values between 0.1 and 0.2 are recommended for production planning.")
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        fc_criterion_ui(ns),
        helpText("α = 0.01, 0.02, ..., 1.00 are tried.")
      ),
      hr(),
      radioButtons(ns("init"), "Initial value S₀ (= forecast for period 1)",
                   choices = c("Fit a regression line" = "regression",
                               "Use the first observation" = "first",
                               "Enter manually" = "manual")),
      conditionalPanel(
        "input.init == 'regression'", ns = ns,
        numericInput(ns("k"), "Regression baseline: first k periods (empty = all data)",
                     value = NA, min = 2, step = 1)
      ),
      conditionalPanel(
        "input.init == 'manual'", ns = ns,
        numericInput(ns("S0"), "Initial level S₀", value = 200)
      ),
      hr(),
      fc_common_ui(ns)
    ),
    mainPanel(
      fc_results_ui(ns, method = tagList(
        p("New forecast = α × (latest observation) + (1 − α) × (last forecast):"),
        p("$$F_{t} = \\alpha D_{t-1} + (1-\\alpha) F_{t-1} = F_{t-1} - \\alpha\\, e_{t-1}$$"),
        p("The method applies exponentially declining weights",
          "\\(\\alpha(1-\\alpha)^i\\) to past data. Multiple-step-ahead forecasts",
          "equal the one-step-ahead forecast."),
        p("Regression initial value: a line \\(D_t = a + b\\,t\\) is fitted to the",
          "first k periods by least squares and \\(S_0 = a\\) (the level at time 0).")
      ))
    )
  )
}

# Server -----------------------------------------------------------------------
exp_smoothing_server <- function(id) {
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

      # Initial value
      if (identical(input$init, "regression")) {
        reg <- fc_regression(D, fc_int(input$k, n, 2))
        S0 <- reg$a
        notes <- c(notes, sprintf(
          "Initial value from regression on periods 1 to %d: D = %s + %s t, so S0 = %s.",
          reg$k, fc_fmt(reg$a), fc_fmt(reg$b), fc_fmt(S0)))
      } else if (identical(input$init, "first")) {
        S0 <- D[1]
        notes <- c(notes, sprintf("Initial value S0 = D1 = %s.", fc_fmt(S0)))
      } else {
        validate(need(!is.na(input$S0), "Please enter the initial level S0."))
        S0 <- input$S0
      }

      # Smoothing constant
      if (identical(input$mode, "best")) {
        crit <- input$crit
        alphas <- seq(0.01, 1, by = 0.01)
        scores <- t(vapply(alphas, function(a) {
          e <- fc_errors(D, fc_ses(D, a, S0)$F, from)
          c(MAD = e$MAD, MSE = e$MSE, MAPE = e$MAPE)
        }, numeric(3)))
        validate(need(any(!is.na(scores[, crit])), "No periods left to evaluate the errors."))
        alpha <- alphas[which.min(scores[, crit])]
        tab <- data.frame(alpha = alphas, scores, check.names = FALSE)
        top <- tab[order(tab[[crit]]), ][1:10, ]
        names(top)[1] <- "α"
        notes <- c(notes, sprintf("Best α = %.2f (minimum %s).", alpha, crit))
        search <- list(
          title = paste("Search for the best α (criterion:", crit, ")"),
          caption = "The 10 best values of α:",
          plot = function() fc_plot_search_line(tab$alpha, tab[[crit]], alpha, "alpha", crit),
          table = top
        )
      } else {
        alpha <- input$alpha
        validate(need(!is.na(alpha) && alpha > 0 && alpha <= 1, "α must be between 0 and 1."))
      }

      fit <- fc_ses(D, alpha, S0)
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))

      list(label = sprintf("SES (α = %.2f)", alpha), series_name = s$name,
           D = D, F = fit$F, future = rep(fit$next_value, h), from = from,
           extra = NULL, notes = notes, search = search)
    })

    fc_results_server(output, session, res, "exponential_smoothing")
  })
}
