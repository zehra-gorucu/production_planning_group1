# =============================================================================
# Holt's Method (double exponential smoothing)   (Forecasting > Trend series)
# Nahmias & Olsen Section 2.8; lecture slides 47-50
#   S_t = alpha * D_t + (1 - alpha) * (S_{t-1} + G_{t-1})      (intercept / level)
#   G_t = beta * (S_t - S_{t-1}) + (1 - beta) * G_{t-1}         (slope / trend)
#   F_{t, t+tau} = S_t + tau * G_t   ->  one-step-ahead F_t = S_{t-1} + G_{t-1}
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------
fc_holt <- function(D, alpha, beta, S0, G0, h = 1) {
  n <- length(D)
  S <- numeric(n)
  G <- numeric(n)
  F <- numeric(n)
  S_prev <- S0
  G_prev <- G0
  for (t in seq_len(n)) {
    F[t] <- S_prev + G_prev
    S[t] <- alpha * D[t] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    S_prev <- S[t]
    G_prev <- G[t]
  }
  list(S = S, G = G, F = F, future = S[n] + seq_len(h) * G[n])
}

# Grid search for alpha and beta: coarse grid (0.05) and then a fine grid
# (0.01) around the best coarse point.
fc_holt_search <- function(D, S0, G0, from, crit) {
  score <- function(a, b) fc_errors(D, fc_holt(D, a, b, S0, G0)$F, from)[[crit]]
  coarse <- seq(0.05, 1, by = 0.05)
  Z <- outer(coarse, coarse, Vectorize(score))
  best <- which(Z == min(Z, na.rm = TRUE), arr.ind = TRUE)[1, ]
  a_fine <- seq(max(0.01, coarse[best[1]] - 0.05), min(1, coarse[best[1]] + 0.05), by = 0.01)
  b_fine <- seq(max(0.01, coarse[best[2]] - 0.05), min(1, coarse[best[2]] + 0.05), by = 0.01)
  fine <- expand.grid(alpha = a_fine, beta = b_fine)
  fine$value <- mapply(score, fine$alpha, fine$beta)
  fine <- fine[order(fine$value), ]
  list(alpha = fine$alpha[1], beta = fine$beta[1], coarse = coarse, Z = Z, fine = fine)
}

# UI ---------------------------------------------------------------------------
holt_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Holt's Method"),
      fc_data_ui(ns, default_example = "Engine failures (Book Example 2.2)"),
      hr(),
      radioButtons(ns("mode"), "How should α and β be chosen?",
                   choices = c("I will enter α and β" = "manual",
                               "Find the best α and β" = "best")),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "α (smoothing constant for the level)",
                     value = 0.1, min = 0.01, max = 1, step = 0.05),
        numericInput(ns("beta"), "β (smoothing constant for the slope)",
                     value = 0.1, min = 0.01, max = 1, step = 0.05),
        helpText("Usually β ≤ α, which gives a more stable slope.")
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        fc_criterion_ui(ns),
        helpText("A 0.05 grid is searched first, then a 0.01 grid around the best point.")
      ),
      hr(),
      radioButtons(ns("init"), "Initial level S₀ and slope G₀",
                   choices = c("Fit a regression line" = "regression",
                               "Enter manually" = "manual")),
      conditionalPanel(
        "input.init == 'regression'", ns = ns,
        numericInput(ns("k"), "Regression baseline: first k periods (empty = all data)",
                     value = NA, min = 2, step = 1)
      ),
      conditionalPanel(
        "input.init == 'manual'", ns = ns,
        numericInput(ns("S0"), "Initial level (intercept) S₀", value = 200),
        numericInput(ns("G0"), "Initial slope G₀", value = 10)
      ),
      hr(),
      fc_common_ui(ns, horizon_default = 4)
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(h4("Initial values"), tableOutput(ns("init_table"))),
        method = tagList(
          p("Two smoothing equations, one for the level (intercept) and one for the slope:"),
          p("$$S_t = \\alpha D_t + (1-\\alpha)(S_{t-1} + G_{t-1})$$"),
          p("$$G_t = \\beta (S_t - S_{t-1}) + (1-\\beta) G_{t-1}$$"),
          p("The \\(\\tau\\)-step-ahead forecast made in period t is",
            "\\(F_{t,t+\\tau} = S_t + \\tau G_t\\); the one-step-ahead forecast for",
            "period t is \\(F_t = S_{t-1} + G_{t-1}\\)."),
          p("Regression initial values: a line \\(D_t = a + b\\,t\\) is fitted to the first",
            "k periods by least squares; \\(S_0 = a\\) and \\(G_0 = b\\).")
        ))
    )
  )
}

# Server -----------------------------------------------------------------------
holt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      h <- fc_int(input$h, 4)
      from <- fc_int(input$from, 1)
      notes <- character(0)
      search <- NULL

      # Initial values
      if (identical(input$init, "regression")) {
        reg <- fc_regression(D, fc_int(input$k, n, 2))
        S0 <- reg$a
        G0 <- reg$b
        init_src <- sprintf("Regression on periods 1 to %d", reg$k)
      } else {
        validate(need(!is.na(input$S0) && !is.na(input$G0), "Please enter S0 and G0."))
        S0 <- input$S0
        G0 <- input$G0
        init_src <- "Entered manually"
      }

      # Smoothing constants
      if (identical(input$mode, "best")) {
        crit <- input$crit
        sr <- fc_holt_search(D, S0, G0, from, crit)
        validate(need(!is.na(sr$fine$value[1]), "No periods left to evaluate the errors."))
        alpha <- sr$alpha
        beta <- sr$beta
        top <- head(sr$fine, 10)
        names(top) <- c("α", "β", crit)
        notes <- c(notes, sprintf("Best α = %.2f, β = %.2f (minimum %s).", alpha, beta, crit))
        search <- list(
          title = paste("Search for the best α and β (criterion:", crit, ")"),
          caption = paste("Lighter (yellow) cells have a smaller error on the 0.05 grid;",
                          "the blue dot is the best point. The 10 best points of the fine grid:"),
          plot = function() {
            image(sr$coarse, sr$coarse, sr$Z, col = hcl.colors(40, "YlOrRd", rev = TRUE),
                  xlab = "alpha", ylab = "beta", las = 1,
                  main = sprintf("%s for each (alpha, beta)", crit))
            contour(sr$coarse, sr$coarse, sr$Z, add = TRUE, col = "grey30", labcex = 0.7)
            points(alpha, beta, pch = 19, cex = 1.8, col = "#2C7BB6")
          },
          table = top
        )
      } else {
        alpha <- input$alpha
        beta <- input$beta
        validate(need(!is.na(alpha) && alpha > 0 && alpha <= 1 &&
                        !is.na(beta) && beta > 0 && beta <= 1,
                      "α and β must be between 0 and 1."))
      }

      fit <- fc_holt(D, alpha, beta, S0, G0, h)
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))

      list(label = sprintf("Holt (α = %.2f, β = %.2f)", alpha, beta),
           series_name = s$name, D = D, F = fit$F, future = fit$future, from = from,
           extra = data.frame("Level (S)" = c(fit$S, rep(NA, h)),
                              "Slope (G)" = c(fit$G, rep(NA, h)), check.names = FALSE),
           notes = notes, search = search,
           init = data.frame(Source = init_src, "S0 (level)" = S0, "G0 (slope)" = G0,
                             check.names = FALSE))
    })

    output$init_table <- renderTable(res()$init, digits = 3)

    fc_results_server(output, session, res, "holt")
  })
}
