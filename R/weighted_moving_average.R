# =============================================================================
# Weighted Moving Average - WMA(N)   (Forecasting > Stationary series)
# Lecture slides 33-36 (Wallace Garden Supply example, weights 3-2-1)
#   F_t = (w_1 D_{t-1} + w_2 D_{t-2} + ... + w_N D_{t-N}) / (w_1 + ... + w_N)
# w_1 is the weight of the most recent observation.
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------
fc_wma <- function(D, w) {
  w <- w / sum(w)
  N <- length(w)
  n <- length(D)
  F <- rep(NA_real_, n)
  if (n > N) for (t in (N + 1):n) F[t] <- sum(w * D[(t - 1):(t - N)])
  list(F = F, next_value = sum(w * D[n:(n - N + 1)]))
}

# All weight vectors with `parts` non-negative weights on a grid of `step`
# that add up to 1 (e.g. step 0.05 -> 0.00, 0.05, ..., 1.00).
fc_weight_grid <- function(parts, step = 0.05) {
  total <- round(1 / step)
  comp <- function(total, parts) {
    if (parts == 1) return(matrix(total, 1, 1))
    do.call(rbind, lapply(0:total, function(k) cbind(k, comp(total - k, parts - 1))))
  }
  unname(comp(total, parts)) / total
}

# Evaluate many weight vectors at once (rows of W) over periods t >= from.
fc_wma_search <- function(D, W, from) {
  N <- ncol(W)
  n <- length(D)
  tt <- max(N + 1, from):n
  lags <- sapply(seq_len(N), function(i) D[tt - i])          # rows: periods, cols: lag i
  if (is.null(dim(lags))) lags <- matrix(lags, nrow = length(tt))
  Fm <- lags %*% t(W)                                          # periods x weight vectors
  E <- Fm - D[tt]
  nz <- D[tt] != 0
  cbind(MAD = colMeans(abs(E)),
        MSE = colMeans(E^2),
        MAPE = colMeans(abs(E[nz, , drop = FALSE] / D[tt][nz])) * 100)
}

# UI ---------------------------------------------------------------------------
weighted_ma_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Weighted Moving Average"),
      fc_data_ui(ns, default_example = "Wallace Garden shed sales (Slides)"),
      hr(),
      radioButtons(ns("mode"), "How should the weights be chosen?",
                   choices = c("I will enter the weights" = "manual",
                               "Find the best N and weights" = "best")),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        textInput(ns("w"), "Weights, most recent period first (comma separated)",
                  value = "3, 2, 1"),
        helpText("Example: '3, 2, 1' gives weight 3 to last period, 2 to two periods ago",
                 "and 1 to three periods ago. N = number of weights. Weights are divided",
                 "by their sum.")
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try (2 to 5)", value = 3, min = 2, max = 5, step = 1),
        fc_criterion_ui(ns),
        helpText("Weights are searched on a 0.05 grid (each between 0 and 1,",
                 "adding up to 1).")
      ),
      hr(),
      fc_common_ui(ns)
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(h4("Weights used"), tableOutput(ns("weights"))),
        method = tagList(
          p("More emphasis is placed on recent observations:"),
          p("$$F_{t} = \\frac{\\sum_{i=1}^{N} w_i D_{t-i}}{\\sum_{i=1}^{N} w_i}$$"),
          p("\\(w_1\\) is the weight of the most recent observation.",
            "Best weights: for every N from 2 to the largest N, all weight",
            "combinations on a 0.05 grid are tried over the same periods and the",
            "one with the smallest error measure is chosen.")
        ))
    )
  )
}

# Server -----------------------------------------------------------------------
weighted_ma_server <- function(id) {
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
        Nmax <- min(fc_int(input$Nmax, 3, 2), 5, n - 2)
        validate(need(Nmax >= 2, "The series is too short to search for weights."))
        common_from <- max(from, Nmax + 1)
        best_per_N <- lapply(2:Nmax, function(N) {
          W <- fc_weight_grid(N)
          sc <- fc_wma_search(D, W, common_from)
          i <- which.min(sc[, crit])
          list(N = N, w = W[i, ], scores = sc[i, ])
        })
        tab <- do.call(rbind, lapply(best_per_N, function(b) data.frame(
          N = as.integer(b$N),
          "Best weights (most recent first)" = paste(formatC(b$w, format = "f", digits = 2), collapse = ", "),
          MAD = b$scores[["MAD"]], MSE = b$scores[["MSE"]], MAPE = b$scores[["MAPE"]],
          check.names = FALSE)))
        best <- best_per_N[[which.min(tab[[crit]])]]
        w <- best$w
        from <- common_from
        notes <- c(notes, sprintf(
          "Best: N = %d with weights %s (minimum %s over periods %d to %d).",
          best$N, paste(formatC(w, format = "f", digits = 2), collapse = ", "),
          crit, common_from, n))
        search <- list(
          title = paste("Search for the best N and weights (criterion:", crit, ")"),
          caption = sprintf("All candidates are compared over periods %d to %d.", common_from, n),
          plot = function() fc_plot_search_bar(tab$N, tab[[crit]], best$N, "N", crit),
          table = tab
        )
      } else {
        w <- suppressWarnings(as.numeric(trimws(strsplit(input$w, "[,;[:space:]]+")[[1]])))
        w <- w[!is.na(w)]
        validate(
          need(length(w) >= 1, "Please enter at least one weight, e.g. 3, 2, 1."),
          need(all(w >= 0) && sum(w) > 0, "Weights must be non-negative and not all zero."),
          need(length(w) < n, sprintf("The number of weights must be smaller than the number of observations (%d).", n))
        )
      }

      fit <- fc_wma(D, w)
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))

      list(label = sprintf("WMA(%d)", length(w)), series_name = s$name,
           D = D, F = fit$F, future = rep(fit$next_value, h), from = from,
           extra = NULL, notes = notes, search = search, weights = w)
    })

    output$weights <- renderTable({
      r <- res()
      w <- r$weights
      data.frame(
        Lag = sprintf("D(t-%d)", seq_along(w)),
        "Weight entered / found" = w,
        "Normalized weight" = w / sum(w),
        check.names = FALSE
      )
    }, digits = 3)

    fc_results_server(output, session, res, "weighted_moving_average")
  })
}
