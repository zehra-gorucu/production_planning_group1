# =============================================================================
# Box-Jenkins models: AR, MA, ARMA and ARIMA
#
# All four families are fitted by maximum likelihood with stats::arima().
# The non-seasonal order is (p, d, q): autoregressive order, differencing order
# and moving-average order.  Future forecasts and their standard errors come
# from stats::predict.Arima().
# =============================================================================

# Computation (no Shiny) --------------------------------------------------------

fc_bj_fit <- function(D, order, h = 1, include_mean = TRUE) {
  D <- as.numeric(D)
  order <- as.integer(order)
  if (length(order) != 3 || any(order < 0)) stop("ARIMA order must be three non-negative integers.")
  if (length(D) < sum(order) + 5) stop("The series is too short for the selected model order.")

  warnings <- character(0)
  fit <- withCallingHandlers(
    stats::arima(
      D, order = order, include.mean = include_mean && order[2] == 0,
      method = "ML", transform.pars = TRUE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.null(fit$code) && fit$code != 0) {
    stop("The ARIMA optimizer did not converge (code ", fit$code, ").")
  }

  prediction <- withCallingHandlers(
    stats::predict(fit, n.ahead = h, se.fit = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  residuals <- as.numeric(fit$residuals)
  fitted <- D - residuals
  burn <- min(length(D), max(order[1], order[3]) + order[2])
  if (burn > 0) fitted[seq_len(burn)] <- NA_real_

  future <- as.numeric(prediction$pred)
  se <- as.numeric(prediction$se)
  lower <- future - stats::qnorm(0.975) * se
  upper <- future + stats::qnorm(0.975) * se
  coefficient_se <- sqrt(pmax(0, diag(stats::vcov(fit))))
  estimate <- as.numeric(stats::coef(fit))
  z <- estimate / coefficient_se
  coefficients <- data.frame(
    Parameter = names(stats::coef(fit)), Estimate = estimate,
    "Std. error" = coefficient_se, "z value" = z,
    "p value" = 2 * stats::pnorm(-abs(z)), check.names = FALSE
  )

  list(
    order = order, fit = fit, fitted = fitted, residuals = residuals,
    future = future, se = se, lower = lower, upper = upper,
    coefficients = coefficients,
    AIC = stats::AIC(fit), BIC = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)), sigma2 = fit$sigma2,
    warnings = unique(warnings), burn = burn
  )
}

fc_bj_candidates <- function(family, max_p = 4, max_d = 2, max_q = 4) {
  if (family == "AR") {
    return(data.frame(p = seq_len(max_p), d = 0L, q = 0L))
  }
  if (family == "MA") {
    return(data.frame(p = 0L, d = 0L, q = seq_len(max_q)))
  }
  if (family == "ARMA") {
    return(expand.grid(p = seq_len(max_p), d = 0L, q = seq_len(max_q)))
  }
  grid <- expand.grid(p = 0:max_p, d = 0:max_d, q = 0:max_q)
  grid[!(grid$p == 0 & grid$d == 0 & grid$q == 0), , drop = FALSE]
}

fc_bj_search <- function(D, family, max_p, max_d, max_q, criterion,
                         include_mean = TRUE) {
  candidates <- fc_bj_candidates(family, max_p, max_d, max_q)
  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    order <- as.integer(candidates[i, c("p", "d", "q")])
    result <- tryCatch(
      fc_bj_fit(D, order, h = 1, include_mean = include_mean),
      error = function(e) NULL
    )
    if (is.null(result)) return(NULL)
    data.frame(
      p = order[1], d = order[2], q = order[3],
      AIC = result$AIC, BIC = result$BIC, logLik = result$logLik,
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(table) || nrow(table) == 0) stop("No candidate model could be fitted.")
  table <- table[is.finite(table[[criterion]]), , drop = FALSE]
  if (nrow(table) == 0) stop("No candidate model produced a finite selection criterion.")
  table <- table[order(table[[criterion]], table$p + table$q + table$d), , drop = FALSE]
  best <- as.integer(table[1, c("p", "d", "q")])
  list(order = best, table = table)
}

fc_bj_ljung_box <- function(residuals, p, q) {
  residuals <- residuals[is.finite(residuals)]
  fitdf <- p + q
  max_lag <- length(residuals) - 1L
  lag <- min(max(10L, fitdf + 1L), max_lag)
  if (length(residuals) < 5 || lag <= fitdf) {
    return(data.frame(Test = "Ljung-Box", Lag = NA, Statistic = NA,
                      df = NA, "p value" = NA, check.names = FALSE))
  }
  test <- stats::Box.test(residuals, lag = lag, type = "Ljung-Box", fitdf = fitdf)
  data.frame(
    Test = "Ljung-Box", Lag = lag, Statistic = unname(test$statistic),
    df = unname(test$parameter), "p value" = test$p.value,
    check.names = FALSE
  )
}

fc_bj_order_label <- function(order, family = "ARIMA") {
  sprintf("%s(%d,%d,%d)", family, order[1], order[2], order[3])
}

# UI ---------------------------------------------------------------------------

box_jenkins_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Box-Jenkins: AR, MA, ARMA and ARIMA"),
      fc_data_ui(ns, default_example = "Nile annual flow, 1871-1970 (R dataset)"),
      hr(),
      radioButtons(
        ns("family"), "Model family",
        choices = c("AR(p)" = "AR", "MA(q)" = "MA",
                    "ARMA(p,q)" = "ARMA", "ARIMA(p,d,q)" = "ARIMA"),
        selected = "ARIMA"
      ),
      radioButtons(
        ns("mode"), "How should the order be chosen?",
        choices = c("I will enter p, d and q" = "manual",
                    "Search for the best order" = "best")
      ),
      conditionalPanel(
        "input.mode == 'manual' && (input.family == 'AR' || input.family == 'ARMA' || input.family == 'ARIMA')",
        ns = ns,
        numericInput(ns("p"), "AR order p", value = 1, min = 0, max = 20, step = 1)
      ),
      conditionalPanel(
        "input.mode == 'manual' && input.family == 'ARIMA'", ns = ns,
        numericInput(ns("d"), "Differencing order d", value = 1, min = 0, max = 5, step = 1)
      ),
      conditionalPanel(
        "input.mode == 'manual' && (input.family == 'MA' || input.family == 'ARMA' || input.family == 'ARIMA')",
        ns = ns,
        numericInput(ns("q"), "MA order q", value = 1, min = 0, max = 20, step = 1)
      ),
      conditionalPanel(
        "input.mode == 'best'", ns = ns,
        radioButtons(ns("criterion"), "Best model minimizes",
                     choices = c("AIC", "BIC"), selected = "AIC", inline = TRUE),
        numericInput(ns("max_p"), "Maximum p", value = 4, min = 1, max = 10, step = 1),
        conditionalPanel(
          "input.family == 'ARIMA'", ns = ns,
          numericInput(ns("max_d"), "Maximum d", value = 2, min = 0, max = 3, step = 1)
        ),
        numericInput(ns("max_q"), "Maximum q", value = 4, min = 1, max = 10, step = 1),
        helpText("Every permitted order up to these limits is fitted by maximum likelihood.")
      ),
      checkboxInput(ns("include_mean"), "Include a mean/intercept when d = 0", value = TRUE),
      hr(),
      fc_common_ui(ns, horizon_default = 10)
    ),
    mainPanel(
      fc_results_ui(
        ns,
        extra = tagList(
          h4("Model information"),
          tableOutput(ns("model_info")),
          h4("Parameter estimates"),
          div(style = "overflow-x: auto;", tableOutput(ns("coefficients"))),
          h4("Residual independence check"),
          tableOutput(ns("ljung_box")),
          h4("Forecast with 95% prediction intervals"),
          plotOutput(ns("interval_plot"), height = "360px"),
          h4("ACF, PACF and residual diagnostics"),
          plotOutput(ns("diagnostic_plot"), height = "620px")
        ),
        method = tagList(
          p("AR(p) uses past observations; MA(q) uses past innovations; ARMA(p,q)",
            "combines both; ARIMA(p,d,q) first differences the series d times."),
          p("Models are estimated by maximum likelihood. Automatic order search compares",
            "all permitted candidates using AIC or BIC."),
          p("ACF and PACF support model identification. After fitting, residual ACF and the",
            "Ljung-Box test help check whether serial correlation remains."),
          p("Forecast intervals are \\(\\hat{D}_{t+h} \\pm 1.96\\,SE_h\\). They do not include",
            "uncertainty in the estimated ARIMA coefficients.")
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

box_jenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)

    res <- reactive({
      s <- series()
      D <- s$values
      n <- length(D)
      h <- fc_int(input$h, 10)
      from <- fc_int(input$from, 1)
      notes <- character(0)
      search <- NULL

      validate(need(n >= 10, "Box-Jenkins models need at least 10 numeric observations."))
      if (identical(input$mode, "best")) {
        max_p <- fc_int(input$max_p, 4, 1)
        max_d <- if (input$family == "ARIMA") fc_int(input$max_d, 2, 0) else 0L
        max_q <- fc_int(input$max_q, 4, 1)
        sr <- tryCatch(
          fc_bj_search(D, input$family, max_p, max_d, max_q,
                       input$criterion, input$include_mean),
          error = function(e) validate(need(FALSE, conditionMessage(e)))
        )
        order <- sr$order
        top <- head(sr$table, 10)
        top$Model <- sprintf("(%d,%d,%d)", top$p, top$d, top$q)
        top <- top[c("Model", "p", "d", "q", "AIC", "BIC", "logLik")]
        notes <- c(notes, sprintf(
          "Best order (%d,%d,%d) has the minimum %s among %d fitted candidates.",
          order[1], order[2], order[3], input$criterion, nrow(sr$table)
        ))
        plot_table <- head(sr$table, 10)
        search <- list(
          title = paste("Automatic order search —", input$criterion),
          caption = "The 10 best converged models are shown.",
          plot = function() {
            labels <- sprintf("(%d,%d,%d)", plot_table$p, plot_table$d, plot_table$q)
            values <- plot_table[[input$criterion]]
            barplot(values, names.arg = labels, las = 2,
                    col = c("#D95F02", rep("#9DB4CF", length(values) - 1)),
                    border = NA, ylab = input$criterion,
                    main = paste("Best orders by", input$criterion))
          },
          table = top
        )
      } else {
        order <- switch(
          input$family,
          AR = c(fc_int(input$p, 1, 1), 0L, 0L),
          MA = c(0L, 0L, fc_int(input$q, 1, 1)),
          ARMA = c(fc_int(input$p, 1, 1), 0L, fc_int(input$q, 1, 1)),
          ARIMA = c(fc_int(input$p, 1, 0), fc_int(input$d, 1, 0), fc_int(input$q, 1, 0))
        )
        validate(need(sum(order) > 0, "At least one of p, d or q must be greater than zero."))
      }

      fit <- tryCatch(
        fc_bj_fit(D, order, h, input$include_mean),
        error = function(e) validate(need(FALSE, paste("The model could not be fitted:", conditionMessage(e))))
      )
      if (order[2] > 0 && isTRUE(input$include_mean)) {
        notes <- c(notes, "The mean/intercept option is ignored when d > 0.")
      }
      if (length(fit$warnings) > 0) notes <- c(notes, paste("Model warning:", fit$warnings))
      if (s$dropped > 0) notes <- c(notes, sprintf(
        "%d empty / non-numeric cells were ignored.", s$dropped
      ))

      extra <- data.frame(
        Residual = c(fit$residuals, rep(NA, h)),
        "Forecast standard error" = c(rep(NA, n), fit$se),
        "Lower 95%" = c(rep(NA, n), fit$lower),
        "Upper 95%" = c(rep(NA, n), fit$upper),
        check.names = FALSE
      )
      family_label <- if (input$family == "ARIMA") "ARIMA" else input$family
      label <- fc_bj_order_label(order, family_label)

      list(
        label = label, series_name = s$name, D = D, F = fit$fitted,
        future = fit$future, from = from, extra = extra,
        notes = notes, search = search, bj = fit,
        ljung = fc_bj_ljung_box(fit$residuals[-seq_len(max(1, fit$burn))], order[1], order[3])
      )
    })

    output$model_info <- renderTable({
      r <- res()
      fit <- r$bj
      data.frame(
        Model = r$label, Observations = length(r$D),
        "Innovation variance" = fit$sigma2, logLik = fit$logLik,
        AIC = fit$AIC, BIC = fit$BIC, check.names = FALSE
      )
    }, digits = 4)

    output$coefficients <- renderTable(res()$bj$coefficients, digits = 5, na = "")
    output$ljung_box <- renderTable(res()$ljung, digits = 5, na = "")

    output$interval_plot <- renderPlot({
      r <- res()
      fit <- r$bj
      n <- length(r$D)
      h <- length(fit$future)
      yr <- range(c(r$D, fit$future, fit$lower, fit$upper), na.rm = TRUE)
      pad <- max(diff(yr) * 0.05, 1)
      plot(seq_len(n), r$D, type = "l", lwd = 2, col = "#3B3B3B",
           xlim = c(1, n + h), ylim = yr + c(-pad, pad),
           xlab = "Period", ylab = r$series_name,
           main = paste(r$label, "forecast and 95% interval"))
      grid(col = "#E5E5E5")
      future_x <- n + seq_len(h)
      polygon(c(future_x, rev(future_x)), c(fit$lower, rev(fit$upper)),
              col = grDevices::adjustcolor("#FDAE61", alpha.f = 0.3), border = NA)
      lines(future_x, fit$future, type = "o", pch = 17, lwd = 2, col = "#D95F02")
      abline(v = n + 0.5, lty = 3, col = "grey40")
      legend("topleft", bty = "n", legend = c("Actual", "Forecast", "95% interval"),
             col = c("#3B3B3B", "#D95F02", "#FDAE61"),
             lty = c(1, 1, NA), pch = c(NA, 17, 15))
    })

    output$diagnostic_plot <- renderPlot({
      r <- res()
      residuals <- r$bj$residuals[is.finite(r$bj$residuals)]
      lag_max <- min(20, length(r$D) - 1)
      residual_lag <- min(20, length(residuals) - 1)
      old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
      on.exit(par(old_par), add = TRUE)
      stats::acf(r$D, lag.max = lag_max, main = "Series ACF", na.action = na.pass)
      stats::pacf(r$D, lag.max = lag_max, main = "Series PACF", na.action = na.pass)
      plot(residuals, type = "o", pch = 19, col = "#3B3B3B",
           xlab = "Fitted period", ylab = "Residual", main = "Model residuals")
      abline(h = 0, lty = 2, col = "#D95F02")
      stats::acf(residuals, lag.max = residual_lag, main = "Residual ACF", na.action = na.pass)
    })

    fc_results_server(output, session, res, "box_jenkins")
  })
}
