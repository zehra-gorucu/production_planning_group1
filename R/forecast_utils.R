# =============================================================================
# forecast_utils.R
# Shared helpers for the Forecasting modules (data input, error measures,
# regression-based initial values, plots and result tables).
#
# Notation follows Nahmias & Olsen, "Production and Operations Analysis", Ch. 2:
#   D_t : observed value (demand) in period t
#   F_t : one-step-ahead forecast for period t (made in period t - 1)
#   e_t : forecast error, e_t = F_t - D_t
#   MAD = mean(|e_t|), MSE = mean(e_t^2), MAPE = mean(|e_t / D_t|) * 100
#
# All functions start with "fc_" so they do not clash with other modules.
# Other forecasting modules (seasonal, Winters, Box-Jenkins) can reuse them.
# =============================================================================

# ---- Example data sets (from the textbook and the lecture slides) -----------
fc_examples <- list(
  "Engine failures (Book Example 2.2)" = data.frame(
    Quarter  = 1:8,
    Failures = c(200, 250, 175, 186, 225, 285, 305, 190)
  ),
  "Wallace Garden shed sales (Slides)" = data.frame(
    Month = month.abb,
    Sales = c(10, 12, 13, 16, 19, 23, 26, 30, 28, 18, 16, 14)
  ),
  "Wacker Distributors annual sales (Slides)" = data.frame(
    Year        = 1:10,
    Televisions = rep(250, 10),
    Radios      = seq(300, 390, by = 10),
    CD_Players  = c(110, 100, 120, 140, 170, 150, 160, 190, 200, 190)
  ),
  "Bridge traffic, 4 weeks x 5 days (Book Example 2.6)" = data.frame(
    Week = rep(1:4, each = 5),
    Day  = rep(c("Mon", "Tue", "Wed", "Thu", "Fri"), times = 4),
    Cars = c(16.2, 12.2, 14.2, 17.3, 22.5,
             17.3, 11.5, 15.0, 17.6, 23.5,
             14.6, 13.1, 13.0, 16.9, 21.9,
             16.1, 11.8, 12.9, 16.6, 24.3)
  ),
  "Turner Industries quarterly sales (Slides)" = data.frame(
    Year    = rep(1:3, each = 4),
    Quarter = rep(1:4, times = 3),
    Sales   = c(108, 125, 150, 141, 116, 134, 159, 152, 123, 142, 168, 165)
  ),
  "Nile annual flow, 1871-1970 (R dataset)" = data.frame(
    Year = 1871:1970,
    Flow = as.numeric(datasets::Nile)
  )
)

# ---- Reading data -----------------------------------------------------------

# Convert a column to numbers; accepts "1,5" (decimal comma) and "1 250".
fc_to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]]", "", x)
  comma_decimal <- grepl("^-?[0-9]*,[0-9]+$", x)
  x[comma_decimal] <- sub(",", ".", x[comma_decimal])
  suppressWarnings(as.numeric(x))
}

# Read an Excel (.xlsx / .xls) or CSV file into a data frame.
fc_read_file <- function(path, filename, sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet) || !nzchar(sheet)) sheet <- 1
    df <- readxl::read_excel(path, sheet = sheet)
  } else if (ext %in% c("csv", "txt")) {
    first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    n_semi  <- lengths(regmatches(first, gregexpr(";", first)))
    n_comma <- lengths(regmatches(first, gregexpr(",", first)))
    n_tab   <- lengths(regmatches(first, gregexpr("\t", first)))
    sep <- if (n_tab > max(n_semi, n_comma)) "\t" else if (n_semi > n_comma) ";" else ","
    dec <- if (sep == ";") "," else "."
    df <- utils::read.csv(path, sep = sep, dec = dec, check.names = FALSE,
                          stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  } else {
    stop("Unsupported file type '.", ext, "'. Please upload .xlsx, .xls or .csv.")
  }
  as.data.frame(df, check.names = FALSE)
}

# Names of the columns that contain at least two numbers.
fc_numeric_columns <- function(df) {
  ok <- vapply(df, function(col) sum(!is.na(fc_to_numeric(col))) >= 2, logical(1))
  names(df)[ok]
}

# ---- Data input: UI + server (used by every forecasting module) ------------

fc_data_ui <- function(ns, default_example = names(fc_examples)[1]) {
  tagList(
    radioButtons(ns("src"), "Data source",
                 choices = c("Example data" = "example", "Upload a file" = "upload"),
                 selected = "example", inline = TRUE),
    conditionalPanel(
      "input.src == 'upload'", ns = ns,
      fileInput(ns("file"), "Excel (.xlsx, .xls) or CSV file",
                accept = c(".xlsx", ".xls", ".csv", ".txt")),
      uiOutput(ns("sheet_ui"))
    ),
    conditionalPanel(
      "input.src == 'example'", ns = ns,
      selectInput(ns("example"), "Example data set",
                  choices = names(fc_examples), selected = default_example)
    ),
    uiOutput(ns("column_ui"))
  )
}

# Returns a reactive list(values = numeric vector, name = column name).
fc_data_server <- function(input, output, session) {
  ns <- session$ns

  is_excel <- function(name) tolower(tools::file_ext(name)) %in% c("xlsx", "xls")

  output$sheet_ui <- renderUI({
    req(input$src == "upload", input$file)
    if (!is_excel(input$file$name)) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(input$file$datapath),
                       error = function(e) character(0))
    if (length(sheets) <= 1) return(NULL)
    selectInput(ns("sheet"), "Sheet", choices = sheets)
  })

  raw <- reactive({
    if (identical(input$src, "example")) {
      req(input$example)
      return(fc_examples[[input$example]])
    }
    validate(need(!is.null(input$file),
                  "Please upload an Excel or CSV file (or choose 'Example data')."))
    sheet <- if (is_excel(input$file$name)) input$sheet else NULL
    tryCatch(
      fc_read_file(input$file$datapath, input$file$name, sheet),
      error = function(e) validate(need(FALSE, paste("Could not read the file:",
                                                     conditionMessage(e))))
    )
  })

  output$column_ui <- renderUI({
    df <- raw()
    cols <- fc_numeric_columns(df)
    validate(need(length(cols) > 0, "No numeric column was found in the data."))
    selectInput(ns("column"), "Series (column) to forecast",
                choices = cols, selected = cols[length(cols)])
  })

  reactive({
    df <- raw()
    req(input$column, input$column %in% names(df))
    v <- fc_to_numeric(df[[input$column]])
    dropped <- sum(is.na(v))
    v <- v[!is.na(v)]
    validate(need(length(v) >= 3, "The selected series needs at least 3 numeric values."))
    list(values = v, name = input$column, dropped = dropped)
  })
}

# ---- Common inputs ------------------------------------------------------------

fc_criterion_ui <- function(ns) {
  radioButtons(ns("crit"), "Best value minimizes",
               choices = c("MAD", "MSE", "MAPE"), selected = "MAD", inline = TRUE)
}

fc_common_ui <- function(ns, horizon_default = 1, horizon_label = "Number of future periods to forecast") {
  tagList(
    numericInput(ns("h"), horizon_label, value = horizon_default, min = 1, step = 1),
    numericInput(ns("from"), "Measure errors from period", value = 1, min = 1, step = 1),
    helpText("Periods before this one are not used in MAD, MSE and MAPE.",
             "Use it to compare methods over the same periods.")
  )
}

# Safe reading of an integer input with a default value and lower bound.
fc_int <- function(x, default, lower = 1) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  max(lower, as.integer(round(x)))
}

# Autocorrelations and the simple season-length suggestion used by the
# seasonal modules.  Lags start at 2 because lag 1 usually reflects local
# persistence rather than a complete seasonal cycle.
fc_acf <- function(D, max_lag) {
  as.numeric(stats::acf(D, lag.max = max_lag, plot = FALSE)$acf)[-1]
}

fc_suggest_season_length <- function(D) {
  max_lag <- floor(length(D) / 2)
  if (max_lag < 2) stop("The series is too short to suggest a season length.")
  r <- fc_acf(D, max_lag)
  candidates <- 2:max_lag
  candidate_acf <- r[candidates]
  candidate_acf[!is.finite(candidate_acf)] <- -Inf
  if (all(candidate_acf == -Inf)) {
    stop("A season length cannot be suggested because the autocorrelations are undefined.")
  }
  N <- candidates[which.max(candidate_acf)]
  list(N = N, acf = r)
}

# ---- Error measures -------------------------------------------------------------

# MAD, MSE and MAPE of one-step-ahead forecasts F for the series D,
# using the periods t >= from that have a forecast.
fc_errors <- function(D, F, from = 1) {
  idx <- which(!is.na(F) & !is.na(D) & seq_along(D) >= from)
  if (length(idx) == 0) {
    return(list(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_,
                n = 0L, first = NA_integer_, last = NA_integer_))
  }
  e <- F[idx] - D[idx]
  nz <- D[idx] != 0
  list(
    MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = if (any(nz)) mean(abs(e[nz] / D[idx][nz])) * 100 else NA_real_,
    n = length(idx), first = min(idx), last = max(idx)
  )
}

fc_error_table <- function(err) {
  periods <- if (err$n > 0) sprintf("%d to %d (n = %d)", err$first, err$last, err$n) else "none"
  data.frame(
    Measure = c("MAD", "MSE", "MAPE (%)"),
    Value = c(err$MAD, err$MSE, err$MAPE),
    "Periods evaluated" = rep(periods, 3),
    check.names = FALSE
  )
}

# ---- Least squares trend line (slides 51-52, Nahmias Appendix 2-B) -------------
# Fits D_t = a + b t on t = 1..k. "a" is the level at time 0, "b" is the slope.
fc_regression <- function(D, k = length(D)) {
  k <- min(max(2, k), length(D))
  t <- seq_len(k)
  y <- D[t]
  Sxy <- k * sum(t * y) - (k * (k + 1) / 2) * sum(y)
  Sxx <- k^2 * (k + 1) * (2 * k + 1) / 6 - (k * (k + 1) / 2)^2
  b <- Sxy / Sxx
  a <- mean(y) - b * (k + 1) / 2
  list(a = a, b = b, k = k)
}

# ---- Formatting, tables and plots ----------------------------------------------

fc_fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, big.mark = ","))
}

# Period-by-period table. r is the result list returned by a module:
#   D, F (length n), future (length h), from, extra (optional data frame, n + h rows)
fc_period_table <- function(r) {
  n <- length(r$D)
  h <- length(r$future)
  tab <- data.frame(Period = seq_len(n + h))
  tab[["Actual (D)"]] <- c(r$D, rep(NA, h))
  if (!is.null(r$extra)) for (nm in names(r$extra)) tab[[nm]] <- r$extra[[nm]]
  tab[["Forecast (F)"]] <- c(r$F, r$future)
  e <- c(r$F - r$D, rep(NA, h))
  tab[["Error (e = F - D)"]] <- e
  tab[["|e|"]] <- abs(e)
  used <- c(!is.na(r$F) & seq_len(n) >= r$from, rep(FALSE, h))
  tab[["In error measures"]] <- ifelse(used, "yes", "")
  tab$Period <- as.integer(tab$Period)
  tab
}

fc_plot <- function(D, F, future = NULL, main = "", ylab = "Value",
                    fitted_label = "Forecast") {
  n <- length(D)
  h <- length(future)
  yr <- range(c(D, F, future), na.rm = TRUE)
  yr <- yr + c(-0.05, 0.05) * diff(yr)
  plot(seq_len(n), D, type = "o", pch = 19, col = "#3B3B3B",
       xlim = c(1, n + max(h, 0)), ylim = yr,
       xlab = "Period", ylab = ylab, main = main, las = 1)
  grid(col = "#E5E5E5", lty = 1)
  lines(seq_len(n), D, type = "o", pch = 19, col = "#3B3B3B")
  lines(seq_len(n), F, type = "o", pch = 17, lty = 2, col = "#2C7BB6")
  if (h > 0) {
    abline(v = n + 0.5, lty = 3, col = "grey50")
    last_f <- F[n]
    if (!is.na(last_f)) segments(n, last_f, n + 1, future[1], lty = 2, col = "#D95F02")
    lines(n + seq_len(h), future, type = "o", pch = 17, cex = 1.4,
          lty = 2, col = "#D95F02")
  }
  legend("topleft", bty = "n",
         legend = c("Actual", fitted_label, "Future forecast"),
         col = c("#3B3B3B", "#2C7BB6", "#D95F02"),
         pch = c(19, 17, 17), lty = c(1, 2, 2))
}

# Bar chart for a one-parameter search (e.g. N in moving averages).
fc_plot_search_bar <- function(x, y, best, xlab, ylab) {
  cols <- ifelse(x == best, "#D95F02", "#9DB4CF")
  barplot(y, names.arg = x, col = cols, border = NA, las = 1,
          xlab = xlab, ylab = ylab,
          main = paste0("Best ", xlab, " = ", best, " (", ylab, " = ",
                        fc_fmt(min(y, na.rm = TRUE)), ")"))
}

# Line chart for a continuous one-parameter search (e.g. alpha).
fc_plot_search_line <- function(x, y, best, xlab, ylab) {
  plot(x, y, type = "l", lwd = 2, col = "#2C7BB6", las = 1,
       xlab = xlab, ylab = ylab,
       main = paste0("Best ", xlab, " = ", best, " (", ylab, " = ",
                     fc_fmt(min(y, na.rm = TRUE)), ")"))
  grid(col = "#E5E5E5", lty = 1)
  points(best, min(y, na.rm = TRUE), pch = 19, cex = 1.6, col = "#D95F02")
}

# ---- Shared results area (main panel) --------------------------------------------

fc_results_ui <- function(ns, method = NULL, extra = NULL) {
  tagList(
    h3(textOutput(ns("headline"))),
    uiOutput(ns("notes")),
    plotOutput(ns("plot"), height = "380px"),
    extra,
    uiOutput(ns("search_ui")),
    h4("Error measures"),
    tableOutput(ns("errors")),
    h4("Forecasts by period"),
    div(style = "overflow-x: auto;", tableOutput(ns("table"))),
    downloadButton(ns("download"), "Download table (CSV)"),
    br(), br(),
    if (!is.null(method)) wellPanel(h4("Method"), withMathJax(method))
  )
}

# res() must return a list with:
#   label, series_name, D, F, future, from, extra (or NULL),
#   notes (character, optional), search (NULL or list(title, caption, plot, table))
fc_results_server <- function(output, session, res, prefix) {
  ns <- session$ns

  output$headline <- renderText({
    r <- res()
    sprintf("%s — forecast for period %d: %s",
            r$label, length(r$D) + 1, fc_fmt(r$future[1]))
  })

  output$notes <- renderUI({
    r <- res()
    if (length(r$notes) == 0) return(NULL)
    div(class = "text-muted", lapply(r$notes, p))
  })

  output$plot <- renderPlot({
    r <- res()
    fc_plot(r$D, r$F, r$future,
            main = paste("Actual series and", r$label, "forecasts"),
            ylab = r$series_name)
  })

  output$errors <- renderTable({
    r <- res()
    fc_error_table(fc_errors(r$D, r$F, r$from))
  }, digits = 2)

  output$table <- renderTable(fc_period_table(res()), digits = 2, na = "")

  output$search_ui <- renderUI({
    s <- res()$search
    if (is.null(s)) return(NULL)
    tagList(
      h4(s$title),
      plotOutput(ns("search_plot"), height = "300px"),
      if (!is.null(s$caption)) p(class = "text-muted", s$caption),
      tableOutput(ns("search_table"))
    )
  })

  output$search_plot <- renderPlot({
    s <- res()$search
    req(s)
    s$plot()
  })

  output$search_table <- renderTable({
    s <- res()$search
    req(s)
    s$table
  }, digits = 3)

  output$download <- downloadHandler(
    filename = function() paste0(prefix, "_results.csv"),
    content = function(file) utils::write.csv(fc_period_table(res()), file, row.names = FALSE)
  )
}
