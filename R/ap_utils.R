# =============================================================================
# ap_utils.R
# Shared code for the Aggregate Planning modules (Assignment 3).
#
# Notation follows Nahmias & Olsen, Chapter 3:
#   D_t  demand forecast in period t        n_t  working days in period t
#   K    units produced by one worker in one day, so one worker makes K n_t units
#   W_t  workers, H_t hired, F_t fired, P_t production, I_t ending inventory
#   O_t  overtime units, U_t idle units, S_t subcontracted units
# Net demand: the starting inventory I_0 is subtracted from period 1 and the
# required ending inventory I_T is added to the last period. Shortages are not
# allowed: cumulative production must stay above cumulative net demand.
#
# The "Plan Setup" tab (ap_setup_ui / ap_setup_server) collects the data and
# costs once; every planning module receives the same setup reactive.
# =============================================================================

# ---- Example data sets ----------------------------------------------------------
ap_examples <- list(
  "Densepack (Book Example 3.1)" = list(
    data = data.frame(
      Month = c("January", "February", "March", "April", "May", "June"),
      Demand = c(1280, 640, 900, 1200, 2000, 1400),
      "Working days" = c(20, 24, 18, 26, 22, 15), check.names = FALSE),
    params = list(k_mode = "past", K = 0.14653, past_units = 245, past_workers = 76,
                  past_days = 22, W0 = 300, I0 = 500, IT = 600,
                  c_hire = 500, c_fire = 1000, c_hold = 80, c_reg = 0, reg_basis = "unit",
                  c_ot = 0, c_sc = 0, c_idle = 0)
  ),
  "Constant workforce example, 8 months (Slides)" = list(
    data = data.frame(
      Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"),
      Demand = c(220, 280, 460, 190, 310, 145, 110, 225),
      "Working days" = c(22, 16, 23, 20, 21, 22, 21, 22), check.names = FALSE),
    params = list(k_mode = "past", K = 0.3421, past_units = 520, past_workers = 38,
                  past_days = 40, W0 = 40, I0 = 0, IT = 0,
                  c_hire = 800, c_fire = 1250, c_hold = 8.5, c_reg = 75,
                  reg_basis = "worker_day", c_ot = 0, c_sc = 0, c_idle = 0)
  ),
  "Transportation example, 3 years (Slides)" = list(
    data = data.frame(
      Year = c("Year 1", "Year 2", "Year 3"),
      Demand = c(1500, 2200, 1300),
      "Working days" = c(250, 250, 250), check.names = FALSE),
    params = list(k_mode = "direct", K = 0.048, past_units = 1200, past_workers = 100,
                  past_days = 250, W0 = 100, I0 = 200, IT = 0,
                  c_hire = 0, c_fire = 0, c_hold = 4, c_reg = 80, reg_basis = "unit",
                  c_ot = 100, c_sc = 140, c_idle = 0),
    transport = list(rt_mode = "workforce", workforce = "100", ot_mode = "units",
                     ot_units = "80", sc_units = "500")
  )
)

# ---- Plan Setup: UI -----------------------------------------------------------------
ap_setup_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Plan Setup"),
      helpText("The data, productivity and costs entered here are used by every",
               "aggregate planning method in this menu."),
      radioButtons(ns("src"), "Data source",
                   choices = c("Example data" = "example", "Upload a file" = "upload"),
                   inline = TRUE),
      conditionalPanel(
        "input.src == 'example'", ns = ns,
        selectInput(ns("example"), "Example data set", choices = names(ap_examples))
      ),
      conditionalPanel(
        "input.src == 'upload'", ns = ns,
        fileInput(ns("file"), "Excel (.xlsx, .xls) or CSV file",
                  accept = c(".xlsx", ".xls", ".csv", ".txt")),
        helpText("One row per period with columns for the period name, the demand",
                 "forecast and the number of working days."),
        uiOutput(ns("sheet_ui")),
        uiOutput(ns("column_ui"))
      ),
      textAreaInput(ns("period_names"), "Period names (one per line, editable)",
                    value = "", rows = 6),
      hr(),
      h4("Productivity K"),
      radioButtons(ns("k_mode"), NULL,
                   choices = c("Enter K directly" = "direct",
                               "Compute K from past data" = "past")),
      conditionalPanel(
        "input.k_mode == 'direct'", ns = ns,
        numericInput(ns("K"), "K = units produced by one worker in one day",
                     value = 0.14653, min = 0, step = 0.01)
      ),
      conditionalPanel(
        "input.k_mode == 'past'", ns = ns,
        numericInput(ns("past_units"), "Units produced in the past period", value = 245, min = 0),
        numericInput(ns("past_workers"), "Number of workers", value = 76, min = 0),
        numericInput(ns("past_days"), "Number of working days", value = 22, min = 0),
        helpText("K = units / (workers × working days)")
      ),
      hr(),
      h4("Plant data"),
      numericInput(ns("W0"), "Starting workforce (workers)", value = 300, min = 0, step = 1),
      numericInput(ns("I0"), "Starting inventory (units)", value = 500, min = 0),
      numericInput(ns("IT"), "Required ending inventory (units)", value = 600, min = 0),
      hr(),
      h4("Costs"),
      numericInput(ns("c_hire"), "Hiring cost per worker", value = 500, min = 0),
      numericInput(ns("c_fire"), "Firing cost per worker", value = 1000, min = 0),
      numericInput(ns("c_hold"), "Holding cost per unit per period", value = 80, min = 0),
      numericInput(ns("c_reg"), "Regular time cost", value = 0, min = 0),
      radioButtons(ns("reg_basis"), NULL,
                   choices = c("per unit produced" = "unit",
                               "per worker per day (payroll)" = "worker_day"),
                   inline = TRUE),
      numericInput(ns("c_ot"), "Overtime cost per unit", value = 0, min = 0),
      numericInput(ns("c_sc"), "Subcontracting cost per unit", value = 0, min = 0),
      numericInput(ns("c_idle"), "Idle time cost per unit (linear programming)", value = 0, min = 0)
    ),
    mainPanel(
      h3(textOutput(ns("headline"))),
      uiOutput(ns("notes")),
      h4("Planning data"),
      div(style = "overflow-x: auto;", tableOutput(ns("data_table"))),
      plotOutput(ns("demand_plot"), height = "320px"),
      wellPanel(
        h4("Definitions"),
        withMathJax(
          p("Units one worker produces in period t: \\(K n_t\\)."),
          p("Net demand: \\(D'_1 = D_1 - I_0\\), \\(D'_T = D_T + I_T\\), otherwise \\(D'_t = D_t\\)."),
          p("No shortages: cumulative production must be at least cumulative net demand",
            "in every period. Ending inventory is computed with the original demand:",
            "\\(I_t = I_{t-1} + P_t - D_t\\)."),
          p("Overtime and subcontracting costs are the full cost of one unit.",
            "Holding cost is charged on the ending inventory of every period.")
        )
      )
    )
  )
}

# ---- Plan Setup: server ---------------------------------------------------------------
ap_setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_excel <- function(name) tolower(tools::file_ext(name)) %in% c("xlsx", "xls")

    output$sheet_ui <- renderUI({
      req(input$src == "upload", input$file)
      if (!is_excel(input$file$name)) return(NULL)
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) character(0))
      if (length(sheets) <= 1) return(NULL)
      selectInput(ns("sheet"), "Sheet", choices = sheets)
    })

    raw <- reactive({
      if (identical(input$src, "example")) {
        req(input$example)
        return(ap_examples[[input$example]]$data)
      }
      validate(need(!is.null(input$file), "Please upload an Excel or CSV file (or choose 'Example data')."))
      sheet <- if (is_excel(input$file$name)) input$sheet else NULL
      tryCatch(fc_read_file(input$file$datapath, input$file$name, sheet),
               error = function(e) validate(need(FALSE, paste("Could not read the file:", conditionMessage(e)))))
    })

    output$column_ui <- renderUI({
      df <- raw()
      cols <- names(df)
      num <- fc_numeric_columns(df)
      validate(need(length(num) > 0, "No numeric column was found in the file."))
      pick <- function(pattern, pool, default) {
        hit <- grep(pattern, pool, ignore.case = TRUE, value = TRUE)
        if (length(hit)) hit[1] else default
      }
      text_cols <- setdiff(cols, num)
      tagList(
        selectInput(ns("col_period"), "Period name column",
                    choices = c("(number the periods 1, 2, ...)" = "", cols),
                    selected = pick("period|month|name|year|quarter|week",
                                    cols, if (length(text_cols)) text_cols[1] else "")),
        selectInput(ns("col_demand"), "Demand forecast column", choices = num,
                    selected = pick("demand|forecast", num, num[1])),
        selectInput(ns("col_days"), "Working days column",
                    choices = c("(same number in every period)" = "", num),
                    selected = pick("day", num, "")),
        conditionalPanel(
          "input.col_days == ''", ns = ns,
          numericInput(ns("days_all"), "Working days in every period", value = 20, min = 1)
        )
      )
    })

    # Columns of the planning data: names, demand, working days
    base <- reactive({
      df <- raw()
      if (identical(input$src, "example")) {
        return(list(names = as.character(df[[1]]), demand = df$Demand, days = df[["Working days"]]))
      }
      req(input$col_demand, input$col_demand %in% names(df))
      demand <- fc_to_numeric(df[[input$col_demand]])
      keep <- !is.na(demand)
      days <- if (!is.null(input$col_days) && nzchar(input$col_days) && input$col_days %in% names(df)) {
        fc_to_numeric(df[[input$col_days]])
      } else {
        rep(fc_int(input$days_all, 20), nrow(df))
      }
      names_col <- if (!is.null(input$col_period) && nzchar(input$col_period) &&
                       input$col_period %in% names(df)) {
        as.character(df[[input$col_period]])
      } else {
        paste("Period", seq_len(nrow(df)))
      }
      list(names = names_col[keep], demand = demand[keep], days = days[keep])
    })

    # Put the period names from the data into the editable box
    observeEvent(base(), {
      updateTextAreaInput(session, "period_names", value = paste(base()$names, collapse = "\n"))
    })

    # Example data sets also fill in K, plant data and costs
    observeEvent(list(input$src, input$example), {
      if (!identical(input$src, "example") || is.null(input$example)) return()
      p <- ap_examples[[input$example]]$params
      updateRadioButtons(session, "k_mode", selected = p$k_mode)
      updateNumericInput(session, "K", value = p$K)
      updateNumericInput(session, "past_units", value = p$past_units)
      updateNumericInput(session, "past_workers", value = p$past_workers)
      updateNumericInput(session, "past_days", value = p$past_days)
      for (nm in c("W0", "I0", "IT", "c_hire", "c_fire", "c_hold", "c_reg", "c_ot", "c_sc", "c_idle")) {
        updateNumericInput(session, nm, value = p[[nm]])
      }
      updateRadioButtons(session, "reg_basis", selected = p$reg_basis)
    })

    setup <- reactive({
      b <- base()
      n <- length(b$demand)
      validate(
        need(n >= 2, "At least two periods are needed."),
        need(all(is.finite(b$demand)) && all(b$demand >= 0), "Demand forecasts must be non-negative numbers."),
        need(all(is.finite(b$days)) && all(b$days > 0), "Working days must be positive numbers in every period.")
      )
      notes <- character(0)
      typed <- trimws(strsplit(input$period_names %||% "", "\n")[[1]])
      typed <- typed[nzchar(typed)]
      periods <- if (length(typed) == n) typed else b$names
      if (length(typed) > 0 && length(typed) != n) {
        notes <- c(notes, sprintf("The period-name box has %d lines but there are %d periods; the names from the data are used.",
                                  length(typed), n))
      }

      if (identical(input$k_mode, "past")) {
        validate(need(all(is.finite(c(input$past_units, input$past_workers, input$past_days))) &&
                        input$past_workers > 0 && input$past_days > 0 && input$past_units > 0,
                      "Enter positive past production, workers and working days to compute K."))
        K <- input$past_units / (input$past_workers * input$past_days)
        notes <- c(notes, sprintf("K = %s / (%s × %s) = %.5f units per worker per day.",
                                  fc_fmt(input$past_units, 0), fc_fmt(input$past_workers, 0),
                                  fc_fmt(input$past_days, 0), K))
      } else {
        validate(need(is.finite(input$K) && input$K > 0, "K must be a positive number."))
        K <- input$K
      }

      vals <- c(input$W0, input$I0, input$IT, input$c_hire, input$c_fire, input$c_hold,
                input$c_reg, input$c_ot, input$c_sc, input$c_idle)
      validate(need(length(vals) == 10 && all(is.finite(vals)) && all(vals >= 0),
                    "Plant data and costs must be non-negative numbers."))

      net <- b$demand
      net[1] <- net[1] - input$I0
      net[n] <- net[n] + input$IT

      list(
        periods = periods, demand = b$demand, days = b$days, n = n, K = K,
        kn = K * b$days, net = net, W0 = input$W0, I0 = input$I0, IT = input$IT,
        costs = list(hire = input$c_hire, fire = input$c_fire, hold = input$c_hold,
                     reg = input$c_reg, reg_basis = input$reg_basis %||% "unit",
                     ot = input$c_ot, sc = input$c_sc, idle = input$c_idle),
        preset = if (identical(input$src, "example")) input$example else NULL,
        notes = notes
      )
    })

    output$headline <- renderText({
      s <- setup()
      sprintf("%d periods — total demand %s units, K = %.5f", s$n, fc_fmt(sum(s$demand), 0), s$K)
    })
    output$notes <- renderUI({
      notes <- setup()$notes
      if (length(notes) == 0) return(NULL)
      div(class = "text-muted", lapply(notes, p))
    })
    output$data_table <- renderTable({
      s <- setup()
      data.frame(
        Period = s$periods, "Working days (n)" = s$days, "Demand (D)" = s$demand,
        "Net demand (D')" = s$net, "Cumulative net demand" = cumsum(s$net),
        "Units per worker (K n)" = s$kn, check.names = FALSE
      )
    }, digits = 3)
    output$demand_plot <- renderPlot({
      s <- setup()
      par(mar = c(5, 5, 3, 1))
      barplot(s$demand, names.arg = s$periods, col = "#9DB4CF", border = NA, las = 1,
              ylab = "Units", main = "Demand forecast by period")
    })

    setup
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Plan evaluation ------------------------------------------------------------------

# Cost of each item for a plan.
ap_cost_items <- function(s, W, H, F, inv, regular_units, overtime = NULL,
                          subcontract = NULL, idle = NULL) {
  c <- s$costs
  regular <- if (identical(c$reg_basis, "worker_day")) c$reg * sum(W * s$days) else c$reg * sum(regular_units)
  items <- c("Hiring" = c$hire * sum(H), "Firing" = c$fire * sum(F),
             "Holding" = c$hold * sum(pmax(inv, 0)), "Regular time" = regular)
  if (!is.null(overtime)) items["Overtime"] <- c$ot * sum(overtime)
  if (!is.null(subcontract)) items["Subcontracting"] <- c$sc * sum(subcontract)
  if (!is.null(idle)) items["Idle time"] <- c$idle * sum(idle)
  items
}

# A workforce-only plan (chase, level, mixed): production = K n_t W_t.
ap_evaluate_workforce <- function(s, W) {
  n <- s$n
  P <- W * s$kn
  prevW <- c(s$W0, W[-n])
  H <- pmax(W - prevW, 0)
  F <- pmax(prevW - W, 0)
  inv <- s$I0 + cumsum(P - s$demand)
  tab <- data.frame(
    Period = s$periods, "Working days" = s$days, Demand = s$demand,
    "Net demand" = s$net, "Units per worker (K n)" = s$kn,
    Workers = W, Hired = H, Fired = F, Production = P,
    "Cumulative production" = cumsum(P), "Cumulative net demand" = cumsum(s$net),
    "Ending inventory" = inv, check.names = FALSE
  )
  list(table = tab, production = P, cum_production = cumsum(P),
       costs = ap_cost_items(s, W, H, F, inv, regular_units = P),
       hired = sum(H), fired = sum(F), shortage = any(inv < -1e-6))
}

# ---- Shared results area for the plan modules -------------------------------------------

ap_setup_info_ui <- function(ns) {
  tagList(uiOutput(ns("setup_info")), hr())
}

ap_results_ui <- function(ns, method = NULL) {
  tagList(
    h3(textOutput(ns("headline"))),
    uiOutput(ns("notes")),
    h4("Cost of each item"),
    div(style = "overflow-x: auto;", tableOutput(ns("costs"))),
    plotOutput(ns("cum_plot"), height = "380px"),
    h4("Plan by period"),
    uiOutput(ns("plan_tabs")),
    downloadButton(ns("download"), "Download the plans (CSV)"),
    br(), br(),
    if (!is.null(method)) wellPanel(h4("Method"), withMathJax(method))
  )
}

# res() returns list(title, plans = named list of plan objects, notes)
# A plan object has: table, production (total supply per period), costs (named vector)
ap_results_server <- function(input, output, session, setup, res, prefix) {
  ns <- session$ns

  output$setup_info <- renderUI({
    s <- setup()
    div(class = "text-muted",
        p(sprintf("Data: %d periods, K = %.5f, starting workforce %s, starting inventory %s, ending inventory %s.",
                  s$n, s$K, fc_fmt(s$W0, 0), fc_fmt(s$I0, 0), fc_fmt(s$IT, 0))),
        p("Change the data, K and costs in Aggregate Planning → Plan Setup."))
  })

  output$headline <- renderText({
    r <- res()
    totals <- vapply(r$plans, function(p) sum(p$costs), numeric(1))
    paste0(r$title, " — total cost: ",
           paste(sprintf("%s %s", names(totals), fc_fmt(totals)), collapse = " | "))
  })

  output$notes <- renderUI({
    notes <- res()$notes
    if (length(notes) == 0) return(NULL)
    div(class = "text-muted", lapply(notes, p))
  })

  output$costs <- renderTable({
    r <- res()
    items <- unique(unlist(lapply(r$plans, function(p) names(p$costs))))
    tab <- data.frame(Item = c(items, "Total cost"), check.names = FALSE)
    for (nm in names(r$plans)) {
      v <- r$plans[[nm]]$costs[items]
      v[is.na(v)] <- 0
      tab[[nm]] <- c(v, sum(v))
    }
    tab
  }, digits = 2)

  output$cum_plot <- renderPlot({
    r <- res()
    s <- setup()
    n <- s$n
    cum_net <- cumsum(s$net)
    curves <- lapply(r$plans, function(p) cumsum(p$production))
    yr <- range(c(0, cum_net, unlist(curves)))
    par(mar = c(5, 5, 3, 1))
    plot(seq_len(n), cum_net, type = "o", pch = 19, lwd = 2, col = "#3B3B3B", ylim = yr,
         xaxt = "n", las = 1, xlab = "", ylab = "Cumulative units",
         main = "Cumulative production vs. cumulative net demand")
    axis(1, at = seq_len(n), labels = s$periods, las = 2, cex.axis = 0.85)
    grid(col = "#E5E5E5", lty = 1)
    cols <- c("#2C7BB6", "#D95F02", "#6A8E3A", "#7B3294")
    for (i in seq_along(curves)) {
      lines(seq_len(n), curves[[i]], type = "o", pch = 17, lty = i + 1, lwd = 2, col = cols[i])
    }
    legend("topleft", bty = "n", legend = c("Cumulative net demand", names(curves)),
           col = c("#3B3B3B", cols[seq_along(curves)]), lty = c(1, seq_along(curves) + 1),
           pch = c(19, rep(17, length(curves))), lwd = 2)
  })

  output$plan_tabs <- renderUI({
    r <- res()
    tabs <- lapply(seq_along(r$plans), function(i) {
      tabPanel(names(r$plans)[i], div(style = "overflow-x: auto;", tableOutput(ns(paste0("plan_table_", i)))))
    })
    do.call(tabsetPanel, tabs)
  })

  observe({
    r <- res()
    lapply(seq_along(r$plans), function(i) {
      local({
        k <- i
        output[[paste0("plan_table_", k)]] <- renderTable(res()$plans[[k]]$table, digits = 2, na = "")
      })
    })
  })

  output$download <- downloadHandler(
    filename = function() paste0(prefix, "_plans.csv"),
    content = function(file) {
      r <- res()
      out <- do.call(rbind, lapply(names(r$plans), function(nm) {
        cbind(Plan = nm, r$plans[[nm]]$table)
      }))
      utils::write.csv(out, file, row.names = FALSE)
    }
  )
}

# Two versions of a workforce plan: the original (fractional) and whole workers.
ap_two_plans <- function(s, W_original, W_rounded) {
  original <- ap_evaluate_workforce(s, W_original)
  rounded <- ap_evaluate_workforce(s, W_rounded)
  list("Original plan" = original, "Whole workers (rounded up)" = rounded)
}

ap_rounding_note <- function(plans) {
  w1 <- plans[[1]]$table$Workers
  if (all(abs(w1 - round(w1)) < 1e-9)) {
    "The original plan already uses whole workers, so both plans are the same."
  } else {
    sprintf("Whole workers: rounding up changes the total cost from %s to %s.",
            fc_fmt(sum(plans[[1]]$costs)), fc_fmt(sum(plans[[2]]$costs)))
  }
}
