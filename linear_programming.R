# =============================================================================
# Aggregate planning with linear programming
# Regular production, overtime, idle capacity and subcontracting are optional.
# The model also balances workforce, inventory and backlog in every period.
# =============================================================================

lp_add_constraint <- function(model, coefficients, direction, rhs) {
  model$mat <- rbind(model$mat, coefficients)
  model$dir <- c(model$dir, direction)
  model$rhs <- c(model$rhs, rhs)
  model
}

lp_aggregate_plan <- function(demand, params, overtime_per_worker,
                              overtime_cost, idle_cost,
                              subcontract_capacity, subcontract_cost,
                              use_overtime = TRUE, use_idle = TRUE,
                              use_subcontract = TRUE,
                              integer_workforce = FALSE) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("The 'lpSolve' package is required. Run install.packages('lpSolve') once.")
  }
  demand <- as.numeric(demand)
  n <- length(demand)
  blocks <- c("W", "H", "L", "R", "O", "S", "U", "I", "B")
  nv <- length(blocks) * n
  at <- function(block, period) (match(block, blocks) - 1L) * n + period

  objective <- numeric(nv)
  objective[sapply(seq_len(n), function(t) at("H", t))] <- params$hire_cost
  objective[sapply(seq_len(n), function(t) at("L", t))] <- params$layoff_cost
  objective[sapply(seq_len(n), function(t) at("R", t))] <- params$regular_cost
  objective[sapply(seq_len(n), function(t) at("O", t))] <- overtime_cost
  objective[sapply(seq_len(n), function(t) at("S", t))] <- subcontract_cost
  objective[sapply(seq_len(n), function(t) at("U", t))] <- idle_cost
  objective[sapply(seq_len(n), function(t) at("I", t))] <- params$holding_cost
  objective[sapply(seq_len(n), function(t) at("B", t))] <- params$backlog_cost

  model <- list(mat = matrix(numeric(0), nrow = 0, ncol = nv),
                dir = character(0), rhs = numeric(0))
  for (t in seq_len(n)) {
    # Workforce: W_t = W_(t-1) + H_t - L_t.
    row <- numeric(nv)
    row[at("W", t)] <- 1
    row[at("H", t)] <- -1
    row[at("L", t)] <- 1
    if (t > 1) row[at("W", t - 1)] <- -1
    model <- lp_add_constraint(model, row, "=",
                               if (t == 1) params$initial_workforce else 0)

    # Regular capacity is either produced or idle.
    row <- numeric(nv)
    row[at("R", t)] <- 1
    row[at("U", t)] <- 1
    row[at("W", t)] <- -params$productivity
    model <- lp_add_constraint(model, row, "=", 0)

    # Overtime capacity grows with the workforce.
    row <- numeric(nv)
    row[at("O", t)] <- 1
    row[at("W", t)] <- -overtime_per_worker
    model <- lp_add_constraint(model, row, "<=", 0)

    # Subcontract capacity is entered per period.
    row <- numeric(nv)
    row[at("S", t)] <- 1
    model <- lp_add_constraint(model, row, "<=", subcontract_capacity)

    # Material balance: production + opening net stock - ending net stock = demand.
    row <- numeric(nv)
    row[c(at("R", t), at("O", t), at("S", t))] <- 1
    row[at("I", t)] <- -1
    row[at("B", t)] <- 1
    rhs <- demand[t]
    if (t == 1) {
      rhs <- rhs - params$initial_inventory
    } else {
      row[at("I", t - 1)] <- 1
      row[at("B", t - 1)] <- -1
    }
    model <- lp_add_constraint(model, row, "=", rhs)

    if (!use_overtime) {
      row <- numeric(nv); row[at("O", t)] <- 1
      model <- lp_add_constraint(model, row, "=", 0)
    }
    if (!use_idle) {
      row <- numeric(nv); row[at("U", t)] <- 1
      model <- lp_add_constraint(model, row, "=", 0)
    }
    if (!use_subcontract) {
      row <- numeric(nv); row[at("S", t)] <- 1
      model <- lp_add_constraint(model, row, "=", 0)
    }
  }

  row <- numeric(nv); row[at("I", n)] <- 1
  model <- lp_add_constraint(model, row, "=", params$target_inventory)
  row <- numeric(nv); row[at("B", n)] <- 1
  model <- lp_add_constraint(model, row, "=", 0)

  integer_vars <- if (integer_workforce) {
    unlist(lapply(c("W", "H", "L"), function(b) sapply(seq_len(n), function(t) at(b, t))))
  } else NULL
  fit <- lpSolve::lp(
    direction = "min", objective.in = objective,
    const.mat = model$mat, const.dir = model$dir, const.rhs = model$rhs,
    int.vec = integer_vars
  )
  if (fit$status != 0) {
    stop("No feasible plan was found. Increase capacity, allow idle time, or revise the ending-inventory requirement.")
  }
  value <- function(block) fit$solution[sapply(seq_len(n), function(t) at(block, t))]
  previous_w <- c(params$initial_workforce, value("W")[-n])
  tab <- data.frame(
    Period = seq_len(n), Demand = demand,
    Workforce = value("W"), Hires = value("H"), Layoffs = value("L"),
    "Regular production" = value("R"),
    "Overtime production" = value("O"),
    "Subcontract production" = value("S"),
    "Idle regular capacity" = value("U"),
    "Ending inventory" = value("I"),
    "Ending backlog" = value("B"), check.names = FALSE
  )
  tab[["Total production"]] <- tab[["Regular production"]] +
    tab[["Overtime production"]] + tab[["Subcontract production"]]
  tab[["Regular cost"]] <- tab[["Regular production"]] * params$regular_cost
  tab[["Overtime cost"]] <- tab[["Overtime production"]] * overtime_cost
  tab[["Subcontract cost"]] <- tab[["Subcontract production"]] * subcontract_cost
  tab[["Idle cost"]] <- tab[["Idle regular capacity"]] * idle_cost
  tab[["Hiring cost"]] <- tab$Hires * params$hire_cost
  tab[["Layoff cost"]] <- tab$Layoffs * params$layoff_cost
  tab[["Holding cost"]] <- tab[["Ending inventory"]] * params$holding_cost
  tab[["Backlog cost"]] <- tab[["Ending backlog"]] * params$backlog_cost
  cost_cols <- c("Regular cost", "Overtime cost", "Subcontract cost", "Idle cost",
                 "Hiring cost", "Layoff cost", "Holding cost", "Backlog cost")
  tab[["Period cost"]] <- rowSums(tab[cost_cols])
  list(table = tab, objective = fit$objval, cost_cols = cost_cols,
       workforce_balance = max(abs(tab$Workforce - (previous_w + tab$Hires - tab$Layoffs))))
}

linear_programming_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Linear Programming Aggregate Plan"),
      hp_common_ui(ns), hr(),
      h4("Planning options"),
      checkboxInput(ns("use_overtime"), "Allow overtime", TRUE),
      conditionalPanel("input.use_overtime", ns = ns,
        numericInput(ns("overtime_per_worker"), "Maximum overtime units per worker", 30, min = 0),
        numericInput(ns("overtime_cost"), "Overtime cost per unit", 30, min = 0)
      ),
      checkboxInput(ns("use_idle"), "Allow idle time", TRUE),
      conditionalPanel("input.use_idle", ns = ns,
        numericInput(ns("idle_cost"), "Idle-time cost per unused unit of regular capacity", 5, min = 0)
      ),
      checkboxInput(ns("use_subcontract"), "Allow subcontracting", TRUE),
      conditionalPanel("input.use_subcontract", ns = ns,
        numericInput(ns("subcontract_capacity"), "Maximum subcontract units per period", 300, min = 0),
        numericInput(ns("subcontract_cost"), "Subcontract cost per unit", 35, min = 0)
      ),
      checkboxInput(ns("integer_workforce"), "Require whole workers", FALSE)
    ),
    mainPanel(
      h3(textOutput(ns("headline"))), uiOutput(ns("notes")),
      h4("Cost summary"), tableOutput(ns("cost_summary")),
      plotOutput(ns("plot"), height = "560px"),
      h4("Optimal plan by period"),
      div(style = "overflow-x:auto;", tableOutput(ns("plan_table"))),
      downloadButton(ns("download"), "Download optimal plan (CSV)"), br(), br(),
      wellPanel(h4("Model"),
        p("The objective minimizes regular, overtime, subcontracting, idle-time, hiring, layoff, inventory and backlog costs."),
        p("Workforce balance, regular/overtime capacity, material balance and required ending inventory are enforced in every period."))
    )
  )
}

linear_programming_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    result <- reactive({
      s <- series()
      p <- tryCatch(hp_parameters(input), error = function(e) validate(need(FALSE, conditionMessage(e))))
      optional_value <- function(x) if (is.null(x) || length(x) == 0) 0 else x
      extra <- c(optional_value(input$overtime_per_worker), optional_value(input$overtime_cost),
                 optional_value(input$idle_cost), optional_value(input$subcontract_capacity),
                 optional_value(input$subcontract_cost))
      validate(need(all(is.finite(extra)) && all(extra >= 0),
                    "All option capacities and costs must be non-negative numbers."))
      tryCatch(lp_aggregate_plan(
        s$values, p,
        overtime_per_worker = if (isTRUE(input$use_overtime)) input$overtime_per_worker else 0,
        overtime_cost = if (isTRUE(input$use_overtime)) input$overtime_cost else 0,
        idle_cost = if (isTRUE(input$use_idle)) input$idle_cost else 0,
        subcontract_capacity = if (isTRUE(input$use_subcontract)) input$subcontract_capacity else 0,
        subcontract_cost = if (isTRUE(input$use_subcontract)) input$subcontract_cost else 0,
        use_overtime = isTRUE(input$use_overtime), use_idle = isTRUE(input$use_idle),
        use_subcontract = isTRUE(input$use_subcontract),
        integer_workforce = isTRUE(input$integer_workforce)
      ), error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$headline <- renderText(sprintf("Optimal LP plan — total cost: %s", fc_fmt(result()$objective)))
    output$notes <- renderUI({
      s <- series()
      notes <- c(if (isTRUE(input$integer_workforce)) "Workforce, hires and layoffs are restricted to whole workers." else "Workforce may be fractional.")
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))
      div(class = "text-muted", lapply(notes, p))
    })
    output$cost_summary <- renderTable({
      r <- result(); values <- colSums(r$table[r$cost_cols])
      data.frame(Cost = c(names(values), "Total cost"), Amount = c(values, sum(values)), check.names = FALSE)
    }, digits = 2)
    output$plot <- renderPlot(hp_plot_plan(result()$table, "Linear Programming"))
    output$plan_table <- renderTable(result()$table, digits = 2)
    output$download <- downloadHandler(
      filename = function() "linear_programming_plan.csv",
      content = function(file) utils::write.csv(result()$table, file, row.names = FALSE)
    )
  })
}
