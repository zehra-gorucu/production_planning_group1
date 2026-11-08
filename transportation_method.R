# =============================================================================
# Aggregate planning as a transportation tableau.
# Sources: regular time, overtime and subcontracting in each period.
# Destinations: demand in each period. Early production carries inventory;
# late production represents backlog. Three solutions are reported:
# north-west corner, cheapest-cell first, and the exact LP optimum.
# =============================================================================

tm_parse_period_values <- function(text, n, label) {
  pieces <- trimws(unlist(strsplit(as.character(text), "[,;[:space:]]+")))
  pieces <- pieces[nzchar(pieces)]
  values <- suppressWarnings(as.numeric(pieces))
  if (length(values) == 1) values <- rep(values, n)
  if (length(values) != n || any(!is.finite(values)) || any(values < 0)) {
    stop(sprintf("%s must contain one non-negative number or exactly %d values.", label, n))
  }
  values
}

tm_northwest <- function(supply, demand) {
  allocation <- matrix(0, nrow = length(supply), ncol = length(demand))
  remaining_s <- supply
  remaining_d <- demand
  i <- 1L; j <- 1L; tolerance <- 1e-8
  while (i <= length(supply) && j <= length(demand)) {
    amount <- min(remaining_s[i], remaining_d[j])
    allocation[i, j] <- allocation[i, j] + amount
    remaining_s[i] <- remaining_s[i] - amount
    remaining_d[j] <- remaining_d[j] - amount
    if (remaining_s[i] <= tolerance) i <- i + 1L
    if (remaining_d[j] <= tolerance) j <- j + 1L
  }
  allocation
}

tm_cheapest_cell <- function(supply, demand, cost) {
  allocation <- matrix(0, nrow = length(supply), ncol = length(demand))
  remaining_s <- supply
  remaining_d <- demand
  tolerance <- 1e-8
  while (sum(remaining_d) > tolerance) {
    available <- outer(remaining_s > tolerance, remaining_d > tolerance, "&")
    candidate_cost <- cost
    candidate_cost[!available] <- Inf
    if (!any(is.finite(candidate_cost))) stop("The remaining demand cannot be assigned to an available source.")
    k <- which.min(candidate_cost)
    i <- ((k - 1L) %% nrow(cost)) + 1L
    j <- ((k - 1L) %/% nrow(cost)) + 1L
    amount <- min(remaining_s[i], remaining_d[j])
    allocation[i, j] <- allocation[i, j] + amount
    remaining_s[i] <- remaining_s[i] - amount
    remaining_d[j] <- remaining_d[j] - amount
  }
  allocation
}

tm_exact <- function(supply, demand, cost) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("The 'lpSolve' package is required. Run install.packages('lpSolve') once.")
  }
  nr <- length(supply); nc <- length(demand); nv <- nr * nc
  row_constraints <- matrix(0, nrow = nr, ncol = nv)
  for (i in seq_len(nr)) row_constraints[i, i + nr * (seq_len(nc) - 1L)] <- 1
  column_constraints <- matrix(0, nrow = nc, ncol = nv)
  for (j in seq_len(nc)) column_constraints[j, ((j - 1L) * nr + 1L):(j * nr)] <- 1
  fit <- lpSolve::lp(
    "min", as.vector(cost),
    rbind(row_constraints, column_constraints),
    c(rep("<=", nr), rep("=", nc)), c(supply, demand)
  )
  if (fit$status != 0) stop("The transportation model is infeasible with the entered capacities.")
  matrix(fit$solution, nrow = nr, ncol = nc)
}

tm_build <- function(demand, regular_capacity, overtime_capacity,
                     subcontract_capacity, regular_cost, overtime_cost,
                     subcontract_cost, holding_cost, backlog_cost) {
  n <- length(demand)
  modes <- c("Regular", "Overtime", "Subcontract")
  source_period <- rep(seq_len(n), each = 3)
  source_mode <- rep(modes, times = n)
  supply <- as.vector(rbind(regular_capacity, overtime_capacity, subcontract_capacity))
  base_cost <- rep(c(regular_cost, overtime_cost, subcontract_cost), times = n)
  cost <- outer(source_period, seq_len(n), function(i, j) {
    holding_cost * pmax(j - i, 0) + backlog_cost * pmax(i - j, 0)
  })
  cost <- cost + base_cost
  rownames(cost) <- paste0(substr(source_mode, 1, 1), source_period)
  colnames(cost) <- paste0("Demand P", seq_len(n))
  if (sum(supply) + 1e-8 < sum(demand)) {
    stop(sprintf("Total capacity (%.2f) is below total demand (%.2f). Increase at least one source capacity.",
                 sum(supply), sum(demand)))
  }
  nw <- tm_northwest(supply, demand)
  cheap <- tm_cheapest_cell(supply, demand, cost)
  exact <- tm_exact(supply, demand, cost)
  rownames(nw) <- rownames(cheap) <- rownames(exact) <- rownames(cost)
  colnames(nw) <- colnames(cheap) <- colnames(exact) <- colnames(cost)
  list(
    demand = demand, supply = supply, source_period = source_period,
    source_mode = source_mode, cost = cost,
    allocations = list(northwest = nw, cheapest = cheap, optimal = exact),
    totals = c(
      northwest = sum(nw * cost), cheapest = sum(cheap * cost),
      optimal = sum(exact * cost)
    )
  )
}

tm_allocation_table <- function(result, method) {
  allocation <- result$allocations[[method]]
  data.frame(
    Source = rownames(allocation), Period = result$source_period,
    Mode = result$source_mode, Capacity = result$supply,
    as.data.frame(allocation, check.names = FALSE),
    Used = rowSums(allocation), Unused = result$supply - rowSums(allocation),
    check.names = FALSE
  )
}

transportation_method_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Transportation Method"),
      fc_data_ui(ns, default_example = "Aggregate planning, 6 periods (illustrative)"), hr(),
      h4("Source capacity per period"),
      helpText("Enter one value for every period, or a comma-separated value for each period."),
      textInput(ns("regular_capacity"), "Regular-time capacity", "1000"),
      textInput(ns("overtime_capacity"), "Overtime capacity", "300"),
      textInput(ns("subcontract_capacity"), "Subcontract capacity", "300"),
      h4("Production cost per unit"),
      numericInput(ns("regular_cost"), "Regular time", 20, min = 0),
      numericInput(ns("overtime_cost"), "Overtime", 30, min = 0),
      numericInput(ns("subcontract_cost"), "Subcontracting", 35, min = 0),
      h4("Time-shift cost"),
      numericInput(ns("holding_cost"), "Inventory cost per unit per early period", 2, min = 0),
      numericInput(ns("backlog_cost"), "Backlog cost per unit per late period", 5, min = 0),
      radioButtons(ns("method"), "Table shown below",
        choices = c("North-west corner" = "northwest",
                    "Cheapest cell first" = "cheapest",
                    "Exact minimum-cost solution" = "optimal"),
        selected = "optimal")
    ),
    mainPanel(
      h3(textOutput(ns("headline"))),
      uiOutput(ns("notes")),
      h4("Method cost comparison"), tableOutput(ns("comparison")),
      plotOutput(ns("plot"), height = "430px"),
      h4("Allocation table"),
      div(style = "overflow-x:auto;", tableOutput(ns("allocation"))),
      h4("Unit-cost tableau"),
      div(style = "overflow-x:auto;", tableOutput(ns("cost_table"))),
      downloadButton(ns("download"), "Download selected allocation (CSV)"), br(), br(),
      wellPanel(h4("Method"),
        p("Every period contributes three sources: regular time, overtime and subcontracting."),
        p("Producing early adds inventory cost; producing late adds backlog cost."),
        p("North-west corner is a starting solution. Cheapest-cell first is a cost-based heuristic; the exact LP solution verifies the true minimum cost."))
    )
  )
}

transportation_method_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    result <- reactive({
      s <- series(); n <- length(s$values)
      costs <- c(input$regular_cost, input$overtime_cost, input$subcontract_cost,
                 input$holding_cost, input$backlog_cost)
      validate(need(all(is.finite(costs)) && all(costs >= 0), "All costs must be non-negative numbers."))
      tryCatch(tm_build(
        s$values,
        tm_parse_period_values(input$regular_capacity, n, "Regular-time capacity"),
        tm_parse_period_values(input$overtime_capacity, n, "Overtime capacity"),
        tm_parse_period_values(input$subcontract_capacity, n, "Subcontract capacity"),
        input$regular_cost, input$overtime_cost, input$subcontract_cost,
        input$holding_cost, input$backlog_cost
      ), error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    method <- reactive({
      key <- input$method
      if (!key %in% c("northwest", "cheapest", "optimal")) "optimal" else key
    })
    method_names <- c(northwest = "North-west corner", cheapest = "Cheapest cell first",
                      optimal = "Exact minimum-cost")
    output$headline <- renderText({
      r <- result(); key <- method()
      sprintf("%s solution — total cost: %s", method_names[[key]], fc_fmt(r$totals[[key]]))
    })
    output$notes <- renderUI({
      r <- result()
      gap <- r$totals[["cheapest"]] - r$totals[["optimal"]]
      div(class = "text-muted",
          p(sprintf("Cheapest-cell gap from the exact optimum: %s.", fc_fmt(gap))),
          p("Source labels use R = regular, O = overtime, S = subcontract; the number is the source period."))
    })
    output$comparison <- renderTable({
      r <- result()
      data.frame(
        Method = unname(method_names), Cost = unname(r$totals),
        "Above exact optimum" = unname(r$totals - r$totals[["optimal"]]), check.names = FALSE
      )
    }, digits = 2)
    output$allocation <- renderTable(tm_allocation_table(result(), method()), digits = 2)
    output$cost_table <- renderTable({
      r <- result()
      data.frame(Source = rownames(r$cost), as.data.frame(r$cost, check.names = FALSE), check.names = FALSE)
    }, digits = 2, rownames = FALSE)
    output$plot <- renderPlot({
      r <- result(); allocation <- r$allocations[[method()]]
      used <- rowsum(rowSums(allocation), group = r$source_period, reorder = FALSE)
      by_mode <- matrix(0, nrow = 3, ncol = length(r$demand),
                        dimnames = list(c("Regular", "Overtime", "Subcontract"), seq_along(r$demand)))
      for (i in seq_along(r$supply)) by_mode[match(r$source_mode[i], rownames(by_mode)), r$source_period[i]] <- sum(allocation[i, ])
      centers <- barplot(by_mode, col = c("#2C7BB6", "#FDAE61", "#6A8E3A"), border = NA,
                         xlab = "Source period", ylab = "Units", main = paste(method_names[[method()]], "source use"))
      lines(centers, r$demand, type = "o", pch = 19, lwd = 2, col = "#3B3B3B")
      legend("topleft", bty = "n", ncol = 2,
             legend = c("Regular", "Overtime", "Subcontract", "Demand in period"),
             fill = c("#2C7BB6", "#FDAE61", "#6A8E3A", NA),
             lty = c(NA, NA, NA, 1), pch = c(NA, NA, NA, 19), col = c(NA, NA, NA, "#3B3B3B"))
    })
    output$download <- downloadHandler(
      filename = function() paste0("transportation_", method(), ".csv"),
      content = function(file) utils::write.csv(tm_allocation_table(result(), method()), file, row.names = FALSE)
    )
  })
}
