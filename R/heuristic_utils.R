# =============================================================================
# Shared helpers for aggregate-planning heuristic modules.
#
# The three plans use the same demand input, capacity/cost parameters, table,
# graph and download layout.  A positive net inventory means stock on hand; a
# negative net inventory is reported as backlog.
# =============================================================================

hp_common_ui <- function(ns) {
  tagList(
    fc_data_ui(ns, default_example = "Aggregate planning, 6 periods (illustrative)"),
    hr(),
    h4("Capacity and inventory"),
    numericInput(ns("initial_workforce"), "Initial workforce", value = 10, min = 0, step = 1),
    numericInput(ns("productivity"), "Regular production per worker per period",
                 value = 100, min = 0.01, step = 10),
    numericInput(ns("initial_inventory"), "Initial inventory", value = 0, min = 0, step = 10),
    numericInput(ns("target_inventory"), "Required ending inventory", value = 0, min = 0, step = 10),
    hr(),
    h4("Costs"),
    numericInput(ns("regular_cost"), "Regular production cost per unit", value = 20, min = 0),
    numericInput(ns("hire_cost"), "Hiring cost per worker", value = 1000, min = 0),
    numericInput(ns("layoff_cost"), "Layoff cost per worker", value = 1500, min = 0),
    numericInput(ns("holding_cost"), "Inventory holding cost per unit per period", value = 2, min = 0),
    numericInput(ns("backlog_cost"), "Backlog cost per unit per period", value = 5, min = 0)
  )
}

hp_display_ui <- function(ns) {
  radioButtons(
    ns("display"), "Plan shown in the graph and period table",
    choices = c("Fractional workforce" = "exact",
                "Workforce rounded up" = "rounded"),
    selected = "exact"
  )
}

hp_parameters <- function(input) {
  values <- list(
    initial_workforce = input$initial_workforce,
    productivity = input$productivity,
    initial_inventory = input$initial_inventory,
    target_inventory = input$target_inventory,
    regular_cost = input$regular_cost,
    hire_cost = input$hire_cost,
    layoff_cost = input$layoff_cost,
    holding_cost = input$holding_cost,
    backlog_cost = input$backlog_cost
  )
  flat <- unlist(values, use.names = FALSE)
  if (any(!is.finite(flat))) stop("All capacity, inventory and cost inputs must be finite numbers.")
  if (values$productivity <= 0) stop("Production per worker must be greater than zero.")
  if (values$initial_workforce < 0 || values$initial_inventory < 0 ||
      values$target_inventory < 0 || any(flat[5:9] < 0)) {
    stop("Workforce, inventory and cost inputs cannot be negative.")
  }
  values
}

hp_make_plan <- function(demand, workforce, regular, params,
                         overtime = NULL, subcontract = NULL, label = "Plan") {
  demand <- as.numeric(demand)
  n <- length(demand)
  if (any(!is.finite(demand)) || any(demand < 0)) {
    stop("Demand values must be finite and non-negative.")
  }
  if (length(workforce) != n || length(regular) != n) stop("Plan vectors must match demand length.")
  if (is.null(overtime)) overtime <- rep(0, n)
  if (is.null(subcontract)) subcontract <- rep(0, n)

  previous_workforce <- c(params$initial_workforce, workforce[-n])
  hires <- pmax(workforce - previous_workforce, 0)
  layoffs <- pmax(previous_workforce - workforce, 0)
  opening_net <- ending_net <- numeric(n)
  net <- params$initial_inventory
  for (t in seq_len(n)) {
    opening_net[t] <- net
    net <- net + regular[t] + overtime[t] + subcontract[t] - demand[t]
    ending_net[t] <- net
  }

  tab <- data.frame(
    Period = seq_len(n), Demand = demand,
    "Opening inventory" = pmax(opening_net, 0),
    "Opening backlog" = pmax(-opening_net, 0),
    Workforce = workforce, Hires = hires, Layoffs = layoffs,
    "Regular production" = regular,
    "Overtime production" = overtime,
    "Subcontract production" = subcontract,
    "Total production" = regular + overtime + subcontract,
    "Ending inventory" = pmax(ending_net, 0),
    "Ending backlog" = pmax(-ending_net, 0),
    check.names = FALSE
  )
  tab[["Regular cost"]] <- regular * params$regular_cost
  tab[["Overtime cost"]] <- 0
  tab[["Subcontract cost"]] <- 0
  tab[["Hiring cost"]] <- hires * params$hire_cost
  tab[["Layoff cost"]] <- layoffs * params$layoff_cost
  tab[["Holding cost"]] <- tab[["Ending inventory"]] * params$holding_cost
  tab[["Backlog cost"]] <- tab[["Ending backlog"]] * params$backlog_cost
  tab[["Period cost"]] <- rowSums(tab[c(
    "Regular cost", "Overtime cost", "Subcontract cost", "Hiring cost",
    "Layoff cost", "Holding cost", "Backlog cost"
  )])
  attr(tab, "label") <- label
  tab
}

hp_apply_extra_costs <- function(tab, overtime_cost = 0, subcontract_cost = 0) {
  tab[["Overtime cost"]] <- tab[["Overtime production"]] * overtime_cost
  tab[["Subcontract cost"]] <- tab[["Subcontract production"]] * subcontract_cost
  tab[["Period cost"]] <- rowSums(tab[c(
    "Regular cost", "Overtime cost", "Subcontract cost", "Hiring cost",
    "Layoff cost", "Holding cost", "Backlog cost"
  )])
  tab
}

hp_summary_row <- function(tab, plan_name) {
  data.frame(
    Plan = plan_name,
    "Total cost" = sum(tab[["Period cost"]]),
    "Regular production cost" = sum(tab[["Regular cost"]]),
    "Overtime cost" = sum(tab[["Overtime cost"]]),
    "Subcontract cost" = sum(tab[["Subcontract cost"]]),
    "Hiring cost" = sum(tab[["Hiring cost"]]),
    "Layoff cost" = sum(tab[["Layoff cost"]]),
    "Holding cost" = sum(tab[["Holding cost"]]),
    "Backlog cost" = sum(tab[["Backlog cost"]]),
    "Ending inventory" = tail(tab[["Ending inventory"]], 1),
    "Ending backlog" = tail(tab[["Ending backlog"]], 1),
    check.names = FALSE
  )
}

hp_plot_plan <- function(tab, title) {
  old_par <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
  on.exit(par(old_par), add = TRUE)
  production <- rbind(
    Regular = tab[["Regular production"]],
    Overtime = tab[["Overtime production"]],
    Subcontract = tab[["Subcontract production"]]
  )
  centers <- barplot(
    production, beside = FALSE, col = c("#2C7BB6", "#FDAE61", "#6A8E3A"),
    border = NA, names.arg = tab$Period, xlab = "Period", ylab = "Units",
    main = paste(title, "— demand and production")
  )
  lines(centers, tab$Demand, type = "o", pch = 19, lwd = 2, col = "#3B3B3B")
  legend("topleft", bty = "n", ncol = 2,
         legend = c("Regular", "Overtime", "Subcontract", "Demand"),
         fill = c("#2C7BB6", "#FDAE61", "#6A8E3A", NA),
         border = c(NA, NA, NA, NA), lty = c(NA, NA, NA, 1),
         pch = c(NA, NA, NA, 19), col = c(NA, NA, NA, "#3B3B3B"))

  ymax <- max(c(tab[["Ending inventory"]], tab[["Ending backlog"]], 1))
  plot(tab$Period, tab[["Ending inventory"]], type = "o", pch = 19,
       ylim = c(0, ymax * 1.1), xlab = "Period", ylab = "Units",
       col = "#2C7BB6", main = "Ending inventory and backlog", xaxt = "n")
  axis(1, at = tab$Period)
  lines(tab$Period, tab[["Ending backlog"]], type = "o", pch = 17, col = "#D95F02")
  legend("topleft", bty = "n", legend = c("Inventory", "Backlog"),
         col = c("#2C7BB6", "#D95F02"), pch = c(19, 17), lty = 1)
}

hp_results_ui <- function(ns, method = NULL) {
  tagList(
    h3(textOutput(ns("headline"))),
    uiOutput(ns("notes")),
    h4("Cost comparison"),
    div(style = "overflow-x: auto;", tableOutput(ns("cost_summary"))),
    plotOutput(ns("plot"), height = "560px"),
    h4("Plan by period"),
    div(style = "overflow-x: auto;", tableOutput(ns("plan_table"))),
    downloadButton(ns("download"), "Download selected plan (CSV)"),
    br(), br(),
    if (!is.null(method)) wellPanel(h4("Method"), method)
  )
}

hp_results_server <- function(output, session, plans, display, prefix) {
  selected <- reactive({
    result <- plans()
    key <- display()
    if (!key %in% names(result$plans)) key <- names(result$plans)[1]
    result$plans[[key]]
  })

  output$headline <- renderText({
    result <- plans()
    tab <- selected()
    sprintf("%s — %s — total cost: %s",
            result$title, attr(tab, "label"), fc_fmt(sum(tab[["Period cost"]]), 2))
  })
  output$notes <- renderUI({
    notes <- plans()$notes
    if (length(notes) == 0) return(NULL)
    div(class = "text-muted", lapply(notes, p))
  })
  output$cost_summary <- renderTable({
    result <- plans()
    do.call(rbind, Map(hp_summary_row, result$plans, result$plan_names))
  }, digits = 2)
  output$plot <- renderPlot({
    hp_plot_plan(selected(), plans()$title)
  })
  output$plan_table <- renderTable(selected(), digits = 2, na = "")
  output$download <- downloadHandler(
    filename = function() paste0(prefix, "_", display(), ".csv"),
    content = function(file) utils::write.csv(selected(), file, row.names = FALSE)
  )
}
