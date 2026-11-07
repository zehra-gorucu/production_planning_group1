# =============================================================================
# Mixed aggregate plan
# A constant regular workforce supplies a chosen share of average requirements.
# Overtime and then subcontracting cover shortages up to their capacities;
# remaining shortages become backlog.
# =============================================================================

hp_mixed_plan <- function(demand, params, regular_workforce,
                          overtime_capacity, subcontract_capacity,
                          overtime_cost, subcontract_cost,
                          round_workforce = FALSE) {
  n <- length(demand)
  workers <- max(0, regular_workforce)
  if (round_workforce) workers <- ceiling(workers)
  workforce <- rep(workers, n)
  regular <- workforce * params$productivity
  overtime <- subcontract <- numeric(n)
  net <- params$initial_inventory
  for (t in seq_len(n)) {
    target <- if (t == n) params$target_inventory else 0
    shortage <- max(0, demand[t] + target - (net + regular[t]))
    overtime[t] <- min(shortage, overtime_capacity)
    shortage <- shortage - overtime[t]
    subcontract[t] <- min(shortage, subcontract_capacity)
    net <- net + regular[t] + overtime[t] + subcontract[t] - demand[t]
  }
  tab <- hp_make_plan(
    demand, workforce, regular, params, overtime, subcontract,
    label = if (round_workforce) "Workforce rounded up" else "Fractional workforce"
  )
  hp_apply_extra_costs(tab, overtime_cost, subcontract_cost)
}

mixed_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Mixed Plan"),
      hp_common_ui(ns),
      hr(),
      h4("Mixed-plan choices"),
      radioButtons(
        ns("workforce_mode"), "Regular workforce",
        choices = c("Use a share of the average requirement" = "share",
                    "Enter the regular workforce" = "manual")
      ),
      conditionalPanel(
        "input.workforce_mode == 'share'", ns = ns,
        sliderInput(ns("regular_share"), "Share covered by regular workforce",
                    min = 0.1, max = 1, value = 0.8, step = 0.05)
      ),
      conditionalPanel(
        "input.workforce_mode == 'manual'", ns = ns,
        numericInput(ns("regular_workforce"), "Constant regular workforce", value = 9, min = 0, step = 0.1)
      ),
      numericInput(ns("overtime_capacity"), "Maximum overtime production per period",
                   value = 300, min = 0, step = 10),
      numericInput(ns("overtime_cost"), "Overtime cost per unit", value = 30, min = 0),
      numericInput(ns("subcontract_capacity"), "Maximum subcontract production per period",
                   value = 300, min = 0, step = 10),
      numericInput(ns("subcontract_cost"), "Subcontract cost per unit", value = 35, min = 0),
      hr(), hp_display_ui(ns)
    ),
    mainPanel(
      hp_results_ui(ns, method = tagList(
        p("A constant regular workforce supplies the base production."),
        p("When regular production and available inventory are insufficient, the plan uses",
          "overtime first, then subcontracting. Any remaining shortage becomes backlog."),
        p("The automatic option sets regular workforce to the selected share of the average",
          "net requirement. The rounded alternative takes its ceiling.")
      ))
    )
  )
}

mixed_plan_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    plans <- reactive({
      s <- series()
      params <- tryCatch(hp_parameters(input),
                         error = function(e) validate(need(FALSE, conditionMessage(e))))
      validate(need(all(s$values >= 0), "Demand cannot contain negative values."))
      extra <- c(input$overtime_capacity, input$overtime_cost,
                 input$subcontract_capacity, input$subcontract_cost)
      validate(need(all(is.finite(extra)) && all(extra >= 0),
                    "Mixed-plan capacity and cost inputs must be non-negative numbers."))

      average_workers <- max(0, (sum(s$values) + params$target_inventory -
                                   params$initial_inventory) /
                               (length(s$values) * params$productivity))
      if (identical(input$workforce_mode, "manual")) {
        validate(need(is.finite(input$regular_workforce) && input$regular_workforce >= 0,
                      "Regular workforce must be a non-negative number."))
        regular_workforce <- input$regular_workforce
        source_note <- sprintf("Entered constant regular workforce: %.3f.", regular_workforce)
      } else {
        share <- input$regular_share
        validate(need(is.finite(share) && share > 0 && share <= 1,
                      "The regular-workforce share must be between 0 and 1."))
        regular_workforce <- average_workers * share
        source_note <- sprintf(
          "Regular workforce covers %.0f%% of the average net requirement (%.3f workers).",
          100 * share, regular_workforce
        )
      }

      exact <- hp_mixed_plan(
        s$values, params, regular_workforce,
        input$overtime_capacity, input$subcontract_capacity,
        input$overtime_cost, input$subcontract_cost, FALSE
      )
      rounded <- hp_mixed_plan(
        s$values, params, regular_workforce,
        input$overtime_capacity, input$subcontract_capacity,
        input$overtime_cost, input$subcontract_cost, TRUE
      )
      notes <- source_note
      if (s$dropped > 0) notes <- c(notes, sprintf(
        "%d empty / non-numeric cells were ignored.", s$dropped
      ))
      if (tail(exact[["Ending backlog"]], 1) > 0) notes <- c(notes,
        "The fractional plan ends with backlog because overtime and subcontract capacity are insufficient.")
      list(
        title = "Mixed Plan",
        plans = list(exact = exact, rounded = rounded),
        plan_names = c("Fractional workforce", "Workforce rounded up"),
        notes = notes
      )
    })
    hp_results_server(output, session, plans, reactive(input$display), "mixed_plan")
  })
}
