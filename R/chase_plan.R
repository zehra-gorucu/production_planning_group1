# =============================================================================
# Zero-inventory (Chase) aggregate plan
# Workforce follows the net requirement in every period.  With fractional
# workers, regular production exactly covers demand after available inventory.
# The rounded alternative takes the ceiling of the required workforce.
# =============================================================================

hp_chase_plan <- function(demand, params, round_workforce = FALSE) {
  n <- length(demand)
  workforce <- regular <- numeric(n)
  net <- params$initial_inventory
  for (t in seq_len(n)) {
    target <- if (t == n) params$target_inventory else 0
    required <- max(0, demand[t] + target - net)
    workers <- required / params$productivity
    if (round_workforce) workers <- ceiling(workers)
    workforce[t] <- workers
    regular[t] <- workers * params$productivity
    net <- net + regular[t] - demand[t]
  }
  hp_make_plan(
    demand, workforce, regular, params,
    label = if (round_workforce) "Workforce rounded up" else "Fractional workforce"
  )
}

chase_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Zero-inventory (Chase) Plan"),
      hp_common_ui(ns),
      hr(), hp_display_ui(ns)
    ),
    mainPanel(
      hp_results_ui(ns, method = tagList(
        p("The workforce changes each period so regular production follows the net demand requirement."),
        p("Required workforce = required production / production per worker."),
        p("The rounded alternative takes the ceiling of the workforce. Any resulting excess production",
          "is carried as inventory and reduces the following period's requirement.")
      ))
    )
  )
}

chase_plan_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    plans <- reactive({
      s <- series()
      params <- tryCatch(hp_parameters(input),
                         error = function(e) validate(need(FALSE, conditionMessage(e))))
      validate(need(all(s$values >= 0), "Demand cannot contain negative values."))
      exact <- hp_chase_plan(s$values, params, FALSE)
      rounded <- hp_chase_plan(s$values, params, TRUE)
      notes <- character(0)
      if (s$dropped > 0) notes <- sprintf("%d empty / non-numeric cells were ignored.", s$dropped)
      list(
        title = "Zero-inventory (Chase) Plan",
        plans = list(exact = exact, rounded = rounded),
        plan_names = c("Fractional workforce", "Workforce rounded up"),
        notes = notes
      )
    })
    hp_results_server(output, session, plans, reactive(input$display), "chase_plan")
  })
}
