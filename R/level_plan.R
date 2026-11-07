# =============================================================================
# Constant-workforce (Level) aggregate plan
# A constant workforce produces the same quantity every period.  Inventory or
# backlog absorbs the difference between production and demand.
# =============================================================================

hp_level_plan <- function(demand, params, round_workforce = FALSE) {
  total_required <- max(0, sum(demand) + params$target_inventory - params$initial_inventory)
  workers <- total_required / (length(demand) * params$productivity)
  if (round_workforce) workers <- ceiling(workers)
  workforce <- rep(workers, length(demand))
  regular <- workforce * params$productivity
  hp_make_plan(
    demand, workforce, regular, params,
    label = if (round_workforce) "Workforce rounded up" else "Fractional workforce"
  )
}

level_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Constant-workforce (Level) Plan"),
      hp_common_ui(ns),
      hr(), hp_display_ui(ns)
    ),
    mainPanel(
      hp_results_ui(ns, method = tagList(
        p("One constant workforce is used for the entire planning horizon."),
        p("Workforce = total net requirement / (number of periods x production per worker)."),
        p("Production above demand becomes inventory; unmet demand becomes backlog."),
        p("Hiring or layoffs occur once, at the start of period 1.")
      ))
    )
  )
}

level_plan_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    plans <- reactive({
      s <- series()
      params <- tryCatch(hp_parameters(input),
                         error = function(e) validate(need(FALSE, conditionMessage(e))))
      validate(need(all(s$values >= 0), "Demand cannot contain negative values."))
      exact <- hp_level_plan(s$values, params, FALSE)
      rounded <- hp_level_plan(s$values, params, TRUE)
      notes <- character(0)
      if (s$dropped > 0) notes <- sprintf("%d empty / non-numeric cells were ignored.", s$dropped)
      list(
        title = "Constant-workforce (Level) Plan",
        plans = list(exact = exact, rounded = rounded),
        plan_names = c("Fractional workforce", "Workforce rounded up"),
        notes = notes
      )
    })
    hp_results_server(output, session, plans, reactive(input$display), "level_plan")
  })
}
