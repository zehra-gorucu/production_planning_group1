# =============================================================================
# Constant-workforce (Level) Plan          (Aggregate Planning > Heuristic plans)
# Nahmias & Olsen Section 3.4; lecture slides "Constant Work Force Plan"
# One workforce for the whole horizon, the minimum that causes no shortage:
#   W = max over t of  (cumulative net demand_t) / (K * cumulative working days_t)
# The period that gives the maximum is the "critical" period.
# =============================================================================

ap_level_workers <- function(s, round_up = FALSE) {
  ratio <- cumsum(s$net) / (s$K * cumsum(s$days))
  w <- max(0, ratio)
  if (round_up) w <- ceiling(w - 1e-9)
  list(W = rep(w, s$n), ratio = ratio, critical = which.max(ratio))
}

level_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Constant-workforce (Level) Plan"),
      ap_setup_info_ui(ns),
      helpText("No extra inputs are needed: the app finds the smallest constant workforce",
               "that never runs short."),
      h4("Required workers by period"),
      tableOutput(ns("ratio_table"))
    ),
    mainPanel(
      ap_results_ui(ns, method = tagList(
        p("The workforce is kept constant; inventory absorbs the differences between",
          "production and demand. To avoid shortages the cumulative production line must stay",
          "above the cumulative net demand line, which gives"),
        p("$$W = \\max_t \\frac{\\sum_{i \\le t} D'_i}{K \\sum_{i \\le t} n_i}$$"),
        p("Hiring or firing happens once, at the start of the first period. The whole-worker",
          "plan rounds W up.")
      ))
    )
  )
}

level_plan_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      s <- setup()
      exact <- ap_level_workers(s, FALSE)
      rounded <- ap_level_workers(s, TRUE)
      plans <- ap_two_plans(s, exact$W, rounded$W)
      list(title = "Constant-workforce (Level) Plan", plans = plans, ratio = exact$ratio,
           notes = c(sprintf("Critical period: %s, where %.3f workers are needed. Whole workers: %d.",
                             s$periods[exact$critical], exact$W[1], as.integer(rounded$W[1])),
                     ap_rounding_note(plans)))
    })

    output$ratio_table <- renderTable({
      s <- setup()
      r <- res()
      data.frame(Period = s$periods, "Cum. net demand" = cumsum(s$net),
                 "Cum. days" = cumsum(s$days), "Workers needed" = r$ratio, check.names = FALSE)
    }, digits = 2)

    ap_results_server(input, output, session, setup, res, "level_plan")
    res
  })
}
