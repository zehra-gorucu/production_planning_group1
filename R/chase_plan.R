# =============================================================================
# Zero-inventory (Chase) Plan             (Aggregate Planning > Heuristic plans)
# Nahmias & Olsen Section 3.4; lecture slides "Zero Inventory Plan"
# The workforce changes every period so that production matches net demand:
#   W_t = (net requirement in t) / (K n_t)
# The net requirement subtracts any inventory left from earlier periods, so
# after rounding up the next periods are "adjusted downward" (slides).
# =============================================================================

# adjust = FALSE: every period is rounded up on its own (Nahmias Example 3.1).
# adjust = TRUE : inventory left over from rounding lowers the next periods'
#                 requirement ("rounded up and possibly adjusted downward", slides).
ap_chase_workers <- function(s, round_up = FALSE, adjust = FALSE) {
  cum_net <- cumsum(s$net)
  W <- numeric(s$n)
  cum_p <- 0
  for (t in seq_len(s$n)) {
    # Net requirement of period t; inventory left from the starting stock is
    # always used, inventory created by rounding only when adjust = TRUE.
    exact_cum_p <- if (t > 1) max(0, cum_net[t - 1]) else 0
    used_before <- if (adjust || !round_up) cum_p else exact_cum_p
    requirement <- max(0, cum_net[t] - used_before)
    w <- requirement / s$kn[t]
    if (round_up) w <- ceiling(w - 1e-9)
    W[t] <- w
    cum_p <- cum_p + w * s$kn[t]
  }
  W
}

chase_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Zero-inventory (Chase) Plan"),
      ap_setup_info_ui(ns),
      radioButtons(ns("rounding"), "Whole-worker plan",
                   choices = c("Round each period up on its own (book, Example 3.1)" = "book",
                               "Round up, then use the extra units to lower the next periods (slides)" = "adjust")),
      helpText("The workforce follows net demand in every period.")
    ),
    mainPanel(
      ap_results_ui(ns, method = tagList(
        p("Each period, hire or fire so that production equals the net requirement:"),
        p("$$W_t = \\frac{\\max(0,\\ \\text{cumulative net demand}_t - \\text{cumulative production}_{t-1})}{K n_t}$$"),
        p("The original plan keeps fractional workers and ends every period with zero inventory",
          "above net demand. The whole-worker plan rounds \\(W_t\\) up. In the book's version each",
          "period is rounded on its own and the extra units stay in inventory; in the slides'",
          "version the extra units reduce the requirement (and the workers) of the next periods.")
      ))
    )
  )
}

chase_plan_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      s <- setup()
      adjust <- identical(input$rounding, "adjust")
      plans <- ap_two_plans(s, ap_chase_workers(s, FALSE), ap_chase_workers(s, TRUE, adjust))
      list(title = "Zero-inventory (Chase) Plan", plans = plans,
           notes = c(sprintf("Whole-worker plan: %s workers hired and %s fired in total.",
                             fc_fmt(plans[[2]]$hired, 0), fc_fmt(plans[[2]]$fired, 0)),
                     ap_rounding_note(plans)))
    })
    ap_results_server(input, output, session, setup, res, "chase_plan")
    res
  })
}
