# =============================================================================
# Mixed Plan                               (Aggregate Planning > Heuristic plans)
# Lecture slides "Cost Reduction in Constant Work Force Plan"
# The user chooses the periods in which the workforce changes. Between two
# changes the workforce is constant and is the smallest level that causes no
# shortage, starting from the inventory left by the previous segment:
#   W_seg = max over t in segment of
#           (cumulative net demand from segment start to t - inventory carried in)
#           / (K * cumulative working days from segment start to t)
# With no change periods the mixed plan equals the level plan.
# =============================================================================

ap_mixed_workers <- function(s, change_periods, round_up = FALSE) {
  n <- s$n
  starts <- sort(unique(c(1L, as.integer(change_periods))))
  starts <- starts[starts >= 1 & starts <= n]
  ends <- c(starts[-1] - 1L, n)
  cum_net <- cumsum(s$net)
  W <- numeric(n)
  cum_p <- 0
  segments <- vector("list", length(starts))
  for (k in seq_along(starts)) {
    idx <- starts[k]:ends[k]
    carried <- cum_p - (if (starts[k] > 1) cum_net[starts[k] - 1] else 0)
    need <- cumsum(s$net[idx]) - carried
    ratio <- need / (s$K * cumsum(s$days[idx]))
    w <- max(0, ratio)
    if (round_up) w <- ceiling(w - 1e-9)
    W[idx] <- w
    cum_p <- cum_p + sum(w * s$kn[idx])
    segments[[k]] <- data.frame(Segment = sprintf("%s to %s", s$periods[starts[k]], s$periods[ends[k]]),
                                "Inventory carried in" = carried, "Critical period" = s$periods[idx[which.max(ratio)]],
                                Workers = w, check.names = FALSE)
  }
  list(W = W, segments = do.call(rbind, segments))
}

mixed_plan_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Mixed Plan"),
      ap_setup_info_ui(ns),
      selectizeInput(ns("changes"), "Periods in which the workforce changes",
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Choose one or more periods")),
      helpText("The workforce is constant between two chosen periods. Example from the slides:",
               "change in April and in June."),
      h4("Workforce of each segment"),
      tableOutput(ns("segment_table"))
    ),
    mainPanel(
      ap_results_ui(ns, method = tagList(
        p("A level plan is used inside each segment between the chosen change periods. The",
          "workforce of a segment is the smallest that causes no shortage until the segment ends,",
          "taking into account the inventory carried in from the previous segment:"),
        p("$$W_{seg} = \\max_{t \\in seg} \\frac{\\sum_{i=a}^{t} D'_i - I_{carried}}{K \\sum_{i=a}^{t} n_i}$$"),
        p("where a is the first period of the segment. Workers are hired or fired only at the",
          "start of a segment. The whole-worker plan rounds every segment's workforce up.")
      ))
    )
  )
}

mixed_plan_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    # Offer periods 2..n as change points; keep the user's choice when possible.
    observeEvent(setup()$periods, {
      s <- setup()
      choices <- setNames(as.character(2:s$n), s$periods[-1])
      keep <- intersect(isolate(input$changes), choices)
      if (length(keep) == 0 && identical(s$preset, "Constant workforce example, 8 months (Slides)")) {
        keep <- c("4", "6")
      }
      updateSelectizeInput(session, "changes", choices = choices, selected = keep)
    })

    res <- reactive({
      s <- setup()
      changes <- as.integer(input$changes %||% character(0))
      changes <- changes[!is.na(changes) & changes >= 2 & changes <= s$n]
      exact <- ap_mixed_workers(s, changes, FALSE)
      rounded <- ap_mixed_workers(s, changes, TRUE)
      plans <- ap_two_plans(s, exact$W, rounded$W)
      notes <- if (length(changes) == 0) {
        "No change period is chosen, so this is the same as the constant-workforce plan."
      } else {
        sprintf("Workforce changes at the start of: %s.", paste(s$periods[sort(changes)], collapse = ", "))
      }
      list(title = "Mixed Plan", plans = plans, segments = exact$segments,
           segments_rounded = rounded$segments, notes = c(notes, ap_rounding_note(plans)))
    })

    output$segment_table <- renderTable({
      r <- res()
      tab <- r$segments[, c("Segment", "Critical period", "Workers")]
      tab[["Whole workers"]] <- as.integer(r$segments_rounded$Workers)
      tab
    }, digits = 2)

    ap_results_server(input, output, session, setup, res, "mixed_plan")
    res
  })
}
