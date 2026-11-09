# =============================================================================
# Transportation Method                    (Aggregate Planning > Optimization)
# Lecture slides "Transportation method for aggregate planning"
# Sources: regular time, overtime and subcontracting of every period.
# Destinations: the net demand of every period plus an "Excess capacity" column.
# Cost of producing in period i for period j (j >= i):
#     unit cost of the source + holding cost x (j - i)
# Production cannot be used before it is made (no backorders), so cells with
# j < i are not allowed. Two solutions are shown: the north-west corner
# starting solution and the cheapest-cell-first (optimal) solution. An exact LP
# solution is computed as a check.
# =============================================================================

# Net demand without a negative first period: starting stock that exceeds
# period 1 demand is carried to the next periods.
ap_transport_demand <- function(s) {
  d <- s$demand
  stock <- s$I0
  for (t in seq_along(d)) {
    use <- min(stock, d[t])
    d[t] <- d[t] - use
    stock <- stock - use
  }
  d[length(d)] <- d[length(d)] + s$IT
  d
}

ap_transport_build <- function(s, rt_cap, ot_cap, sc_cap, rt_cost) {
  n <- s$n
  modes <- c("Regular time", "Overtime", "Subcontracting")
  unit <- c(rt_cost, s$costs$ot, s$costs$sc)
  period <- rep(seq_len(n), each = 3)
  mode <- rep(modes, times = n)
  supply <- as.vector(rbind(rt_cap, ot_cap, sc_cap))
  demand <- ap_transport_demand(s)
  cost <- outer(seq_along(period), seq_len(n), function(r, j) {
    ifelse(j >= period[r], unit[match(mode[r], modes)] + s$costs$hold * (j - period[r]), NA)
  })
  cum_supply <- cumsum(tapply(supply, period, sum))
  short <- which(cum_supply + 1e-9 < cumsum(demand))
  if (length(short)) {
    stop(sprintf("Not enough capacity: by %s cumulative capacity is %s but cumulative net demand is %s.",
                 s$periods[short[1]], fc_fmt(cum_supply[short[1]]), fc_fmt(cumsum(demand)[short[1]])))
  }
  list(period = period, mode = mode, supply = supply, demand = demand, cost = cost,
       excess = sum(supply) - sum(demand), unit = setNames(unit, modes))
}

ap_northwest <- function(tp) {
  nr <- length(tp$supply); nc <- length(tp$demand)
  A <- matrix(0, nr, nc + 1)
  rs <- tp$supply; rd <- c(tp$demand, tp$excess)
  i <- 1; j <- 1
  while (i <= nr && j <= nc + 1) {
    q <- min(rs[i], rd[j])
    A[i, j] <- A[i, j] + q
    rs[i] <- rs[i] - q; rd[j] <- rd[j] - q
    if (rs[i] <= 1e-9) i <- i + 1 else j <- j + 1
  }
  A
}

# Cheapest cell first: fill the periods in order, each from the cheapest
# source that still has capacity; what is left goes to the excess column.
ap_cheapest <- function(tp) {
  nr <- length(tp$supply); nc <- length(tp$demand)
  A <- matrix(0, nr, nc + 1)
  rs <- tp$supply
  for (j in seq_len(nc)) {
    need <- tp$demand[j]
    while (need > 1e-9) {
      cand <- which(!is.na(tp$cost[, j]) & rs > 1e-9)
      r <- cand[which.min(tp$cost[cand, j])]
      q <- min(rs[r], need)
      A[r, j] <- A[r, j] + q
      rs[r] <- rs[r] - q; need <- need - q
    }
  }
  A[, nc + 1] <- rs
  A
}

ap_transport_exact <- function(tp) {
  big <- 1e7
  C <- cbind(ifelse(is.na(tp$cost), big, tp$cost), 0)
  # integers = NULL: capacities K n_t W_t are usually fractional, and
  # lp.transport would otherwise treat every cell as an integer variable.
  fit <- lpSolve::lp.transport(C, "min", rep("=", length(tp$supply)), tp$supply,
                               rep("=", ncol(C)), c(tp$demand, tp$excess), integers = NULL)
  if (fit$status != 0) stop("The exact transportation problem could not be solved.")
  fit$solution
}

ap_transport_cost <- function(tp, A) {
  n <- length(tp$demand)
  C <- tp$cost
  C[is.na(C)] <- 0
  sum(A[, seq_len(n)] * C)
}

# Plan table and cost items for one allocation.
ap_transport_plan <- function(s, tp, A, workers, rt_cost) {
  n <- s$n
  used <- rowSums(A[, seq_len(n), drop = FALSE])
  by_mode <- function(m) vapply(seq_len(n), function(t) sum(used[tp$period == t & tp$mode == m]), numeric(1))
  rt <- by_mode("Regular time"); ot <- by_mode("Overtime"); sc <- by_mode("Subcontracting")
  production <- rt + ot + sc
  inv <- s$I0 + cumsum(production - s$demand)
  carried <- sum(vapply(seq_len(nrow(A)), function(r) {
    sum(A[r, seq_len(n)] * pmax(seq_len(n) - tp$period[r], 0))
  }, numeric(1)))
  tab <- data.frame(Period = s$periods, Demand = s$demand, "Net demand" = tp$demand,
                    check.names = FALSE)
  if (!is.null(workers)) {
    prevW <- c(s$W0, workers[-n])
    tab$Workers <- workers
    tab$Hired <- pmax(workers - prevW, 0)
    tab$Fired <- pmax(prevW - workers, 0)
  }
  tab[["Regular time"]] <- rt
  tab[["Overtime"]] <- ot
  tab[["Subcontracted"]] <- sc
  tab[["Total production"]] <- production
  tab[["Cumulative production"]] <- cumsum(production)
  tab[["Cumulative net demand"]] <- cumsum(tp$demand)
  tab[["Ending inventory"]] <- inv
  items <- c("Regular time" = rt_cost * sum(rt), "Overtime" = s$costs$ot * sum(ot),
             "Subcontracting" = s$costs$sc * sum(sc), "Holding" = s$costs$hold * carried)
  if (!is.null(workers)) {
    items["Hiring"] <- s$costs$hire * sum(tab$Hired)
    items["Firing"] <- s$costs$fire * sum(tab$Fired)
    if (identical(s$costs$reg_basis, "worker_day")) items["Regular time"] <- s$costs$reg * sum(workers * s$days)
  }
  list(table = tab, production = production, costs = items, allocation = A)
}

transportation_method_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Transportation Method"),
      ap_setup_info_ui(ns),
      h4("Capacities"),
      radioButtons(ns("rt_mode"), "Regular-time capacity",
                   choices = c("From the workforce: K n_t W_t" = "workforce",
                               "Enter units per period" = "units")),
      conditionalPanel(
        "input.rt_mode == 'workforce'", ns = ns,
        textInput(ns("workforce"), "Workforce in each period (one value or a list)", "300")
      ),
      conditionalPanel(
        "input.rt_mode == 'units'", ns = ns,
        textInput(ns("rt_units"), "Regular-time capacity per period (units)", "1000")
      ),
      radioButtons(ns("ot_mode"), "Overtime capacity",
                   choices = c("% of regular-time capacity" = "pct", "Units per period" = "units")),
      conditionalPanel(
        "input.ot_mode == 'pct'", ns = ns,
        numericInput(ns("ot_pct"), "Overtime capacity (%)", value = 20, min = 0)
      ),
      conditionalPanel(
        "input.ot_mode == 'units'", ns = ns,
        textInput(ns("ot_units"), "Overtime capacity per period (units)", "100")
      ),
      textInput(ns("sc_units"), "Subcontracting capacity per period (units)", "500"),
      helpText("Unit costs and the holding cost come from Plan Setup."),
      hr(),
      radioButtons(ns("show"), "Tableau shown",
                   choices = c("North-west corner (starting solution)" = "Northwest corner",
                               "Cheapest cell first (optimal)" = "Cheapest cell first"),
                   selected = "Cheapest cell first")
    ),
    mainPanel(
      ap_results_ui(ns, method = tagList(
        p("Each period has three sources (regular time, overtime, subcontracting) and each",
          "period's net demand is a destination. A unit made in period i for period j costs its",
          "unit cost plus \\(c_I (j - i)\\) holding cost. Units cannot be sent to an earlier period.",
          "An Excess column absorbs unused capacity at zero cost."),
        p("North-west corner: start at the top-left cell and fill row by row and column by column,",
          "ignoring costs. It gives a feasible starting solution."),
        p("Cheapest cell first: fill the periods in order, each time using the cheapest source",
          "that still has capacity. For this problem it gives the optimal plan; the app also",
          "checks the result with an exact linear programming solution.")
      )),
      h4("Transportation tableau"),
      helpText("Each cell shows the allocated units; empty cells are not used, '–' means not allowed."),
      div(style = "overflow-x: auto;", tableOutput(ns("tableau"))),
      h4("Unit cost of each cell"),
      div(style = "overflow-x: auto;", tableOutput(ns("cost_table")))
    )
  )
}

transportation_method_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    # Example presets for this module
    observeEvent(setup()$preset, {
      preset <- setup()$preset
      tr <- if (!is.null(preset)) ap_examples[[preset]]$transport else NULL
      s <- setup()
      if (is.null(tr)) {
        # Start from the whole-worker level plan, which always has enough capacity.
        level_w <- ceiling(max(0, cumsum(s$net) / (s$K * cumsum(s$days))) - 1e-9)
        updateRadioButtons(session, "rt_mode", selected = "workforce")
        updateTextInput(session, "workforce", value = as.character(level_w))
        updateRadioButtons(session, "ot_mode", selected = "pct")
        updateTextInput(session, "sc_units", value = "0")
      } else {
        updateRadioButtons(session, "rt_mode", selected = tr$rt_mode)
        updateTextInput(session, "workforce", value = tr$workforce)
        updateRadioButtons(session, "ot_mode", selected = tr$ot_mode)
        updateTextInput(session, "ot_units", value = tr$ot_units)
        updateTextInput(session, "sc_units", value = tr$sc_units)
      }
    }, ignoreNULL = FALSE)

    model <- reactive({
      s <- setup()
      parse <- function(text, label) tryCatch(ap_parse_list(text, s$n, label),
                                              error = function(e) validate(need(FALSE, conditionMessage(e))))
      workers <- NULL
      if (identical(input$rt_mode, "units")) {
        rt_cap <- parse(input$rt_units, "Regular-time capacity")
      } else {
        workers <- parse(input$workforce, "Workforce")
        rt_cap <- s$kn * workers
      }
      ot_cap <- if (identical(input$ot_mode, "units")) parse(input$ot_units, "Overtime capacity") else {
        validate(need(is.finite(input$ot_pct) && input$ot_pct >= 0, "Overtime % must be non-negative."))
        rt_cap * input$ot_pct / 100
      }
      sc_cap <- parse(input$sc_units, "Subcontracting capacity")
      # With payroll costing, regular time is paid for every worker-day, so
      # using a regular-time unit costs nothing extra in the tableau.
      rt_cost <- if (!is.null(workers) && identical(s$costs$reg_basis, "worker_day")) 0 else s$costs$reg
      tp <- tryCatch(ap_transport_build(s, rt_cap, ot_cap, sc_cap, rt_cost),
                     error = function(e) validate(need(FALSE, conditionMessage(e))))
      list(s = s, tp = tp, workers = workers, rt_cost = rt_cost)
    })

    res <- reactive({
      m <- model()
      nw <- ap_transport_plan(m$s, m$tp, ap_northwest(m$tp), m$workers, m$rt_cost)
      ch <- ap_transport_plan(m$s, m$tp, ap_cheapest(m$tp), m$workers, m$rt_cost)
      exact <- ap_transport_exact(m$tp)
      gap <- ap_transport_cost(m$tp, ch$allocation) - ap_transport_cost(m$tp, exact)
      notes <- c(
        sprintf("Tableau cost (production + holding): north-west corner %s, cheapest cell first %s, exact LP %s.",
                fc_fmt(ap_transport_cost(m$tp, nw$allocation)), fc_fmt(ap_transport_cost(m$tp, ch$allocation)),
                fc_fmt(ap_transport_cost(m$tp, exact))),
        if (abs(gap) < 1e-6) "The cheapest-cell-first solution is optimal (same cost as the exact LP)."
        else sprintf("The cheapest-cell-first solution is %s above the exact LP optimum.", fc_fmt(gap)),
        sprintf("Unused (excess) capacity: %s units.", fc_fmt(m$tp$excess))
      )
      list(title = "Transportation Method",
           plans = list("Northwest corner" = nw, "Cheapest cell first" = ch), notes = notes)
    })

    row_labels <- function(m) sprintf("%s – %s (%s)", m$s$periods[m$tp$period], m$tp$mode,
                                      fc_fmt(m$tp$unit[m$tp$mode]))

    output$tableau <- renderTable({
      m <- model()
      r <- res()
      A <- r$plans[[input$show %||% "Cheapest cell first"]]$allocation
      n <- m$s$n
      cells <- matrix("", nrow(A), n + 1)
      for (i in seq_len(nrow(A))) for (j in seq_len(n + 1)) {
        if (j <= n && is.na(m$tp$cost[i, j])) cells[i, j] <- "–"
        else if (A[i, j] > 1e-9) cells[i, j] <- fc_fmt(A[i, j], 1)
      }
      tab <- data.frame(Source = row_labels(m), cells, Capacity = fc_fmt(m$tp$supply, 1),
                        check.names = FALSE, stringsAsFactors = FALSE)
      names(tab)[2:(n + 2)] <- c(m$s$periods, "Excess")
      total <- c("Net demand", fc_fmt(m$tp$demand, 1), fc_fmt(m$tp$excess, 1), fc_fmt(sum(m$tp$supply), 1))
      rbind(tab, setNames(as.list(total), names(tab)))
    }, align = "l")

    output$cost_table <- renderTable({
      m <- model()
      cells <- ifelse(is.na(m$tp$cost), "–", fc_fmt(m$tp$cost, 2))
      tab <- data.frame(Source = row_labels(m), cells, check.names = FALSE, stringsAsFactors = FALSE)
      names(tab)[-1] <- m$s$periods
      tab
    }, align = "l")

    ap_results_server(input, output, session, setup, res, "transportation_method")
    res
  })
}
