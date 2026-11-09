# =============================================================================
# Linear Programming aggregate plan        (Aggregate Planning > Optimization)
# Nahmias & Olsen Section 3.5
#   min  sum( cH H_t + cF F_t + cI I_t + regular time + cO O_t + cU U_t + cS S_t )
#   s.t. W_t = W_{t-1} + H_t - F_t                      (conservation of workforce)
#        P_t = K n_t W_t + O_t - U_t                    (production and workforce)
#        I_t = I_{t-1} + P_t + S_t - D_t                (inventory balance)
#        I_T >= required ending inventory, all variables >= 0
# Options: overtime (limited to a % of regular capacity), idle time and
# subcontracting (optional capacity per period) can be switched on or off.
# Whole workers: the book's conservative rounding rounds every W_t up and then
# re-solves for the other variables; an integer program can also be solved.
# =============================================================================

ap_lp_solve <- function(s, opts, fixed_W = NULL, integer = FALSE) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("The 'lpSolve' package is required. Run install.packages('lpSolve') once.")
  }
  n <- s$n
  c <- s$costs
  blocks <- c("W", "H", "F", "P", "I", "O", "U", "S")
  nv <- length(blocks) * n
  at <- function(b, t) (match(b, blocks) - 1L) * n + t

  # Objective
  obj <- numeric(nv)
  for (t in seq_len(n)) {
    obj[at("H", t)] <- c$hire
    obj[at("F", t)] <- c$fire
    obj[at("I", t)] <- c$hold
    obj[at("O", t)] <- c$ot
    obj[at("S", t)] <- c$sc
    obj[at("U", t)] <- c$idle
    if (identical(c$reg_basis, "worker_day")) {
      obj[at("W", t)] <- c$reg * s$days[t]                 # payroll for every worker-day
    } else {
      obj[at("W", t)] <- c$reg * s$kn[t]                   # regular units = K n W - U
      obj[at("U", t)] <- obj[at("U", t)] - c$reg
    }
  }

  rows <- list(); dirs <- character(0); rhs <- numeric(0)
  add <- function(coef, dir, value) {
    row <- numeric(nv)
    row[as.integer(names(coef))] <- coef
    rows[[length(rows) + 1]] <<- row
    dirs <<- c(dirs, dir)
    rhs <<- c(rhs, value)
  }
  for (t in seq_len(n)) {
    # (A) W_t - W_{t-1} - H_t + F_t = 0
    coef <- setNames(c(1, -1, 1), c(at("W", t), at("H", t), at("F", t)))
    if (t > 1) coef <- c(coef, setNames(-1, at("W", t - 1)))
    add(coef, "=", if (t == 1) s$W0 else 0)
    # (B) P_t - K n_t W_t - O_t + U_t = 0
    add(setNames(c(1, -s$kn[t], -1, 1), c(at("P", t), at("W", t), at("O", t), at("U", t))), "=", 0)
    # (C) P_t + S_t + I_{t-1} - I_t = D_t
    coef <- setNames(c(1, 1, -1), c(at("P", t), at("S", t), at("I", t)))
    if (t > 1) coef <- c(coef, setNames(1, at("I", t - 1)))
    add(coef, "=", s$demand[t] - (if (t == 1) s$I0 else 0))
    # Options
    if (isTRUE(opts$allow_ot)) {
      add(setNames(c(1, -opts$ot_pct / 100 * s$kn[t]), c(at("O", t), at("W", t))), "<=", 0)
    } else {
      add(setNames(1, at("O", t)), "=", 0)
    }
    if (!isTRUE(opts$allow_idle)) add(setNames(1, at("U", t)), "=", 0)
    if (isTRUE(opts$allow_sc)) {
      if (is.finite(opts$sc_cap[t])) add(setNames(1, at("S", t)), "<=", opts$sc_cap[t])
    } else {
      add(setNames(1, at("S", t)), "=", 0)
    }
    if (!is.null(fixed_W)) add(setNames(1, at("W", t)), "=", fixed_W[t])
  }
  add(setNames(1, at("I", n)), ">=", s$IT)

  mat <- do.call(rbind, rows)
  int_vec <- if (integer) unlist(lapply(c("W", "H", "F"), function(b) at(b, seq_len(n)))) else NULL
  fit <- lpSolve::lp("min", obj, mat, dirs, rhs, int.vec = int_vec)
  if (fit$status != 0) {
    stop("No feasible plan exists with these options. Allow overtime, idle time or subcontracting, ",
         "or raise their limits.")
  }
  v <- function(b) fit$solution[at(b, seq_len(n))]
  W <- v("W"); H <- v("H"); F <- v("F"); P <- v("P"); I <- v("I")
  O <- v("O"); U <- v("U"); S <- v("S")
  regular <- s$kn * W - U
  tab <- data.frame(
    Period = s$periods, "Working days" = s$days, Demand = s$demand,
    Workers = W, Hired = H, Fired = F,
    "Regular production" = regular, Overtime = O, "Idle (units)" = U,
    Subcontracted = S, "Total production" = P + S,
    "Cumulative production" = cumsum(P + S), "Cumulative net demand" = cumsum(s$net),
    "Ending inventory" = I, check.names = FALSE
  )
  costs <- ap_cost_items(s, W, H, F, I, regular_units = regular,
                         overtime = if (isTRUE(opts$allow_ot)) O else NULL,
                         subcontract = if (isTRUE(opts$allow_sc)) S else NULL,
                         idle = if (isTRUE(opts$allow_idle)) U else NULL)
  list(table = tab, production = P + S, costs = costs, objective = fit$objval, W = W)
}

linear_programming_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Linear Programming"),
      ap_setup_info_ui(ns),
      checkboxInput(ns("allow_ot"), "Allow overtime", FALSE),
      conditionalPanel(
        "input.allow_ot", ns = ns,
        numericInput(ns("ot_pct"), "Maximum overtime (% of regular capacity K n W)", value = 20, min = 0)
      ),
      checkboxInput(ns("allow_idle"), "Allow idle time", FALSE),
      checkboxInput(ns("allow_sc"), "Allow subcontracting", FALSE),
      conditionalPanel(
        "input.allow_sc", ns = ns,
        textInput(ns("sc_cap"), "Subcontracting limit per period (units; one value, a list, or empty = no limit)", "")
      ),
      hr(),
      checkboxInput(ns("integer"), "Also solve as an integer program (whole workers, hires, fires)", FALSE),
      helpText("Costs of overtime, idle time and subcontracting are entered in Plan Setup.")
    ),
    mainPanel(
      ap_results_ui(ns, method = tagList(
        p("Decision variables for every period: workers W, hired H, fired F, production P,",
          "inventory I, overtime O, idle time U and subcontracting S."),
        p("$$\\min \\sum_t \\left(c_H H_t + c_F F_t + c_I I_t + c_R(\\cdot) + c_O O_t + c_U U_t + c_S S_t\\right)$$"),
        p("$$W_t = W_{t-1} + H_t - F_t,\\quad P_t = K n_t W_t + O_t - U_t,\\quad I_t = I_{t-1} + P_t + S_t - D_t$$"),
        p("with \\(I_T \\ge\\) the required ending inventory and all variables \\(\\ge 0\\).",
          "Regular time costs \\(c_R (K n_t W_t - U_t)\\) per unit, or \\(c_R n_t W_t\\) as payroll."),
        p("Whole workers (book's conservative rounding): every \\(W_t\\) is rounded up and the LP",
          "is solved again for the other variables. This is always feasible but rarely optimal.")
      ))
    )
  )
}

linear_programming_server <- function(id, setup) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      s <- setup()
      sc_cap <- rep(Inf, s$n)
      if (isTRUE(input$allow_sc) && nzchar(trimws(input$sc_cap %||% ""))) {
        sc_cap <- tryCatch(ap_parse_list(input$sc_cap, s$n, "Subcontracting limit"),
                           error = function(e) validate(need(FALSE, conditionMessage(e))))
      }
      validate(need(!isTRUE(input$allow_ot) || (is.finite(input$ot_pct) && input$ot_pct >= 0),
                    "The overtime limit must be a non-negative percentage."))
      opts <- list(allow_ot = isTRUE(input$allow_ot), ot_pct = input$ot_pct,
                   allow_idle = isTRUE(input$allow_idle), allow_sc = isTRUE(input$allow_sc),
                   sc_cap = sc_cap)
      solve <- function(...) tryCatch(ap_lp_solve(s, opts, ...),
                                      error = function(e) validate(need(FALSE, conditionMessage(e))))
      original <- solve()
      rounded <- solve(fixed_W = ceiling(original$W - 1e-6))
      plans <- list("Original LP plan" = original, "Whole workers (rounded up)" = rounded)
      if (isTRUE(input$integer)) plans[["Integer program"]] <- solve(integer = TRUE)
      notes <- c(sprintf("Optimal LP cost (fractional workers): %s.", fc_fmt(original$objective)),
                 ap_rounding_note(plans))
      list(title = "Linear Programming", plans = plans, notes = notes)
    })
    ap_results_server(input, output, session, setup, res, "linear_programming")
    res
  })
}

# Parse "500" or "500, 400, 300" into a vector of length n.
ap_parse_list <- function(text, n, label) {
  pieces <- trimws(unlist(strsplit(as.character(text), "[,;[:space:]]+")))
  pieces <- pieces[nzchar(pieces)]
  values <- fc_to_numeric(pieces)
  if (length(values) == 1) values <- rep(values, n)
  if (length(values) != n || any(!is.finite(values)) || any(values < 0)) {
    stop(sprintf("%s: enter one non-negative number or exactly %d numbers.", label, n))
  }
  values
}
