# =============================================================================
# Disaggregation with the square-root rule   (Aggregate Planning > Disaggregation)
# Nahmias & Olsen Section 3.6 (Bitran and Hax, 1981); lecture slides 79-81
# The aggregate production X* of a period is split among the product families
# Y_1, ..., Y_J so that the average annual setup cost is as small as possible:
#     minimize  sum_j K_j lambda_j / Y_j
#     s.t.      sum_j Y_j = X*,   a_j <= Y_j <= b_j
# K_j = setup cost of family j, lambda_j = annual demand (usage rate) of family j.
# Without bounds the solution is the square-root rule:
#     Y_j = X* sqrt(K_j lambda_j) / sum_i sqrt(K_i lambda_i)
# With bounds, Y_j = min(b_j, max(a_j, s sqrt(K_j lambda_j))) and s is found so
# that the Y_j add up to X* (Lagrange multiplier, found by bisection).
# =============================================================================

ap_parse_families <- function(text) {
  lines <- trimws(unlist(strsplit(as.character(text), "\n")))
  lines <- lines[nzchar(lines)]
  rows <- lapply(lines, function(l) trimws(unlist(strsplit(l, "[,;\t]"))))
  if (length(rows) < 2) stop("Enter at least two product families (one per line).")
  out <- do.call(rbind, lapply(rows, function(r) {
    if (length(r) < 3) stop("Each line needs: name, setup cost, annual demand [, minimum, maximum].")
    num <- fc_to_numeric(r[-1])
    data.frame(Family = r[1], K = num[1], lambda = num[2],
               a = if (length(num) >= 3 && is.finite(num[3])) num[3] else 0,
               b = if (length(num) >= 4 && is.finite(num[4])) num[4] else Inf)
  }))
  if (any(!is.finite(out$K)) || any(!is.finite(out$lambda)) || any(out$K < 0) || any(out$lambda < 0)) {
    stop("Setup costs and annual demands must be non-negative numbers.")
  }
  if (sum(out$K * out$lambda) <= 0) stop("At least one family needs a positive setup cost and demand.")
  if (any(out$a > out$b)) stop("A minimum is larger than its maximum.")
  if (anyDuplicated(out$Family)) stop("Family names must be different.")
  out
}

# Square-root rule with bounds for one aggregate quantity X.
ap_sqrt_rule <- function(X, fam) {
  root <- sqrt(fam$K * fam$lambda)
  if (X < sum(fam$a) - 1e-9 || X > sum(fam$b) + 1e-9) return(rep(NA_real_, nrow(fam)))
  y <- function(s) pmin(fam$b, pmax(fam$a, s * root))
  lo <- 0
  hi <- 1
  while (sum(y(hi)) < X && hi < 1e15) hi <- hi * 2
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (sum(y(mid)) < X) lo <- mid else hi <- mid
  }
  y(hi)
}

disaggregation_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Disaggregation (square-root rule)"),
      ap_setup_info_ui(ns),
      selectInput(ns("source"), "Aggregate plan to split",
                  choices = c("Chase plan", "Level plan", "Mixed plan", "Linear programming",
                              "Enter quantities manually")),
      conditionalPanel(
        "input.source != 'Enter quantities manually'", ns = ns,
        radioButtons(ns("version"), NULL,
                     choices = c("Whole-worker plan" = "2", "Original plan" = "1"), inline = TRUE)
      ),
      conditionalPanel(
        "input.source == 'Enter quantities manually'", ns = ns,
        textInput(ns("manual"), "Aggregate production per period (one value or a list)", "1000")
      ),
      textAreaInput(ns("families"),
                    "Product families: name, setup cost K_j, annual demand λ_j [, minimum, maximum]",
                    value = "Family A, 200, 2000\nFamily B, 150, 4500\nFamily C, 300, 900", rows = 5),
      helpText("One family per line. Minimum and maximum (units per period) are optional.")
    ),
    mainPanel(
      h3(textOutput(ns("headline"))),
      uiOutput(ns("notes")),
      h4("Product families"),
      tableOutput(ns("family_table")),
      plotOutput(ns("plot"), height = "380px"),
      h4("Disaggregated plan by period"),
      div(style = "overflow-x: auto;", tableOutput(ns("period_table"))),
      downloadButton(ns("download"), "Download the disaggregated plan (CSV)"),
      br(), br(),
      wellPanel(h4("Method"), withMathJax(
        p("The aggregate production \\(X^*\\) of each period is divided among the families so",
          "that the average annual setup cost is minimized (Bitran and Hax):"),
        p("$$\\min \\sum_j \\frac{K_j \\lambda_j}{Y_j} \\quad \\text{s.t.} \\quad \\sum_j Y_j = X^*,\\ a_j \\le Y_j \\le b_j$$"),
        p("Without bounds the optimal run sizes follow the square-root rule:"),
        p("$$Y_j = X^* \\frac{\\sqrt{K_j \\lambda_j}}{\\sum_i \\sqrt{K_i \\lambda_i}}$$"),
        p("With bounds, a family at its bound is fixed there and the rest is shared by the",
          "same rule among the other families.")
      ))
    )
  )
}

disaggregation_server <- function(id, setup, plans) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      s <- setup()
      fam <- tryCatch(ap_parse_families(input$families),
                      error = function(e) validate(need(FALSE, conditionMessage(e))))
      if (identical(input$source, "Enter quantities manually")) {
        X <- tryCatch(ap_parse_list(input$manual, s$n, "Aggregate production"),
                      error = function(e) validate(need(FALSE, conditionMessage(e))))
        label <- "Manual quantities"
      } else {
        p <- plans[[input$source]]()
        k <- min(as.integer(input$version %||% "2"), length(p$plans))
        X <- p$plans[[k]]$production
        label <- sprintf("%s – %s", input$source, names(p$plans)[k])
      }
      Y <- t(vapply(X, ap_sqrt_rule, numeric(nrow(fam)), fam = fam))
      if (nrow(fam) == 1) Y <- t(Y)
      colnames(Y) <- fam$Family
      setup_cost <- apply(Y, 1, function(y) if (any(is.na(y))) NA else sum(ifelse(y > 0, fam$K * fam$lambda / y, Inf)))
      list(s = s, fam = fam, X = X, Y = Y, label = label, setup_cost = setup_cost)
    })

    output$headline <- renderText({
      r <- res()
      sprintf("Square-root disaggregation of: %s", r$label)
    })

    output$notes <- renderUI({
      r <- res()
      notes <- character(0)
      if (any(is.na(r$Y))) notes <- c(notes, "In some periods the aggregate quantity is outside the sum of the family minimums/maximums; those periods are left empty.")
      if (any(is.finite(r$fam$b)) || any(r$fam$a > 0)) notes <- c(notes, "Family bounds are used, so the shares can differ between periods.")
      if (length(notes) == 0) notes <- "No bounds are binding: every period uses the same square-root shares."
      div(class = "text-muted", lapply(notes, p))
    })

    output$family_table <- renderTable({
      f <- res()$fam
      root <- sqrt(f$K * f$lambda)
      data.frame(Family = f$Family, "Setup cost K_j" = f$K, "Annual demand λ_j" = f$lambda,
                 "sqrt(K_j λ_j)" = root, "Share (%)" = 100 * root / sum(root),
                 Minimum = f$a, Maximum = ifelse(is.finite(f$b), f$b, NA), check.names = FALSE)
    }, digits = 2, na = "")

    period_table <- reactive({
      r <- res()
      data.frame(Period = r$s$periods, "Aggregate production X*" = r$X, r$Y,
                 "Check: sum of families" = rowSums(r$Y),
                 "Setup cost sum Kλ/Y" = r$setup_cost, check.names = FALSE)
    })
    output$period_table <- renderTable(period_table(), digits = 2, na = "")

    output$plot <- renderPlot({
      r <- res()
      Y <- r$Y
      Y[is.na(Y)] <- 0
      cols <- grDevices::hcl.colors(ncol(Y), "Set 2")
      par(mar = c(6, 5, 3, 1))
      barplot(t(Y), names.arg = r$s$periods, col = cols, border = NA, las = 2,
              ylab = "Units", main = "Aggregate production split among product families")
      legend("topleft", bty = "n", fill = cols, legend = colnames(Y))
    })

    output$download <- downloadHandler(
      filename = function() "disaggregation_square_root.csv",
      content = function(file) utils::write.csv(period_table(), file, row.names = FALSE)
    )
  })
}
