# =============================================================================
# Disaggregation of an aggregate plan among product families.
# Square-root rule: family share_i = sqrt(weight_i) / sum_j sqrt(weight_j).
# =============================================================================

dg_split_text <- function(x) {
  parts <- unlist(strsplit(as.character(x), "[,;\\n]+"))
  trimws(parts[nzchar(trimws(parts))])
}

dg_parse_families <- function(names_text, weights_text) {
  family <- dg_split_text(names_text)
  weight_text <- dg_split_text(weights_text)
  weight <- suppressWarnings(as.numeric(weight_text))
  if (length(family) < 2) stop("Enter at least two product-family names.")
  if (length(weight) != length(family)) {
    stop("Product-family names and family weights must contain the same number of entries.")
  }
  if (any(!is.finite(weight)) || any(weight < 0) || sum(weight) <= 0) {
    stop("Family weights must be non-negative numbers with a positive total.")
  }
  if (anyDuplicated(family)) stop("Product-family names must be unique.")
  root <- sqrt(weight)
  share <- root / sum(root)
  data.frame(Family = family, Weight = weight, "Square-root score" = root,
             Share = share, check.names = FALSE)
}

dg_largest_remainder <- function(total, shares) {
  target <- as.integer(round(total))
  raw <- target * shares
  allocation <- floor(raw)
  remaining <- target - sum(allocation)
  if (remaining > 0) {
    order_fraction <- order(raw - allocation, decreasing = TRUE)
    allocation[order_fraction[seq_len(remaining)]] <-
      allocation[order_fraction[seq_len(remaining)]] + 1L
  }
  allocation
}

dg_make_result <- function(aggregate_plan, family_table, whole_units = TRUE) {
  aggregate_plan <- as.numeric(aggregate_plan)
  if (any(!is.finite(aggregate_plan)) || any(aggregate_plan < 0)) {
    stop("Aggregate-plan values must be finite and non-negative.")
  }
  shares <- family_table$Share
  if (whole_units) {
    allocation <- t(vapply(aggregate_plan, dg_largest_remainder,
                           FUN.VALUE = numeric(length(shares)), shares = shares))
    plan_used <- round(aggregate_plan)
  } else {
    allocation <- outer(aggregate_plan, shares)
    plan_used <- aggregate_plan
  }
  colnames(allocation) <- family_table$Family
  period_table <- data.frame(
    Period = seq_along(aggregate_plan),
    "Aggregate plan" = aggregate_plan,
    as.data.frame(allocation, check.names = FALSE),
    "Allocated total" = rowSums(allocation),
    Difference = rowSums(allocation) - aggregate_plan,
    check.names = FALSE
  )
  family_table[["Total allocated"]] <- colSums(allocation)
  list(period_table = period_table, family_table = family_table,
       allocation = allocation, plan_used = plan_used, whole_units = whole_units)
}

disaggregation_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Disaggregation with the Square-root Rule"),
      fc_data_ui(ns, default_example = "Aggregate planning, 6 periods (illustrative)"), hr(),
      h4("Product families"),
      textAreaInput(ns("family_names"), "Family names (comma-separated)",
                    value = "Family A, Family B, Family C", rows = 2),
      textAreaInput(ns("family_weights"), "Family demand / importance weights (same order)",
                    value = "100, 64, 36", rows = 2),
      radioButtons(ns("unit_mode"), "Allocation units",
        choices = c("Whole units; preserve each rounded period total" = "whole",
                    "Fractional units; preserve the exact period total" = "fractional"),
        selected = "whole")
    ),
    mainPanel(
      h3(textOutput(ns("headline"))), uiOutput(ns("notes")),
      h4("Family shares"), tableOutput(ns("family_table")),
      plotOutput(ns("plot"), height = "430px"),
      h4("Disaggregated plan by period"),
      div(style = "overflow-x:auto;", tableOutput(ns("period_table"))),
      downloadButton(ns("download"), "Download disaggregated plan (CSV)"), br(), br(),
      wellPanel(h4("Square-root rule"), withMathJax(
        p("For family \\(i\\), the allocation share is"),
        p("\\[s_i = \\frac{\\sqrt{w_i}}{\\sum_j \\sqrt{w_j}}\\]"),
        p("where \\(w_i\\) is the entered family demand or importance weight. Every period's aggregate quantity is multiplied by these shares."),
        p("In whole-unit mode, the largest-remainder procedure makes the family quantities add up to the rounded aggregate total."))
    )
  )
  )
}

disaggregation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    series <- fc_data_server(input, output, session)
    result <- reactive({
      s <- series()
      families <- tryCatch(dg_parse_families(input$family_names, input$family_weights),
                           error = function(e) validate(need(FALSE, conditionMessage(e))))
      tryCatch(dg_make_result(s$values, families, identical(input$unit_mode, "whole")),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$headline <- renderText({
      r <- result()
      sprintf("Square-root disaggregation — %d periods, %d product families",
              nrow(r$period_table), nrow(r$family_table))
    })
    output$notes <- renderUI({
      r <- result(); s <- series(); notes <- character(0)
      if (r$whole_units && any(abs(r$period_table$Difference) > 1e-8)) {
        notes <- c(notes, "Whole-unit mode rounds each aggregate period total to the nearest integer; the Difference column shows that rounding only.")
      }
      if (s$dropped > 0) notes <- c(notes, sprintf("%d empty / non-numeric cells were ignored.", s$dropped))
      if (length(notes) == 0) notes <- "Each period total is preserved exactly."
      div(class = "text-muted", lapply(notes, p))
    })
    output$family_table <- renderTable({
      r <- result(); tab <- r$family_table
      tab$Share <- 100 * tab$Share
      names(tab)[names(tab) == "Share"] <- "Share (%)"
      tab
    }, digits = 2)
    output$period_table <- renderTable(result()$period_table, digits = 2)
    output$plot <- renderPlot({
      r <- result()
      colors <- grDevices::hcl.colors(ncol(r$allocation), "Set 2")
      barplot(t(r$allocation), beside = FALSE, col = colors, border = NA,
              names.arg = r$period_table$Period, xlab = "Period", ylab = "Units",
              main = "Aggregate plan split among product families")
      legend("topright", bty = "n", fill = colors, legend = colnames(r$allocation), ncol = 2)
    })
    output$download <- downloadHandler(
      filename = function() "disaggregated_square_root_plan.csv",
      content = function(file) utils::write.csv(result()$period_table, file, row.names = FALSE)
    )
  })
}
