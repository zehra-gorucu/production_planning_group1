learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("L"), "Learning rate", 0.6, 0.99, 0.8),
    plotOutput(ns("curve_plot"))
  )
}

learning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$curve_plot <- renderPlot({
      b <- -log(input$L) / log(2)
      u <- 1:50
      plot(u, 100 * u^(-b), type = "l", xlab = "Unit number", ylab = "Time per unit",
           main = sprintf("Learning curve (learning rate = %.2f)", input$L))
    })
  })
}
