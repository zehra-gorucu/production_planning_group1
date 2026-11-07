library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  navbarMenu("Forecasting",
    "Stationary series",
    tabPanel("Moving Average", moving_average_ui("moving_average")),
    tabPanel("Weighted Moving Average", weighted_ma_ui("weighted_ma")),
    tabPanel("Simple Exponential Smoothing", exp_smoothing_ui("exp_smoothing")),
    tabPanel("Seasonal Factors (quick and dirty)", seasonal_factors_ui("seasonal_factors")),
    "----",
    "Trend series",
    tabPanel("Holt's Method", holt_ui("holt"))
  )
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
  weighted_ma_server("weighted_ma")
  exp_smoothing_server("exp_smoothing")
  seasonal_factors_server("seasonal_factors")
  holt_server("holt")
}

shinyApp(ui, server)
