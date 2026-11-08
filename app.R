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
    tabPanel("Holt's Method", holt_ui("holt")),
    "----",
    "Seasonal series",
    tabPanel("Seasonal Decomposition", seasonal_decomposition_ui("seasonal_decomposition")),
    tabPanel("Winters' Method", winters_ui("winters")),
    "----",
    "Box-Jenkins",
    tabPanel("AR, MA, ARMA and ARIMA", box_jenkins_ui("box_jenkins"))
  ),
  navbarMenu("Heuristic Plans",
    "Aggregate planning",
    tabPanel("Zero-inventory (Chase) Plan", chase_plan_ui("chase_plan")),
    tabPanel("Constant-workforce (Level) Plan", level_plan_ui("level_plan")),
    tabPanel("Mixed Plan", mixed_plan_ui("mixed_plan"))
  ),
  navbarMenu("Optimization",
    "Aggregate planning",
    tabPanel("Linear Programming", linear_programming_ui("linear_programming")),
    tabPanel("Transportation Method", transportation_method_ui("transportation_method")),
    "----",
    "Detailed planning",
    tabPanel("Disaggregation", disaggregation_ui("disaggregation"))
  )
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
  weighted_ma_server("weighted_ma")
  exp_smoothing_server("exp_smoothing")
  holt_server("holt")
  seasonal_factors_server("seasonal_factors")
  seasonal_decomposition_server("seasonal_decomposition")
  winters_server("winters")
  box_jenkins_server("box_jenkins")
  chase_plan_server("chase_plan")
  level_plan_server("level_plan")
  mixed_plan_server("mixed_plan")
  linear_programming_server("linear_programming")
  transportation_method_server("transportation_method")
  disaggregation_server("disaggregation")
}

shinyApp(ui, server)
