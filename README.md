# Production Planning Group 1

R Shiny application for production-planning calculations.

The Forecasting menu currently includes moving averages, weighted moving
averages, simple exponential smoothing, Holt's method, seasonal factors,
classical seasonal decomposition with moving averages, Winters' method, and a
Box-Jenkins module for AR, MA, ARMA and ARIMA models. The Box-Jenkins module
supports manual or AIC/BIC-based order selection, forecasts with 95% intervals,
ACF/PACF plots, coefficient tables and residual diagnostics.

The Aggregate Planning menu includes the heuristic plans (zero-inventory
(chase), constant-workforce (level) and mixed plans), the optimization models
(linear programming and the transportation method) and square-root
disaggregation. Each heuristic plan compares fractional workforce with a
workforce-rounded-up alternative and reports production, inventory, backlog,
workforce changes, and costs by period.

Run the application from the project directory:

```r
shiny::runApp()
```
