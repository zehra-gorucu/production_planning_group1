# Production Planning Group 1

R Shiny application for production-planning calculations.

The Forecasting menu currently includes moving averages, weighted moving
averages, simple exponential smoothing, Holt's method, seasonal factors,
classical seasonal decomposition with moving averages, Winters' method, and a
Box-Jenkins module for AR, MA, ARMA and ARIMA models. The Box-Jenkins module
supports manual or AIC/BIC-based order selection, forecasts with 95% intervals,
ACF/PACF plots, coefficient tables and residual diagnostics.

The Aggregate Planning menu starts with a Plan Setup tab where the user uploads
period names, demand forecasts and working days (Excel or CSV), edits the period
names, enters K directly or computes it from past data, and enters the plant
data and costs. The same setup is used by the heuristic plans (zero-inventory
(chase), constant-workforce (level) and a mixed plan with user-chosen workforce
change periods), linear programming (optional overtime, idle time and
subcontracting), the transportation method (north-west corner and cheapest cell
first) and square-root disaggregation. Each plan shows workers, hiring, firing,
production and inventory by period, a cumulative production vs. cumulative net
demand chart, the cost of each item, and a whole-worker (rounded-up) version next
to the original plan. Results are checked against Nahmias & Olsen Example 3.1
(Densepack).

Run the application from the project directory:

```r
shiny::runApp()
```
