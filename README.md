# Time Serie Econometrics Final Project
## ARMA, GARCH and TGARCH Models to Predict Vanguard Real State ETF Price.
Tomás Bordaberry & Nicolas Odizzio - Universidad de Montevideo 2023

### Abstract
We utilized ARMA, GARCH, and TGARCH models to forecast the adjusted price of Vanguard Real Estate ETF (VNQ). Firstly, we differenced the series and estimated differentiated ARMA models. We incorporated dummy variables to control for the COVID-19 shock in the ARMA models. We compared these models against a simple ARMA. We concluded that the best ARMA model is the one with a smaller dummy centered on the shock, and the best model overall is ARIMA(1,1,4). We performed a train-test split, attempting to predict the last 5% of the data and comparing it with the actual values. Furthermore, we predicted 30 weeks into the future using the bootstrap methodology to simulate 100 different paths with various confidence intervals.

Secondly, using the Ljung-Box methodology, we detected the presence of autocorrelation among the squared residuals, leading us to employ GARCH and TARCH models. We manually estimated GARCH models by imposing our previously found ARMA models. Then, we estimated GARCH models by allowing the model to determine the coefficient values. Additionally, we incorporated TGARCH models that respond asymmetrically to negative shocks. Finally, we decomposed the series and estimated TGARCH models on the random component only.
