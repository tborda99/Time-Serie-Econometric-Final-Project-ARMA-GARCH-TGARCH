##PROYECTO FINAL ECONOMETRIA II
## Bordaberry & Odizzio

# Instalamos paquete quantmod 
install.packages("quantmod")
install.packages("tseries") 
install.packages("rugarch")
library(quantmod) #Datos
library(forecast) #Time series
library(ggplot2) #Graficas
library(tseries) #libreria necesaria para el test ACF PACF
library(TSstudio) #Visualizar Series
library(kableExtra) #Tablas
library(stats) #No me acuerdo para que lo importamos.
library(gridExtra) #Para unir tablas
library(stargazer) #Tablas
library(rugarch) #GARCH Y TARCH


# Descargamos datos de Vanguarda Real State ETF (VNQ) de Yahoo Finance
getSymbols("VNQ", src = "yahoo")

#Asegurarnos que cuando el codigo se corra más adelante, no se tomen días mayores
#al último que tomamos para el trabajo.
end_date <- as.Date("2023-06-16")
VNQ <- VNQ[time(VNQ) <= end_date]

VNQ <- to.weekly(VNQ)
vnq_adjusted <- ts(VNQ$VNQ.Adjusted,frequency = 52)



#MODELIZACIÓN ARMA
#1 ESTACIONERIDAD DE LA SERIE

table <- summary(VNQ)
table

# Creamos una gráfica del Precio de VNQ en el tiempo
x_var <- index(VNQ)
plot(x_var,VNQ$VNQ.Adjusted, type ="l",main = "Serie de Tiempo del Precio de VNQ", xlab = "Tiempo", ylab = "Precio al cierre ajustado", col = 'blue')


##### DESCOMPOSICION DE LOS DATOS ######
decomposed <- decompose(vnq_adjusted)

# Access the individual components
trend <- decomposed$trend
seasonal <- decomposed$seasonal
random <- decomposed$random

# Plot the components
plot(decomposed, col = 'blue')

# Plot the individual components separately
plot(trend, main = "Trend Component")
plot(seasonal, main = "Seasonal Component")
plot(random, main = "Random Component")


# Tests de Estacionaridad
#Para poder confirmar si la serie es estacionaria

# Perform stationary tests
adf_result <- adf.test(vnq_adjusted)
pp_result <- pp.test(vnq_adjusted)
kpss_result <- kpss.test(vnq_adjusted)

# Create a data frame with the test results
results_df <- data.frame(Test = c("ADF", "PP", "KPSS"),
                         Statistic = c(adf_result$statistic, pp_result$statistic, kpss_result$statistic),
                         P_value = c(adf_result$p.value, pp_result$p.value, kpss_result$p.value))

# Format the data frame as a table
table <- results_df %>%
  kable(format = "html", align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Print the table
print(table)


#Vamos a convertirla en estacionaria

# Obtenemos el número óptimo de veces a diferenciar la serie para estacionarizarla
ndiffs(x = vnq_adjusted)
#Nos devuelve 1

serie_dif <- diff(vnq_adjusted,1)

acf <- acf(serie_dif,main ="ACF")
acf$lag <- acf$lag*50
plot(acf,main ="ACF")

pacf <- pacf(serie_dif, main = "PACF")
pacf$lag <-  pacf$lag*50
plot(pacf,  main = "PACF")



# Tests de Estacionaridad 2.0
#Para poder confirmar si la serie ahora es estacionaria
# Perform stationary tests
adf_result <- adf.test(serie_dif)
pp_result <- pp.test(serie_dif)
kpss_result <- kpss.test(serie_dif)

# Create a data frame with the test results
results_df <- data.frame(Test = c("ADF", "PP", "KPSS"),
                         Statistic = c(adf_result$statistic, pp_result$statistic, kpss_result$statistic),
                         P_value = c(adf_result$p.value, pp_result$p.value, kpss_result$p.value))

# Format the data frame as a table
table <- results_df %>%
  kable(format = "html", align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Print the table
print(table)


x_var_dif <- x_var[-1] #El indice pero sin la primer fecha.
plot(x_var_dif,serie_dif,main = "VNQ Diferenciado", xlab = "Tiempo", ylab = "Precio al cierre ajustado diferenciado", col = 'blue', type ="l")

#ARIMA SIN DUMMY
modelo_sin <- auto.arima(serie_dif)
modelo_sin



##### DUMMY COVID ######

#Vemos que hay datos atípicos correspondientes a la crisis COVID19 en 2020
## Agregamos una Dummy para controlar por estos datos atípicos.

#### DUMMY 1 #######
dummy_start_date <- as.Date('2020-02-21') 
dummy_end_date <- as.Date('2020-09-16')

dummy <- rep(0, length(VNQ$VNQ.Adjusted))
constante <- rep(0, length(VNQ$VNQ.Adjusted))

dummy[index(VNQ) >= dummy_start_date & index(VNQ) <= dummy_end_date] <- 1
constante[index(VNQ) > as.Date('2007-01-05')] <-  1

dummy_df <- as.data.frame(dummy)
constante_df <- as.data.frame(constante)
VNQ_df <- as.data.frame(VNQ$VNQ.Adjusted)

# Combine the dummy dataframe with the time series data
data <- cbind(VNQ_df, dummy_df, constante_df)


# Fit the ARIMA model con dummy 1
model <- auto.arima(data[, 1], xreg = cbind(data[,2],data[, 3]))
model

dum_1 <- arima(data[, 1], order = c(0,1,1), xreg = cbind(data[,2],data[, 3]))
dum_1

#(Solo para plotear)
modelo_dummy <- ts(cbind(VNQ_df,dummy_df))
ts_plot(modelo_dummy,title = "Modelo con Dummy 1", type = "multiple")


#Con dummy super puesta
dummy_150_1 <- dummy*150
dummy_df_150_1 <- as.data.frame(dummy_150_1)
data_150_1 <- cbind(VNQ_df, dummy_df_150_1)
modelo_dummy_150_1 <- ts(data_150_1)
ts_plot(modelo_dummy_150_1,title = "Modelo con Dummy 1")


### DUMMY 2 ####
dummy_start_date_2 <- as.Date('2020-03-06') 
dummy_end_date_2 <- as.Date('2020-05-29')

dummy_2 <- rep(0, length(VNQ$VNQ.Adjusted))

dummy_2[index(VNQ) >= dummy_start_date_2 & index(VNQ) <= dummy_end_date_2] <- 1

dummy_df_2 <- as.data.frame(dummy_2)

# Combine the dummy dataframe with the time series data
data_2 <- cbind(VNQ_df, dummy_df_2, constante_df)

# Fit the ARIMA model con dummy 2
model_2 <- auto.arima(data_2[, 1], xreg = cbind(data_2[,2],data_2[, 3]))
model_2

dum_2 <- arima(data_2[,1], order = c(0,1,1), xreg = cbind(data_2[,2],data_2[, 3]))
dum_2

#(Solo para plotear)
modelo_dummy_2 <- ts(cbind(VNQ_df, dummy_df_2))
ts_plot(modelo_dummy_2,title = "Modelo con Dummy 2", type = "multiple")

#Con dummy super puesta
dummy_150 <- dummy_2*150
dummy_df_150 <- as.data.frame(dummy_150)
data_150 <- cbind(VNQ_df, dummy_df_150)
modelo_dummy_150 <- ts(data_150)
ts_plot(modelo_dummy_150,title = "Modelo con Dummy 2")


### DUMMY 3#########
dummy_start_date_3 <- as.Date('2020-03-06') 
dummy_end_date_3 <- as.Date('2020-04-09')

dummy_3 <- rep(0, length(VNQ$VNQ.Adjusted))

dummy_3[index(VNQ) >= dummy_start_date_3 & index(VNQ) <= dummy_end_date_3] <- 1

dummy_df_3 <- as.data.frame(dummy_3)

# Combine the dummy dataframe with the time series data
data_3 <- cbind(VNQ_df, dummy_df_3, constante_df)

# Fit the ARIMA model con dummy 3
model_3 <- auto.arima(data_3[, 1], xreg = cbind(data_3[,2],data_3[, 3]))
model_3


dum_3 <- arima(data_3[,1], order = c(1,1,4), xreg = cbind(data_3[,2],data_3[, 3]))
dum_3

#(Solo para plotear)
modelo_dummy_3 <- ts(cbind(VNQ_df, dummy_df_3))
ts_plot(modelo_dummy_3,title = "Modelo con Dummy 3", type = "multiple")

#Con dummy super puesta
dummy_150_3 <- dummy_3*150
dummy_df_150_3 <- as.data.frame(dummy_150_3)
data_150_3 <- cbind(VNQ_df, dummy_df_150_3)
modelo_dummy_150_3 <- ts(data_150_3)
ts_plot(modelo_dummy_150_3,title = "Modelo con Dummy 3")


##### TABLAS Y COMPARO MODELOS ######
#Sin dummy

sin_dum <- arima(vnq_adjusted, order = c(1,1,0), xreg = constante)

#DIFERENCIADO SIN DUMMY
stargazer(sin_dum,type ='html', out = 'SerieDifSinDum.html',
          align = TRUE, column.labels=c("Simple Diferenciado"),
          title =  "Table 1: ARMA SIMPLE",covariate.labels = c("AR1","Constante"),
          dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),no.space=TRUE, intercept.bottom = TRUE,
          add.lines = list(c("BIC", round(BIC(sin_dum), 2))))


#DUMMY 1
stargazer(dum_1,type ='html', out = 'Dummy1.html',
          align = TRUE, column.labels=c("Simple Diferenciado"),
          title =  "Table 2: ARMA DUMMY COVID 1",covariate.labels = c("MA1","D1","Constante"),
          dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),no.space=TRUE, intercept.bottom = TRUE,
          add.lines = list(c("BIC", round(BIC(dum_1), 2))))
#DUMMY 2
stargazer(dum_2,type ='html', out = 'Dummy2.html',
          align = TRUE, column.labels=c("Simple Diferenciado"),
          title =  "Table 3: ARMA DUMMY COVID 2",covariate.labels = c("MA1","D2","Constante"),
          dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),no.space=TRUE, intercept.bottom = TRUE,
          add.lines = list(c("BIC", round(BIC(dum_2), 2))))

#DUMMY 3
stargazer(dum_3,type ='html', out = 'Dummy3.html',
          align = TRUE, column.labels=c("Simple Diferenciado"),
          title =  "Table 4: ARMA DUMMY COVID 3",covariate.labels = c("AR1","MA1","MA2","MA3","MA4","D3","Constante"),
          dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),no.space=TRUE, intercept.bottom = TRUE,
          add.lines = list(c("BIC", round(BIC(dum_3), 2))))

#COMPARACION
stargazer(sin_dum, dum_1,dum_2,dum_3, type ='html', out = 'resultados_arma.html',
          align = TRUE, column.labels=c("Simple","COVID 1","COVID 2","COVID 3"),
          covariate.labels=c("AR1","Constante","MA1","D1","Constante","D2","Constante","MA2","MA3","MA4","D3","Constante"),
          title =  "Table 5: ARMA con Dummy",dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),
          add.lines = list(c("BIC", round(BIC(sin_dum), 2),round(BIC(dum_1), 2),round(BIC(dum_2), 2),round(BIC(dum_3), 2))))




#######Serie Diferenciado vs No Diferenciado
dum2_sin_dif <- arima(data_3[,1], order = c(4,0,2),xreg = cbind(data_3[,2]))
AIC(dum2_sin_dif)
BIC(dum2_sin_dif)
stargazer(dum2_sin_dif,dum_3, type = 'html',out = 'DifvsNoDif.html', align =TRUE,dep.var.labels=c("","Precio al Cierre Ajustado VNQ","",""),
          covariate.labels=c("AR1","AR2","AR3","AR4","MA1","MA2","Constante","D1","MA3","MA4","D2","Constante"),
          column.labels = c("Sin Diferenciar","Diferenciado"),title = "Table 6: Diferenciado vs No Diferenciado",
          add.lines = list(c("BIC", round(BIC(dum2_sin_dif), 2),round(BIC(dum_3), 2))))

#### MEJORES ARMA ################
#Nuestro Mejor modelo con Dummy es:
dum_3_A <- Arima(data_3[,1], order = c(1,1,4), xreg = cbind(data_3[,2],data_3[, 3]))
#Nuestro mejor modelo sin Dummy es:
modelo_sin <-  modelo_sin
#################################

###### FORECAST ####

n <- length(data_3[, 1])
test_data <- data_3[((n * 0.95) + 1):n, ]
predicted_values <- forecast(dum_3_A, xreg = cbind(test_data[, 2], test_data[, 3]))$mean

result <- data.frame(Actual = test_data[, 1], Predicted = predicted_values)
ggplot(result, aes(x = 1:length(Actual))) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  xlab("Observation") +
  ylab("Value") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

data_3_last <- tail(data_3, 30)
predicted_futuro <- forecast(dum_3_A, xreg = cbind(data_3_last[, 2], data_3_last[, 3]), level =0.95, bootstrap = TRUE, npaths = 100 )
predicted_futuro_2 <- forecast(dum_3_A, xreg = cbind(data_3_last[, 2], data_3_last[, 3]), level =0.70, bootstrap = TRUE, npaths = 100 )
predicted_futuro_3 <- forecast(dum_3_A, xreg = cbind(data_3_last[, 2], data_3_last[, 3]), level =0.30, bootstrap = TRUE, npaths = 100 )
predicted_futuro_4 <- forecast(dum_3_A, xreg = cbind(data_3_last[, 2], data_3_last[, 3]), level =0.10, bootstrap = TRUE, npaths = 100 )


g1 <- autoplot(predicted_futuro) + autolayer(predicted_futuro, series = "95%")+
  ggtitle("Predicción 30 Semanas") +
  ylab("Precio Ajustado") +autolayer(predicted_futuro_2, series = "70%") +
  autolayer(predicted_futuro_3, series = "30%")+ autolayer(predicted_futuro_4, series ="10%")
g2 <- autoplot(predicted_futuro) + autolayer(predicted_futuro, series = "95%") +
  ggtitle("Predicción 30 Semanas") +
  ylab("Precio Ajustado") +autolayer(predicted_futuro_2, series = "70%") +
  autolayer(predicted_futuro_3, series = "30%") + autolayer(predicted_futuro_4, series = "10%")+
  coord_cartesian(xlim = c(800, 875)) 

grid.arrange(g1, g2, ncol=2)


####### TESTS ########

checkresiduals(dum_3_A)


residuos_3_A <- residuals(dum_3_A)
residuos_cuadrados_3_A <- residuos_3_A^2

Box.test(residuos_cuadrados_3_A, lag = 20, type = "Ljung-Box") 



checkresiduals(modelo_sin)

#Queremos saber si los errores son ruidos blancos

# Test Q (Ljung-Box test) --> testamos si hay autocorrelacion entre los residuos

# Ho: los errores son ruidos blanco --> no presentan autocorreación
# Ha: los errores no son ruido blanco --> presentan autocorrelación

modelo_sin <- auto.arima(serie_dif)
modelo_sin

residuos <- residuals(modelo_sin)
residuos_cuadrados <- residuos^2

# Ljung_box_test: testeamos autocorrelación entre los residuos al cuadrado
Box.test(residuos_cuadrados, lag = 20, type = "Ljung-Box") 

# Rechazamos la Hipotesis nula de que no existe autocorrelación
# en los residuos al cuadrado, lo que nos indica que existen efectos ARCH

###############################
### ARCH Y GARCH Y TARCH#######
##############################

##### GARCH #######
tsdisplay(residuos_cuadrados, lag = 30)
# Gráficos de las funciones de autocorrelación para los residuos cuadrados

acf_residuos <- acf(residuos_cuadrados,main ="ACF")
acf_residuos$lag <- acf_residuos$lag*50
plot(acf_residuos, main = "ACF: Residuos Cuadrdos")

# Podemos determinar el r en un modelo ARCH(r) viendo la correlación parcial de los residuos al cuadrado
pacf_residuos <-  pacf(residuos_cuadrados)
pacf_residuos$lag <- pacf_residuos$lag*50
plot(pacf_residuos, main="PACF:Residuos Cuadrados")
#4

# A partir de la PACF podemos setear el r del modelo --> podemos ver hasta donde los errores cuadrados tienen incidencia en el valor de la varabilidad de la serie en t.
# GARCH (4,4)

#MANUAL
spec_man <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4, 4)), 
                   mean.model = list(armaOrder = c(1, 0)))

# Fit the GARCH model with the specified parameters
modelo_garch_man <- ugarchfit(spec = spec_man, data = serie_dif)

# Show the model results
modelo_garch_man

# Generate GARCH forecasts
pred_garch_man <- ugarchforecast(modelo_garch_man, n.ahead = 12)

# Plot the forecasted volatility
plot(pred_garch_man, which = 1)



## AUTOMATICO
# Definir la especificación del modelo GARCH
spec_auto <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(1, 0)))

# Ajustar el modelo GARCH utilizando ugarchfit()
modelo_auto_garch <- ugarchfit(spec = spec_auto, data = serie_dif)

# Mostrar los resultados del modelo
modelo_auto_garch

#El modelo sGARCH es un caso particular del modelo GARCH en el que se asume que
# la respuesta de la varianza condicional a los choques positivos y negativos es la misma.

pred_garch_auto <- ugarchforecast(modelo_auto_garch, n.ahead = 12)
plot(pred_garch_auto, which = 1)


############ TARCH #########################
spec <-  ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH"),
                    mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
                    distribution.model = "std")

fgarch.t <-  ugarchfit(data = vnq_adjusted, spec = spec, solver = "solnp", out.sample = 5)
fgarch.t
coeficiente_nuevo <-  coef(fgarch.t)
coeficiente_nuevo

pred_tarch_auto <- ugarchforecast(fgarch.t, n.ahead = 12, n.roll = 5)
plot(pred_tarch_auto, which =2, main = "Prediciones TARCH")

#Shape : controlar la forma de los resiudos estandarizados. CONTROLA ASIMETRIA. Que tan probable es que tenga una varaicón negativa
#Eta11: Asociado a la dummy del TARCH. El impacto en la volatilidad de la serie, cuando hay una variación del ETF.


###### DESCOMPOSICION DE LA SERIE #### 
#Tenemos los distintos componentes.
trend <- decomposed$trend
seasonal <- decomposed$seasonal
random <- decomposed$random

#Vamos a utilizar nuestro modelos en solo la parte de random, luego sumar

if (any(is.na(random))) {
  # Handle missing values (e.g., removing or imputing)
  random <- na.omit(random)  # Remove rows with missing values
  # Or
  random[is.na(random)] <- 0  # Replace missing values with zeros (or use an appropriate imputation method)
}

arima_random <-  auto.arima(random, seasonal = FALSE)

arima_random$coef
#NOS DEVUELVE ARMA(4,0)
spec_random <-  ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH"),
                    mean.model = list(armaOrder = c(4,0), include.mean = TRUE), 
                    distribution.model = "std")
random_tarch <-ugarchfit(data = random, spec = spec_random, solver = "solnp", out.sample = 10)
random_tarch

forecast_t_garch_random <-  ugarchforecast(random_tarch, n.roll = 10)
plot(forecast_t_garch_random, which = 2, main ="Forecast TARCH sobre Componente Random ")
lines(random, col = "red")


fitted_random_tarch <- fitted(random_tarch)
fitted_random_df <- as.data.frame(fitted_random_tarch)

plot(random, main = "Fitted TARCH sobre Componente Random", col = "blue")
lines(fitted_random_df$V1, col="red")






