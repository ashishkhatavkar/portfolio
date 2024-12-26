library(readxl)
library(forecast)
library(seasonal)
library(seasonalview)
library(ggplot2)
library(lmtest)

#code to load the dataset
trend <- read_xlsx("C:/Users/Ashish Khatavkar/Downloads/Individualized datasets for Section 1 and 2/Individualized datasets for Section 1 and 2/Ashish.xlsx")

#code to convert the series into time series
tsdata = ts(trend, start = c(2011,1), frequency = 12)
plot(tsdata, main = "Movin Time Series Data")

time <- time(tsdata)
#code to fit Linear Model
fit_linear <- lm(tsdata ~ time)

#code to fit Polynomial Model
fit_poly <- lm(tsdata ~ poly(time, 2, raw = TRUE))

#code to check non positive values
if(any(trend <= 0)) {
  stop("Data contains non-positive values, which are unsuitable for logarithmic transformation.")
}

#code to fit Exponential Model
fit_exp <- lm(log(tsdata) ~ time)

#code to prepare data for ggplot to plot above 3 models
data_frame <- data.frame(
  Time = time,
  Data = tsdata,
  Linear_Pred = predict(fit_linear),
  Poly_Pred = predict(fit_poly),
  Exp_Pred = exp(predict(fit_exp))
)

#code to plot the graph
ggplot(data_frame, aes(x = Time)) +
  geom_line(aes(y = tsdata)) +  # Changed 'size' to 'linewidth'
  geom_line(aes(y = Linear_Pred), color = "red", linetype = "dashed") +
  geom_line(aes(y = Poly_Pred), color = "blue", linetype = "dotted") +
  geom_line(aes(y = Exp_Pred), color = "green", linetype = "longdash") +
  labs(title = "Comparison of Trend Fits", x = "Time", y = "Data") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Data" = "black", "Linear" = "red", "Polynomial" = "blue", "Exponential" = "green"))

#code to display model summaries for comparison
summary(fit_linear)
summary(fit_poly)
summary(fit_exp)

#code to create a numerical time index that starts at 1 for the first observation to get the equation
start_year <- start(tsdata)[1]
years_since_start <- floor(time(tsdata)) - start_year
months_since_start <- cycle(tsdata) + years_since_start * frequency(tsdata)
time_index <- months_since_start

# Fit the linear regression model
trend_model_for_equation <- lm(tsdata ~ time_index)

# Summary of the model to see coefficients
summary(trend_model_for_equation)

################################################################################

#code to plot the residuals plot
detrend=residuals(trend_model)
plot.ts(detrend)

#smoothing
#calculate moving average
plot(tsdata)
tsm <- ma(tsdata,order=12,centre = TRUE)
lines(tsm,col="red")
summary(tsm)


#simple exponential smoothing
ses <- ses(tsdata,h=12, alpha = NULL)
autoplot(ses, main = "Simple exponential smoothing", xlab = "Month", ylab = "Trade")
summary(ses)

#holt smoothing
holt <- holt(tsdata,h=12)
autoplot(holt, xlab = "Month", ylab = "Trade")
summary(holt)

#holt winter additive
hwa <- hw(tsdata,h=12)
autoplot(hwa, xlab = "Month", ylab = "Trade")
summary(hwa)

#holt winter multiplicative
hwm <- hw(tsdata,h=12, alpha=.2, beta=.2, level=c(95),
           seasonal = "multiplicative")
autoplot(hwm, xlab = "Month", ylab = "Trade")
summary(hwm)

#code to plot ETS smoothing
ets <- ets(tsdata)
autoplot(ets)
summary(ets)

accuracy(ses)
accuracy(holt)
accuracy(hwa)
accuracy(hwm)
accuracy(ets)

#code to decompose the timeseries
tsdata_dec <- tsdata %>% decompose(type="multiplicative")%>%
  autoplot() + xlab("Month") +
  ggtitle("Multiplicative Decomposition of Movin Timeseries")

#X11 decomposition
fit <- tsdata %>% seas(x11="")
autoplot(fit) + ggtitle("X11 decomposition of Movin")

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

#Forecasting with decomposition
tsdata_uni <- ts(tsdata[,1], start=start(tsdata), frequency=frequency(tsdata))

fitstl <- stl(tsdata_uni, s.window = "periodic")
tsdata_adj <- seasadj(fitstl)

ets_model <- ets(tsdata_adj)

ets_forecast <- forecast(ets_model, h = 12)
autoplot(ets_forecast)


#-------------------------------------------------------
tsdata = ts(trend, start = c(2011,1), frequency = 12)

autoplot(tsdata)
m_acf <-Acf(tsdata,lag=NULL)
autoplot(m_acf)

tsdisplay(diff(tsdata))

m_pacf <-Pacf(tsdata,lag=50)
autoplot(m_pacf)

#----------stationarity-------------------#

tsdata = ts(trend, start = c(2011,1), frequency = 12)
plot(tsdata)

m_acf <-Acf(tsdata,lag=100)
autoplot(m_acf) #tells the series is non stationary

#pacf after diff
pacf <- Pacf(tsdata,lag=50) #P seems to be 1, D = 1 

#transforming
log_tsdata <- log(tsdata)
autoplot(log_tsdata)

#Differencing the transformed data to remove trend
d <- diff(log_tsdata, lag = 1)
#d <- diff(d, lag =12)
autoplot(d)

#boxcox to remove seasonality
bc_trans <- BoxCox.lambda(d)
tsdata_transformed <- BoxCox(d, bc_trans)
autoplot(tsdata_transformed)

# Fit an AR model to the transformed data
fit <- ar(tsdata_transformed)
residuals_squared <- residuals(fit)^2

# Perform the Ljung-Box test on the squared residuals
Box.test(residuals_squared, lag = 12, type = "Ljung-Box")


#acf after diff
Acf(tsdata_transformed,lag=100)

#pacf after diff
pacf_tfd <- Pacf(tsdata_transformed,lag=24) 
plot(pacf_tfd, main = "Movin transformed PACF")

#order of arima 1st candidate model
fit1 <- Arima(log_tsdata, order=c(4,1,1), seasonal = c(1,1,1), include.constant = FALSE)
summary(fit1)
checkresiduals(fit1)

#order of arima 2nd candidate model
fit2 <- Arima(log_tsdata, order=c(4,1,1), seasonal = c(0,1,1), include.constant = FALSE)
summary(fit2)
checkresiduals(fit2)

#order of arima 3rd candidate model
fit3 <- Arima(log_tsdata, order=c(4,1,0), seasonal = c(0,1,1), include.constant = FALSE)
summary(fit3)
checkresiduals(fit3)

forecasts_sarima <- forecast(fit3, h=12)  # for example, forecast 12 periods ahead
autoplot(forecasts_sarima)
summary(forecasts_sarima)
forecast_df <- as.data.frame(forecasts_sarima)

#residuals plot for selected sarima model
# Get residuals from the ARIMA model
residuals_fit <- residuals(fit3) # Use your ARIMA model variable name

# Create a plot of residuals over time
ggplot(data = data.frame(Time = time(residuals_fit), Residuals = as.numeric(residuals_fit)), aes(x = Time, y = Residuals)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Time",
       y = "Residuals")


m_after <- seas(tsdata_transformed)
summary(m_after)
view(m_after)
