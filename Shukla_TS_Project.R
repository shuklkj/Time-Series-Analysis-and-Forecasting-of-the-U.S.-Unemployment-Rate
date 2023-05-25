# install.packages("xts")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("prophet")
# install.packages("dplyr")



library(lubridate)
library(ggplot2)
library(ggfortify)
library(forecast)
library(tseries)
library(MASS)
library(xts)
library(gridExtra)
library(dplyr)
library(prophet)



#Load dataset
unempRate_data <- read.csv('~/Desktop/Final Proj/unrate_train_data.csv')
test_data <- read.csv('~/Desktop/Final Proj/unrate_test_data.csv')

# rename column
names(unempRate_data)[names(unempRate_data) == "DATE"] <- "date"
names(unempRate_data)[names(unempRate_data) == "UNRATE"] <- "unempl_rate"


#Convert 'Date' column to a time series object
unempRate_data$date <- as.Date(unempRate_data$date)
str(unempRate_data)
summary(unempRate_data)


############### EDA on original data ###############

#Visualize the unemployment rate over time
ggplot(unempRate_data, aes(x = date, y = unempl_rate)) +
  geom_line() + 
  labs(x = "Date", y = "Unemployment Rate") + 
  ggtitle("Unemployment rate over time")
# line plot showing the Unemployment Rate trends over time

#Distribution of the Unemployment Rate
ggplot(unempRate_data, aes(x = unempl_rate)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  labs(x = "Unemployment Rate", y = "Frequency") +
  ggtitle("Distribution of Unemployment Rate")

#Check for null values in the 'unempRate'
null_values <- sum(is.na(unempRate_data))
null_values

######### TIME SERIES DATA FOR 6 MONTH MOVING AVERAGE & STL DECOMPOSITION #########

ts_data <- ts(unempRate_data$unempl_rate, start = c(1950,1), frequency = 12)

# Convert test_data to a time series object
test_ts_data <- ts(test_data$UNRATE, start = c(2022,1), frequency = 12)

acf(ts_data, main = "ACF of Unemployment Rate")
# There is a decay pattern as the plot shows an exponential decrease as the lab increases
# it suggest there's a correaltion present and that the differencing is needed. 

# 2. Plot PACF
pacf(ts_data, main = "PACF of Unemployment Rate")

# KPSS Test
kpss_test <- kpss.test(ts_data)
kpss_test
cat("KPSS p-value:", kpss_test$p.value, "\n")




# ------------- MOVING AVERAGE -----------------
moving_avg <- stats::filter(ts_data, rep(1/6, 6), sides = 2, circular = TRUE)

# Plot the original time series and the moving average
plot(ts_data, main = "Original Time Series")

lines(moving_avg, col = "red", lwd = 2)

legend("topleft", legend = c("Original", "6-month Moving Average"), col = c("black", "red"), lwd = c(1, 2))

acf(moving_avg, main = "ACF of Unemployment Rate")
# There is a decay pattern as the plot shows an exponential decrease as the lab increases
# it suggest there's a correaltion present and that the differencing is needed. 

# 2. Plot PACF
pacf(moving_avg, main = "PACF of Unemployment Rate")


# ------------- STL DECOMPOSITION METHOD  -----------------
# Seasonal Decomposition
decomposed <- decompose(moving_avg)
# Plotting the decomposition components
plot(decomposed)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = 0.5)

# Remove missing values from the random component
random_component <- decomposed$random[!is.na(decomposed$random)]
random_component


# ------------- CHECK FOR STATIONARITY  -----------------

# 1. Plot ACF
acf(random_component, main = "ACF of Unemployment Rate")

# 2. Plot PACF
pacf(random_component, main = "PACF of Unemployment Rate")

# Augmented Dickey-Fuller Test
adf_test <- adf.test(random_component)
adf_test
cat("ADF p-value:", adf_test$p.value, "\n")

# KPSS Test
kpss_test <- kpss.test(random_component)
kpss_test
cat("KPSS p-value:", kpss_test$p.value, "\n")


################ TRANSFORMATION: FIRST ORDER DIFFERINCING ON TRAIN DATA ################

# Perform differencing to make the data stationary
differenced_data <- diff(ts_data, differences = 1)

par(mfrow = c(1, 3))
# Plot the differenced data
plot(differenced_data, main = "Differenced Time Series")

# Check for stationarity after differencing
adf_test <- adf.test(differenced_data)
kpss_test <- kpss.test(differenced_data)

# Print the results of the stationarity tests
cat("ADF Test p-value:", adf_test$p.value, "\n")
cat("KPSS Test p-value:", kpss_test$p.value, "\n")

# 1. Plot ACF
acf(differenced_data, main = "ACF of Differenced Unemployment Rate")

# 2. Plot PACF
pacf(differenced_data, main = "PACF of Differenced Unemployment Rate")
dev.off()

################ TRANSFORMATION: FIRST ORDER DIFFERINCING ON TEST DATA ################

# Apply first-order differencing to the test data
differenced_test <- diff(test_ts_data)


################ MODEL ################

# Find optimal orders using auto.arima
auto_arima_model_differenced <- auto.arima(differenced_data, seasonal = FALSE)

summary(auto_arima_model_differenced)

# Fit ARIMA model with optimal orders
arima_model_diff <- arima(differenced_data, order = c(1,0,1))

summary(arima_model_diff)

# Generate forecasts for future time periods
forecast_values <- forecast(arima_model_diff, h = length(differenced_test))

# Transform the forecasted differenced values back to the original scale
forecasted_values <- forecast_values$mean + stats::lag(test_ts_data)


# Plot the forecasted values against the actual values
plot(test_ts_data, main = "ARIMA Forecast")
lines(forecasted_values, col = "red")

arima_model_diff %>% forecast(h = length(differenced_test)) %>% autoplot() +
  ylab("Predicted unemployment rate using ARIMA model")

#### EVALUATION METRICS ON ARIMA MODEL ####

# Calculate MAPE
mape_arima <- mean(abs((test_ts_data - forecasted_values) / test_ts_data)) * 100

# Calculate MAE
mae_arima <- mean(abs(test_ts_data - forecasted_values))

# Calculate RMSE
rmse_arima <- sqrt(mean((test_ts_data - forecasted_values)^2))


cat("MAE: ", mae_arima, "\n")  # MAE:  0.1220111
cat("RMSE: ", rmse_arima, "\n") # RMSE:  0.1465348
cat("MAPE: ", mape_arima, "%\n") # MAPE:  3.32148 %

# Calculate AIC, AICc, and BIC for ARIMA model
arima_aic <- AIC(arima_model_diff)
arima_aic

arima_bic <- BIC(arima_model_diff)
arima_bic



#################### EXPONENTIAL SMOOTHING #################### 

# Fit the ETS model
ets_model <- ets(differenced_data)
summary(ets_model)

ets_forecast_values <- forecast(ets_model, h = length(differenced_test))

# Transform the forecasted differenced values back to the original scale
ets_forecast_values <- ets_forecast_values$mean + stats::lag(test_ts_data)

# Plot the forecasted values
plot(test_ts_data, main = "ETS Forecast")
lines(ets_forecast_values, col = "red")

ets_model %>% forecast(h = length(differenced_test)) %>% autoplot() +
  ylab("Predicted unemployment rate using ETS model")

#### EVALUATION METRICS ON EXPONENTIAL SMOOTHING MODEL ####

# Calculate MAPE
mape_ets <- mean(abs((test_ts_data - ets_forecast_values) / test_ts_data)) * 100

# Calculate MAE
mae_ets <- mean(abs(test_ts_data - ets_forecast_values))

# Calculate RMSE
rmse_ets <- sqrt(mean((test_ts_data - ets_forecast_values)^2))


cat("MAE: ", mae_ets, "\n") # MAE:  0.1201122 
cat("RMSE: ", rmse_ets, "\n") # RMSE:  0.1455818
cat("MAPE: ", mape_ets, "%\n") # MAPE:  3.265728 %

# Calculate AIC, AICc, and BIC for ETS model
ets_aic <- AIC(ets_model)
ets_aic

ets_bic <- BIC(ets_model)
ets_bic

###################### COMPARE ######################

# Create a dataframe with the evaluation metrics
metrics_df <- data.frame(Model = c("ARIMA", "ETS"),
                         MAE = c(mae_arima, mae_ets),
                         RMSE = c(rmse_arima, rmse_ets),
                         MAPE = c(mape_arima, mape_ets))

# Print the dataframe
print(metrics_df)


# Create a dataframe of AIC and BIC values
metrics_df <- data.frame(Model = c("ARIMA", "ETS"),
                         AIC = c(arima_aic, ets_aic),
                         BIC = c(arima_bic, ets_bic))

# Print the dataframe
print(metrics_df)












