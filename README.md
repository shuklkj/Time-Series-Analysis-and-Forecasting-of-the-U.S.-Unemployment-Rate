# Time-Series-Analysis-and-Forecasting-of-the-U.S.-Unemployment-Rate

This project aims to predict the unemployment rate using various time series forecasting methods such as ARIMA and Exponential Smoothing (ETS). The project utilizes the monthly unemployment rate data and is split into training and testing datasets.

## Requirements:

1. R
2. Libraries:
    1. lubridate
    2. ggplot2
    3. ggfortify
    4. forecast
    5. tseries
    6. MASS
    7. xts
    8. gridExtra
    9. dplyr
    10. prophet

## Key Components:

1. Exploratory Data Analysis (EDA): Visualized the unemployment rate trend over time and its distribution. Checked for null values.
2. Time Series Data Preparation: Performed operations such as moving average calculation and STL decomposition to identify patterns and components in the data.
3. Stationarity Checks: Applied the Augmented Dickey-Fuller and KPSS tests to evaluate stationarity.
4. Data Transformation: Implemented first order differencing to make the time series data stationary.
5. Modeling: Built ARIMA and ETS forecasting models.
6. Evaluation: Evaluated the models using metrics like MAE, RMSE, MAPE, AIC, and BIC.

## Instructions:

1. Set the appropriate working directory containing the unrate_train_data.csv and unrate_test_data.csv.
2. Run the R script. Ensure the required packages are installed.
3. Upon executing, the script will:
    1. Load and preprocess the data.
    2. Perform EDA.
    3. Prepare the data for time series analysis.
    4. Check for stationarity.
    5. Apply required transformations.
    6. Build and evaluate the ARIMA and ETS forecasting models.
    7. Compare the performance metrics of the two models.
4. Visual plots, decomposition charts, and model evaluation metrics will be displayed during the execution.

## Results:
1. The performance metrics table will provide insights into which model (ARIMA or ETS) performed better for the given data.
2. The visual plots will help in understanding the actual vs predicted values, decomposition trends, and other insights about the unemployment rate time series.





