# Importing Libraries
library(tidyverse)
library(ISLR2)
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast)
library(reshape2)
library(Metrics)

# Step 1 - Data
# Importing House hold power consumption dataset. 
power_data <- read_delim("D:/Predictive Analytics/Semester 2/Forcasting/Individual Project/household_power_consumption.txt", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(power_data, 5)

# Shape of the dataset
dim(power_data)

# Here the shape of the dataset is too much to perform the analysis smoothly. So, I will only take a small sample from this dataset.
sampled_power_data <- head(power_data, 10000)

view(sampled_power_data)

# I found this dataset on Kaggle (https://www.kaggle.com/datasets/uciml/electric-power-consumption-data-set).
# The dataset was too large for a smooth and convienent analysis that why i have only taken 10000 entries. 

# Further, I will check for missing values and irregularities in the dataset.

missing_values <- colSums(is.na(sampled_power_data))
missing_values

# Here we can see that only column Sub_metering_3 have 2 missing values

# Removing missing values from the dataset.
dataset_cleaned <- na.omit(sampled_power_data)
head(dataset_cleaned, 5)

colSums(is.na(dataset_cleaned))

# Problem Description
# The goal of this project is to develop a forecasting model that accurately predicts future electricity consumption. The dataset includes variables such as global active power, global reactive power, voltage, and sub-metering measurements!

# Accurate electricity consumption forecasting is important for a number of reasons, including:
# 1. Plan and operate power systems to avoid blackouts or brownouts.
# 2. Invest in new power plants to meet demand.
# 3. Design and target energy efficiency programs to reduce electricity consumption.
# 4. Set the right price for electricity to ensure a fair market.

# Step 2 - Visualization
par(mar = c(5, 4, 4, 2) + 0.1)
# Convert Date and Time columns to appropriate data types
dataset_cleaned$Date <- as.Date(dataset_cleaned$Date, format = "%d/%m/%Y")
dataset_cleaned$Time <- as.POSIXct(dataset_cleaned$Time, format = "%H:%M")
dataset_cleaned$Global_active_power <- as.numeric(dataset_cleaned$Global_active_power)
dataset_cleaned$Global_reactive_power <- as.numeric(dataset_cleaned$Global_reactive_power)
dataset_cleaned$Voltage <- as.numeric(dataset_cleaned$Voltage)
dataset_cleaned$Global_intensity <- as.numeric(dataset_cleaned$Global_intensity)
dataset_cleaned$Sub_metering_1 <- as.numeric(dataset_cleaned$Sub_metering_1)
dataset_cleaned$Sub_metering_2 <- as.numeric(dataset_cleaned$Sub_metering_2)

str(dataset_cleaned)

# Summary statistics
summary(dataset_cleaned)

# Descriptive analysis
mean_power <- mean(as.numeric(dataset_cleaned$Global_active_power), na.rm = TRUE)
max_power <- max(as.numeric(dataset_cleaned$Global_active_power), na.rm = TRUE)
min_power <- min(as.numeric(dataset_cleaned$Global_active_power), na.rm = TRUE)

cat("Mean power consumption:", mean_power, "\n")
cat("Maximum power consumption:", max_power, "\n")
cat("Minimum power consumption:", min_power, "\n")

# Basic visualizations

# Time plot
# Set the plot size
options(repr.plot.width = 12, repr.plot.height = 6)

# Create the line plot
ggplot(dataset_cleaned, aes(x = Time, y = Global_active_power)) +
  geom_line() +
  labs(x = "Time", y = "Global Active Power", title = "Global Active Power over Time") +
  theme_minimal()

# This line chart visualizes the "Global Active Power" variable over time. This chart doesn't have many patterns or trends but the only thing which caught my eye was the immense hike each year.

# ACF plot
# Convert the Global_active_power column to a time series object
ts_data <- ts(dataset_cleaned$Global_active_power)

# Create the ACF plot
acf_plot <- acf(ts_data, main = "Autocorrelation Function (ACF) Plot", xlab = "Lag", ylab = "Autocorrelation")

# The Autocorrelation Function (ACF) plot provides insights into the data's correlation and patterns over time. By examining the ACF plot, I can identify significant peaks and tails, indicating strong correlations or gradual declines. Significant lag values signify patterns or dependencies repeating at specific intervals. Periodic patterns in the ACF plot indicate seasonality, while small and fluctuating autocorrelation coefficients suggest randomness and a lack of significant correlation between the variable and its lagged values. These observations help in understanding the data's correlation structure and identifying trends, seasonality, and randomness in the "Global_active_power" variable.


# Convert Global_active_power to time series object
ts_data <- ts(dataset_cleaned$Global_active_power, frequency = 1440)  # Assuming data is recorded every minute (1440 minutes in a day)

# Create the seasonal subseries plot
seasonal_plot <- forecast::seasonplot(ts_data, year.labels = TRUE)

# The seasonal subseries plot provides valuable information for analyzing time series data. By examining the plot, I can identify seasonal patterns such as trends, cyclical variations, and shifts in the data distribution. Comparing subseries patterns across different years allows you to detect consistent changes or deviations, offering insights into long-term trends or shifts in the data. Additionally, outliers or anomalies within specific seasons or years can be identified, potentially indicating abnormal events, measurement errors, or exceptional conditions that warrant further investigation. Overall, the seasonal subseries plot helps in understanding seasonal patterns, detecting changes over time, and identifying outliers or anomalies within the "Global_active_power" variable.

# Select the numeric columns for the heatmap
heatmap_cols <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2")

# Create a subset of the dataset with the selected columns
heatmap_data <- dataset_cleaned[, heatmap_cols]

# Compute the correlation matrix
correlation_matrix <- cor(heatmap_data)

# Reshape the correlation matrix into a long format
correlation_data <- reshape2::melt(correlation_matrix)

# Create the heatmap using ggplot2
ggplot(correlation_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "white", midpoint = 0, na.value = "grey") +
  labs(x = "", y = "", fill = "Correlation") +
  ggtitle("Heatmap of Variable Correlations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# The heatmap visually represents the correlations between selected numeric variables in the dataset. By observing the color intensity of the cells, I can determine the strength of the correlations, with darker colors indicating stronger relationships and lighter colors suggesting weaker or no correlations. Analyzing the clusters or patterns of color helps identify groups of variables with high or low correlations, offering insights into the relationships between them. It's important to note that the heatmap focuses on the selected variables, allowing you to identify associations or dependencies specific to those variables.

# Step 3 - Transformation
# Perform the decomposition
decomposed_data <- decompose(ts_data)

# Extract the components
trend <- decomposed_data$trend
seasonal <- decomposed_data$seasonal
random <- decomposed_data$random

# Plot the components
par(mfrow = c(4, 1))  # Set up a 4-row grid for subplots
plot(ts_data, main = "Original Data")
plot(trend, main = "Trend Component")
plot(seasonal, main = "Seasonal Component")
plot(random, main = "Random Component")

# The provided code performs a time series decomposition to extract information about the trend and seasonality in the data. It assumes the data represents the variable "Global_active_power" recorded at a daily frequency. The code applies the decompose() function to the time series data, which separates it into three components: trend, seasonal, and random. The trend component reveals the long-term behavior and direction of the variable, helping identify any upward or downward patterns. The seasonal component captures the recurring patterns or cycles within the data, indicating seasonality effects. The random component represents the residual or unexplained variation in the data after removing the trend and seasonal components. By decomposing the time series, the code provides insights into the underlying trends, seasonal patterns, and random fluctuations in the "Global_active_power" variable.

# Naive mean modelling

# Split the dataset into training and testing sets
train_data <- dataset_cleaned[1:800, ]  # Example: Use first 800 rows for training
test_data <- dataset_cleaned[801:nrow(dataset_cleaned), ]  # Example: Use remaining rows for testing

# Calculate the mean of the training data
mean_value <- mean(train_data$Global_active_power)

# Create a vector of the mean values for the length of the testing set
forecasted_values <- rep(mean_value, length(test_data$Global_active_power))

# Plot the actual values and forecasted values
ggplot() +
  geom_line(data = test_data, aes(x = 1:length(Global_active_power), y = Global_active_power), color = "blue", linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(data = test_data, aes(x = 1:length(forecasted_values), y = forecasted_values), color = "red", linetype = "dashed", size = 1, alpha = 0.8) +
  labs(title = "Actual Values vs. Forecasted Values", x = "Time", y = "Global Active Power") +
  theme_minimal()

# Evaluate the forecast using appropriate evaluation metrics
# Example: Calculate RMSE
rmse_value <- rmse(forecasted_values, test_data$Global_active_power)

# Print the RMSE value
print(paste("RMSE:", rmse_value))


# The RMSE (Root Mean Squared Error) value of 1.593 indicates that, on average, the Naive Mean model's predictions for Global_active_power differ by approximately 1.593 units from the actual values in the testing set. A lower RMSE suggests a better fit between predicted and actual values. However, the interpretation of the RMSE depends on the scale and context of the data.

# To assess the prediction, consider the range and variability of Global_active_power. If the RMSE is small compared to the range, it implies a reasonably accurate prediction. Conversely, if the RMSE is large relative to the range, the model may struggle to capture underlying patterns or variations in the data effectively.

# For a comprehensive evaluation, it is advisable to utilize additional metrics and compare the Naive Mean model's performance with other forecasting techniques. This helps determine if the model provides sufficiently accurate predictions for Global_active_power.

# Exponential smooth modelling

# Convert Global_active_power to time series object
ts_traindata <- ts(dataset_cleaned$Global_active_power, frequency = 1440)  # Assuming data is recorded every minute (1440 minutes in a day)

# Fit the Exponential Smoothing model
model <- ets(ts_traindata)

# Generate forecasts
forecasts <- forecast(model)

# Print the forecasts
print(forecasts)

# Plot the actual values and forecasted values
ggplot() +
  geom_line(data = ts_traindata, aes(x = time(ts_traindata), y = ts_traindata), color = "blue", linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(data = forecasts$mean, aes(x = time(forecasts$mean), y = forecasts$mean), color = "red", linetype = "dashed", size = 1, alpha = 0.8) +
  labs(title = "Actual Values vs. Forecasted Values", x = "Time", y = "Global Active Power") +
  theme_minimal()


# The provided code fits an Exponential Smoothing model to the Global_active_power data and generates forecasts. The resulting forecasts are then visualized alongside the actual values. The ts_traindata is converted into a time series object, and the ets() function is used to fit the Exponential Smoothing model. The forecast() function is applied to the fitted model to generate forecasts. The code then plots the actual values and the forecasted values using ggplot2, with the actual values shown in blue and the forecasted values shown in red as a dashed line. By comparing the actual and forecasted values, we can visually assess the performance of the Exponential Smoothing model. This code provides a simple and effective way to analyze and visualize the forecasted values and gain insights into the accuracy of the model.

# Comparing the accuracy of the models using matrices

# Calculate the naive mean model
naive_mean <- rep(mean(dataset_cleaned$Global_active_power), length(dataset_cleaned$Global_active_power))

# Fit the exponential smoothing model
exponential_model <- ets(dataset_cleaned$Global_active_power)

# Make predictions using both models
naive_mean_pred <- naive_mean
exponential_pred <- forecast(exponential_model, h = length(dataset_cleaned$Global_active_power))$mean

# Calculate the metrics
naive_mean_mae <- mean(abs(dataset_cleaned$Global_active_power - naive_mean_pred))
naive_mean_rmse <- sqrt(mean((dataset_cleaned$Global_active_power - naive_mean_pred)^2))

exponential_mae <- mean(abs(dataset_cleaned$Global_active_power - exponential_pred))
exponential_rmse <- sqrt(mean((dataset_cleaned$Global_active_power - exponential_pred)^2))

# Print the results
cat("Naive Mean Model - MAE:", naive_mean_mae, "\n")
cat("Naive Mean Model - RMSE:", naive_mean_rmse, "\n\n")

cat("Exponential Smoothing Model - MAE:", exponential_mae, "\n")
cat("Exponential Smoothing Model - RMSE:", exponential_rmse, "\n")

# The comparison between the naive mean and exponential smoothing models reveals that the naive mean model demonstrates superior accuracy. With a mean absolute error (MAE) of 1.096645 and a root mean squared error (RMSE) of 1.339839, the naive mean model outperforms the exponential smoothing model. In contrast, the exponential smoothing model yields less accurate results, exhibiting an MAE of 17.43968 and an RMSE of 19.36913. These values indicate significant deviations between the model's predictions and the actual values. Thus, based on the MAE and RMSE metrics, the naive mean model provides more accurate predictions for the Global_active_power in this dataset. However, further evaluation of additional metrics and consideration of other factors is essential to form a comprehensive assessment of the model's performance.


# Residual Analysis of the models

# Residual analysis for Naive Mean model
naive_mean_residuals <- dataset_cleaned$Global_active_power - naive_mean

# Residual analysis for Exponential Smoothing model
exponential_smoothing_residuals <- dataset_cleaned$Global_active_power - exponential_model$fitted

# Plotting histogram of residuals
par(mfrow = c(2, 1))
hist(naive_mean_residuals, main = "Naive Mean", xlab = "Residuals")
hist(exponential_smoothing_residuals, main = "Exponential Smoothing", xlab = "Residuals")

# Plotting scatterplot of residuals against predicted values
plot(naive_mean, naive_mean_residuals, main = "Naive Mean",
     xlab = "Predicted Values", ylab = "Residuals")
plot(exponential_model$fitted, exponential_smoothing_residuals, main = "Exponential Smoothing",
     xlab = "Predicted Values", ylab = "Residuals")

# Checking normality of residuals using Q-Q plot
par(mar = c(5, 4, 4, 2) + 0.1)
qqnorm(naive_mean_residuals, main = "Q-Q Plot - Naive Mean")
qqline(naive_mean_residuals)
qqnorm(exponential_smoothing_residuals, main = "Q-Q Plot - Exponential")
qqline(exponential_smoothing_residuals)

# Checking for heteroscedasticity using a plot of residuals vs. predicted values
plot(naive_mean, abs(naive_mean_residuals), main = "Naive Mean",
     xlab = "Predicted Values", ylab = "Absolute Residuals")
plot(exponential_model$fitted, abs(exponential_smoothing_residuals), main = "Exponential Smoothing",
     xlab = "Predicted Values", ylab = "Absolute Residuals")

# After comparing the Mean Absolute Error (MAE) and Root Mean Square Error (RMSE) values for the Naive Mean and Exponential Smoothing models, it is evident that the Naive Mean model delivered better forecasts. The Naive Mean model achieved an MAE of 1.096645 and an RMSE of 1.339839, whereas the Exponential Smoothing model had an MAE of 17.43968 and an RMSE of 19.36913.

# The lower values of MAE and RMSE for the Naive Mean model indicate that its forecasts were closer to the actual values compared to the Exponential Smoothing model. It is important to note that the Naive Mean model simply relies on the average of the training data, while the Exponential Smoothing model considers trend and seasonality. However, in this particular case, the Naive Mean model outperformed the Exponential Smoothing model in terms of forecast accuracy.

# Therefore, based on the comparison of the MAE and RMSE values, the Naive Mean model can be concluded to have delivered the best forecasts for the given dataset.

