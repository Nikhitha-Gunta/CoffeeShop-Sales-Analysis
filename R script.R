

#source:https://www.kaggle.com/datasets/ahmedabbas757/coffee-sales/data

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(caret)
library(forecast)
library(tseries)

# Read the data
sales_Original_data <- read_excel("E:/R Final Project/Coffee Shop Sales.xlsx")

########################### Data cleaning ################################

#Checking the dataset
colnames(sales_Original_data)
head(sales_Original_data)
summary(sales_Original_data)
str(sales_Original_data)

sales_data <- sales_Original_data

# This converts the transaction_time to a character, extracts the time part, and then converts it to a POSIXct time format.
sales_data$transaction_time <- format(as.POSIXct(substr(as.character(sales_data$transaction_time), 12, 19), format = "%H:%M:%S"), "%T")

# Standardize text columns
sales_data$store_location <- tolower(sales_data$store_location)
sales_data$product_category <- tolower(sales_data$product_category)
sales_data$product_type <- tolower(sales_data$product_type)
sales_data$product_detail <- tolower(sales_data$product_detail)


# Handle numerical variables that should be categorical
sales_data$store_id <- as.factor(sales_data$store_id)
sales_data$product_id <- as.factor(sales_data$product_id)

# Check for and remove duplicate rows 
sales_data <- sales_data %>%
  distinct()

#Combining columns
sales_data <- sales_data %>%
  unite(product_detail, product_type, product_detail, sep = " - ")

# Creating a new column 'Total_Sales' by multiplying 'unit_price' and 'transaction_qty'
sales_data <- sales_data %>%
  mutate(Total_Sales = unit_price * transaction_qty)

# View the first few rows to confirm the new column has been added
head(sales_data)

###############################Data Visualization#############################


# Calculating sums of Total_Sales for each product_type
product_sales <- sales_data %>%
  group_by(product_category) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()


#Pie Chart
ggplot(product_sales, aes(x = "", y = Total_Sales, fill = product_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Product Type",
       title = "Total sales for each product category")

# To understand the distribution of 'unit_price'
ggplot(sales_data, aes(x = unit_price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Unit Prices",
       x = "Unit Price",
       y = "Count") +
  theme_minimal()


# Summarize data to find the total quantity sold and total revenue per product
product_sales_summary <- sales_data %>%
  group_by(product_detail) %>%
  summarise(
    Total_Quantity = sum(transaction_qty),
    Total_Revenue = sum(Total_Sales),
    .groups = 'drop'
  ) 

# Sort to find the most and least sold products
most_sold_products <- product_sales_summary %>%
  arrange(desc(Total_Quantity))


least_sold_products <- product_sales_summary %>%
  arrange(Total_Quantity)


# Sort to find which products generate the most and least revenue
most_revenue_products <- product_sales_summary %>%
  arrange(desc(Total_Revenue))

least_revenue_products <- product_sales_summary %>%
  arrange(Total_Revenue)


# Plotting the top 10 products by quantity sold

ggplot(most_sold_products[1:10,], aes(x = reorder(product_detail, Total_Quantity), y = Total_Quantity)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use a single color for all bars
  coord_flip() +  # Horizontal bars
  labs(title = "Top 10 Most Sold Products",
       x = "Product",
       y = "Total Quantity Sold") +  # Remove the y-axis label as the product names are clear
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend


# Plotting the top 10 products by revenue
ggplot(most_revenue_products[1:10,], aes(x = reorder(product_detail, Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use a single color for all bars
  coord_flip() +  # Horizontal bars
  labs(title = "Top 10 Products by Revenue",
       x = "Product",
       y = "Total Revenue") +  # Remove the y-axis label as the product names are clear
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend


# Aggregate the weekly quantity sold
weekly_qty_sold <- sales_data %>%
  group_by(Week = floor_date(transaction_date, "week")) %>%
  summarise(Weekly_Quantity = sum(transaction_qty), .groups = 'drop')

# Line chart for weekly quantity sold over time
ggplot(weekly_qty_sold, aes(x = Week, y = Weekly_Quantity)) +
  geom_line(color = "steelblue") +  # Use a line to connect the points
  labs(title = "Weekly Quantity of Transactions Over Time",
       x = "Week",
       y = "Quantity Sold") +
  theme_minimal()

#############################################Data modeling#############################################################

## Time Series Analysis

# Convert 'transaction_date' to a Date type if it isn't already
sales_data$transaction_date <- as.Date(sales_data$transaction_date)


# Aggregate sales data by date to get daily total sales
daily_sales <- sales_data %>%
  group_by(transaction_date) %>%
  summarise(Daily_Total_Sales = sum(Total_Sales), .groups = 'drop')


# Create a time series object from the daily sales totals
ts_sales <- ts(daily_sales$Daily_Total_Sales, frequency = 1)


# Check for stationarity; if the p-value is less than a significance level (e.g., 0.05), the series is considered stationary
adf_test_result <- adf.test(ts_sales)


if(adf_test_result$p.value > 0.05) {
  ts_sales_diff <- diff(ts_sales)
  # Redo the stationarity test on the differenced data
  adf_test_result_diff <- adf.test(ts_sales_diff)
}



# Use the (differenced) series to fit an ARIMA model
# The auto.arima function will determine whether differencing is needed, so we can pass the original series
best_fit <- auto.arima(ts_sales)


# Forecast the next period (e.g., the next 30 days)
forecasted_sales <- forecast(best_fit, h = 30)


# Plot the forecast
plot(forecasted_sales)


# fitted ARIMA model
fitted_values <- fitted(best_fit)

# Create a time series plot comparing the observed and fitted values
plot(ts_sales, main="Observed vs Fitted Values", col="blue")
lines(fitted_values, col="red")
legend("topright", legend=c("Observed", "Fitted"), col=c("blue", "red"), lty=1)


# Calculate residuals
residuals <- residuals(best_fit)

# Basic plot of residuals
plot(residuals, main="Residuals from ARIMA Model", ylab="Residuals")
abline(h=0, col="red")

# ACF and PACF plots to check for autocorrelation in residuals
Acf(residuals, main="ACF of Residuals")
Pacf(residuals, main="PACF of Residuals")
