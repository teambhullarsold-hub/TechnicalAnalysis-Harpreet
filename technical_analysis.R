# BDA400 Assignment 2
# Technical Analysis Using R - Preliminary Stage
# Student: Harpreet Bhullar

library(quantmod)
library(TTR)

# Function to load stock data
load_stock_data <- function(file_name) {
  
  symbols <- scan(file_name, what = "character", quiet = TRUE)
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(
      symbol,
      src = "yahoo",
      from = "2026-01-01",
      auto.assign = FALSE
    )
    
    stock_data[[symbol]] <- data
  }
  
  return(stock_data)
}

# Function to calculate mode
calculate_mode <- function(x) {
  x <- na.omit(x)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Function to calculate statistics
calculate_statistics <- function(stock) {
  
  closing_prices <- as.numeric(Cl(stock))
  
  statistics <- data.frame(
    Mean = mean(closing_prices, na.rm = TRUE),
    Median = median(closing_prices, na.rm = TRUE),
    Mode = calculate_mode(closing_prices),
    Standard_Deviation = sd(closing_prices, na.rm = TRUE),
    Latest_20_Day_Moving_Average =
      as.numeric(tail(SMA(closing_prices, n = 20), 1))
  )
  
  return(statistics)
}

# Load portfolio data
stocks <- load_stock_data("portfolio.txt")

# Display imported stock data
for (symbol in names(stocks)) {
  cat("\nStock Data:", symbol, "\n")
  print(head(stocks[[symbol]]))
}

# Calculate and display statistics
for (symbol in names(stocks)) {
  cat("\nStatistics:", symbol, "\n")
  print(calculate_statistics(stocks[[symbol]]))
}

# Create stock charts
for (symbol in names(stocks)) {
  chartSeries(
    stocks[[symbol]],
    name = paste(symbol, "Stock Price"),
    theme = chartTheme("white")
  )
}


