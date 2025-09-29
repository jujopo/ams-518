library(PSG)
library(dplyr)
library(tidyr)

setwd("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-2/data")

# Set your path to Rdata file including the RData file's name with CS:
load("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-2/data/problem_fc_indtrack1_max_0025_short.RData")

# Load the index data and stock data and delete the date column
log_index_data <- read.csv("dow_index_daily_log_returns.csv")
log_index_data <- log_index_data[2]
stocks_data <- read.csv("stocks_50_daily_log_returns.csv")
stocks_data <- stocks_data[,-1]

# Normalize the stock data
gamma <- 0.015
C <- 1000000
stocks_data <- stocks_data / (C *(1-gamma))

# Get the tickers
stock_tickers <- names(stocks_data)
index_ticker <- names(log_index_data)

# Merge the datasets
merged_data <- cbind(stocks_data, log_index_data)

# Form a matrix out of the merged data
asset_matrix <- as.matrix(merged_data)
header <- names(merged_data)

# Set column names (tickers)
colnames(asset_matrix) <- header

# Update the inmmax matrix
problem.list$matrix_inmmax <- asset_matrix

# Create ksi matrix
# Create a matrix with 51 columns, all ones, and ksi labels
create_ksi_matrix <- function(num_assets = 51) {
  # Generate ksi labels with leading zeros
  ksi_labels <- sprintf("ksi%04d", 1:num_assets)
  
  # Create matrix with ones
  ksi_matrix <- matrix(1, nrow = 1, ncol = num_assets)
  
  # Set column names
  colnames(ksi_matrix) <- ksi_labels
  
  return(ksi_matrix)
}

# Create the ksi matrix with 51 columns
ksi_matrix <- create_ksi_matrix(51)

# Update the ksi_matrix
problem.list$matrix_ksi <- ksi_matrix

# To construct the ksibuy matrix we are going to assume that the minimum
# position we can take in an asset is $15,000
create_ksibuy_matrix <- function(num_assets = 51, value) {
  # Generate ksi labels with leading zeros
  ksibuy_labels <- sprintf("ksi%04d", 1:num_assets)
  
  # Create matrix with ones
  ksibuy_matrix <- matrix(value, nrow = 1, ncol = num_assets)
  
  # Set column names
  colnames(ksibuy_matrix) <- ksibuy_labels
  
  return(ksibuy_matrix)
}

# Create the ksibuy matrix with 51 columns and 15,000 minimum buy
ksibuy_matrix <- create_ksibuy_matrix(51, gamma * C)

# Update the ksibuy matrix
problem.list$matrix_ksibuy <- ksibuy_matrix

# Finally we create the ksi_pol matrix
generate_capital_allocation_matrix <- function(total_capital = C, min_stocks = 20, max_stocks = 30, total_assets = 51) {
  # Generate random number of stocks between min_stocks and max_stocks
  num_selected_stocks <- sample(min_stocks:max_stocks, 1)
  
  # Generate random weights that sum to 1 for selected stocks
  weights <- runif(num_selected_stocks)
  weights <- weights / sum(weights)  # Normalize to sum to 1
  
  # Calculate capital allocation for selected stocks
  capital_allocation <- weights * total_capital
  
  # Select the FIRST N stocks (not random selection)
  selected_indices <- 1:num_selected_stocks
  
  # Create full allocation vectors for all 51 assets
  first_row <- rep(1, total_assets)  # All ones for first row
  second_row <- rep(0, total_assets)  # Zeros for second row initially
  
  # Fill in the capital allocation for the first N stocks
  second_row[selected_indices] <- capital_allocation
  
  # Create ksi labels for all 51 assets
  ksi_labels <- sprintf("ksi%04d", 1:total_assets)
  
  # Create the matrix
  allocation_matrix <- matrix(
    c(first_row, second_row),
    nrow = 2,
    ncol = total_assets,
    byrow = TRUE
  )
  
  # Set column names
  colnames(allocation_matrix) <- ksi_labels
  
  # Set row names
  rownames(allocation_matrix) <- c("[1,]", "[2,]")
  
  return(list(
    matrix = allocation_matrix,
    num_selected_stocks = num_selected_stocks,
    selected_indices = selected_indices,
    weights = weights,
    total_capital = total_capital
  ))
}

# We generate now the capital allocation matrix
result <- generate_capital_allocation_matrix()
allocation_matrix <- result$matrix
problem.list$matrix_ksipol <- allocation_matrix

# Constraint on the number of assets in the new tracking portfolio
problem.list$problem_statement[3] <- "Constraint: <= 15" 

# Constraint prohibiting small positions
problem.list$problem_statement[5] <- "Constraint: <= 0" 

# Bound on sum of “new tracking portfolio value” + “transaction cost” <= “current tracking portfolio value”)
problem.list$problem_statement[7] <- "Constraint: <= 1E+06" 

# We assume the transaction costs are 1.5% the maximum value of the portfolio

# Upper bound on transaction cost
problem.list$problem_statement[10] <- "Constraint: <= 15000" 

# Constraint defines variable and fixed transaction costs
problem.list$problem_statement[12] <- "Constraint: <= 0" 

# Box constraints: We don't allow shorts and one position can't exceed 40% of the maximum budget for the portfolio
problem.list$problem_statement[17] <- "Box: >= 0, <= 400000"

# Run PSG Solver to optimize problem stored in problem.list
results <- rpsg_solver(problem.list)

solution_vector <- results$point_problem_1[results$point_problem_1 != 0]

# Checking the length we can see that the cardinality constraint has been met
length(solution_vector)

stocks_tracking_allocations <- results$point_problem_1
stocks_tracking_allocations <- stocks_tracking_allocations[stocks_tracking_allocations != 0]
stocks_tracking_allocations <- stocks_tracking_allocations[1:10]

names(stocks_tracking_allocations) <- stock_tickers[1:length(stocks_tracking_allocations)]

# Define the portfolio weights
portfolio_weights <- stocks_tracking_allocations / sum(stocks_tracking_allocations)

# To get all 51 weights
full_weights <- rep(0, 51)
full_weights[1:length(portfolio_weights)] <- portfolio_weights

# Convert log returns to simple returns
log_to_simple_returns <- function(log_returns) {
  simple_returns <- exp(log_returns) - 1
  return(simple_returns)
}

simple_asset_returns <- log_to_simple_returns(asset_matrix)
simple_dj_returns <- log_to_simple_returns(log_index_data)

portfolio_returns <- simple_asset_returns %*% full_weights
portfolio_returns <- portfolio_returns * C * (1 - gamma)
portfolio_value <- 100 * cumprod(1 + portfolio_returns)
dj_value <- 100 * cumprod(1 + simple_dj_returns)

library(dplyr)
library(tidyr)
library(ggplot2)

# build data frame for plotting
df <- data.frame(
  t = 1:length(portfolio_value),
  Portfolio = as.vector(portfolio_value),
  DowJones = as.vector(dj_value)
)

# reshape to long format
library(tidyr)
df_long <- pivot_longer(df, cols = c("Portfolio", "X.DJI"),
                        names_to = "Series", values_to = "Value")

# plot
library(ggplot2)
ggplot(df_long, aes(x = t, y = Value, color = Series)) +
  geom_line(size = 1) +
  labs(title = "Portfolio vs Dow Jones",
       x = "Time (index)", y = "Cumulative Value") +
  theme_minimal()




