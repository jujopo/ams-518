library(PSG)
library(dplyr)
library(tidyr)


setwd("C:/Users/Jujop/Documents/GitHub/ams-518/homework-4/data")

# Set path to Rdata file including the RData file's name with CS:
load("C:/Users/Jujop/Documents/GitHub/ams-518/homework-4/data/Quantile_Regression_Problem_1_data_problemlist.RData")

# Load the data 
SP500IBM <- read.csv("C:/Users/Jujop/Documents/GitHub/ams-518/homework-4/data/sp500_ibm_close.csv")

# Reorder the columns to generate the design matrix (the dependent variable-IBM
# first and the independent variable-SP500-is last)

design_matrix <- SP500IBM[, c(1, 3, 2)]
design_matrix <- design_matrix[, c(1, 4, 2, 3)]

# Calculate simple daily returns
design_matrix$ibm_return <- c(NA, diff(design_matrix$IBM) / design_matrix$IBM[-nrow(design_matrix)])
design_matrix$sp500_return <- c(NA, diff(design_matrix$SP500) / design_matrix$SP500[-nrow(design_matrix)])

design_matrix <- design_matrix[,-c(2, 3)]
design_matrix <- na.omit(design_matrix)

# Convert Date column to proper date format
design_matrix$Date <- as.Date(design_matrix$Date)

# Compute monthly returns
data_xts <- xts(design_matrix[, c("ibm_return", "sp500_return")], order.by = design_matrix$Date)

# Calculate monthly returns
monthly_returns <- apply.monthly(data_xts, function(x) {apply(x, 2, function(column) prod(1 + column) - 1)})

# Convert back to data frame if needed
monthly_data <- data.frame(
  Date = index(monthly_returns),
  coredata(monthly_returns)
)

design_matrix <- monthly_data %>% mutate(intercept = 1)
design_matrix <- design_matrix[, c(1, 4, 2, 3)]
design_matrix <- design_matrix[, -1]

# Be aware that in PSG all the matrices we define have to start by matrix_...
design_matrix <- as.matrix(design_matrix)
matrix_s <- design_matrix
colnames(matrix_s)[2] <- "scenario_benchmark"

problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-50%"
problem.list$problem_statement[2] <- "kb_err(0.5, matrix_s)"


# Problem statement for cvar
problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.85, matrix_s)"
problem.list$problem_statement[3] <- "value"
problem.list$problem_statement[4] <- "var_risk(0.85, matrix_s)"

# Reassign the design matrix
problem.list$matrix_s <- matrix_s

# Run PSG Solver to optimize problem stored in problem.list
results_0.5 <- rpsg_solver(problem.list)
