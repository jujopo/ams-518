library(PSG)
library(dplyr)
library(tidyr)
library(xts)


setwd("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-4/data")

# Set path to Rdata file including the RData file's name with CS:
load("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-4/data/Quantile_Regression_Problem_1_data_problemlist.RData")

# Load the data 
SP500IBM <- read.csv("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-4/data/sp500_ibm_close.csv")

# Reorder the columns to generate the design matrix (the dependent variable-IBM
# first and the independent variable-SP500-is last)

design_matrix <- SP500IBM[, c(1, 3, 2)]

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

# Generate two matrices: one containing monthly returns and the other daily returns
matrix_s_daily <- design_matrix %>% mutate(intercept = 1)
matrix_s_monthly <- monthly_data %>% mutate(intercept = 1)

matrix_s_daily <- matrix_s_daily[, c(1, 4, 2, 3)]
matrix_s_daily <- matrix_s_daily[, -1]

matrix_s_monthly <- matrix_s_monthly[, c(1, 4, 2, 3)]
matrix_s_monthly <- matrix_s_monthly[, -1]

# Be aware that in PSG all the matrices we define have to start by matrix_...
matrix_s_monthly <- as.matrix(matrix_s_monthly)
matrix_s_daily <- as.matrix(matrix_s_daily)


colnames(matrix_s_daily)[2] <- "scenario_benchmark"
colnames(matrix_s_monthly)[2] <- "scenario_benchmark"

problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-50%"
problem.list$problem_statement[2] <- "kb_err(0.5, matrix_s_daily)"


# Problem statement for cvar
problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.85, matrix_s)"
problem.list$problem_statement[3] <- "value"
problem.list$problem_statement[4] <- "var_risk(0.85, matrix_s)"

# Reassign the design matrix
problem.list$matrix_s_daily <- matrix_s_daily

# Run PSG Solver to optimize problem stored in problem.list
results_0.5_daily <- rpsg_solver(problem.list)

# For 85% quantile we have that
problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-85%"
problem.list$problem_statement[2] <- "kb_err(0.85, matrix_s_daily)"

# Reassign the design matrix
problem.list$matrix_s_daily <- matrix_s_daily

# Run PSG Solver to optimize problem stored in problem.list
results_0.85_daily <- rpsg_solver(problem.list)

# Store regression coefficients
intercept_0.5_daily <- results_0.5_daily$point_problem_1[1]
slope_0.5_daily <- results_0.5_daily$point_problem_1[2]

intercept_0.85_daily <- results_0.85_daily$point_problem_1[1]
slope_0.85_daily <- results_0.85_daily$point_problem_1[2]

# Run again for the monthly returns
problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-50%"
problem.list$problem_statement[2] <- "kb_err(0.5, matrix_s_monthly)"

# Reassign the design matrix
problem.list$matrix_s_monthly <- matrix_s_monthly

# Run PSG Solver to optimize problem stored in problem.list
results_0.5_monthly <- rpsg_solver(problem.list)

# For 85% quantile we have that
problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-85%"
problem.list$problem_statement[2] <- "kb_err(0.85, matrix_s_monthly)"

# Reassign the design matrix
problem.list$matrix_s_monthly <- matrix_s_monthly

# Run PSG Solver to optimize problem stored in problem.list
results_0.85_monthly <- rpsg_solver(problem.list)

# Store regression coefficients
intercept_0.5_monthly <- results_0.5_monthly$point_problem_1[1]
slope_0.5_monthly <- results_0.5_monthly$point_problem_1[2]

intercept_0.85_monthly <- results_0.85_monthly$point_problem_1[1]
slope_0.85_monthly <- results_0.85_monthly$point_problem_1[2]

# Extract the returns
sp500_returns <- matrix_s_daily[, "sp500_return"]
ibm_daily_returns <- matrix_s_daily[, "scenario_benchmark"]

sp500_monthly_returns <- matrix_s_monthly[, "sp500_return"]
ibm_monthly_returns <- matrix_s_monthly[, "scenario_benchmark"]

# Set up PNG device
png("quantile_regression_daily_plot.png", width = 10, height = 7, units = "in", res = 300)

# Create the scatter plot
plot(sp500_daily_returns, ibm_daily_returns, 
     xlab = "SP500 Daily Return", 
     ylab = "IBM Daily Return",
     main = "Quantile Regression: IBM vs SP500 Returns (Daily)",
     pch = 16, col = rgb(0, 0, 1, 0.2), cex = 0.7)

# Add grid for better readability
grid()

# Create x-values for the regression lines
x_range <- seq(min(sp500_daily_returns), max(sp500_daily_returns), length.out = 100)

# Calculate y-values for both quantile lines
y_50 <- intercept_0.5_daily + slope_0.5_daily * x_range
y_85 <- intercept_0.85_daily + slope_0.85_daily * x_range

# Add the quantile regression lines
lines(x_range, y_50, col = "#E69F00", lwd = 2, lty = 1)
lines(x_range, y_85, col = "#9E1C60", lwd = 2, lty = 1)

# Add legend
legend("topleft", 
       legend = c("Data points", "50% Quantile", "85% Quantile"),
       col = c(rgb(0, 0, 1, 0.5), "#E69F00", "#9E1C60"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2),
       bty = "n")

# Close the device
dev.off()

  # Set up PNG device
png("quantile_regression_monthly_plot.png", width = 10, height = 7, units = "in", res = 300)

# Create the scatter plot
plot(sp500_monthly_returns, ibm_monthly_returns, 
     xlab = "SP500 Monthly Return", 
     ylab = "IBM MOnthly Return",
     main = "Quantile Regression: IBM vs SP500 Returns (Monthly)",
     pch = 16, col = rgb(1, 0, 0, 0.3), cex = 0.7)

# Add grid for better readability
grid()

# Create x-values for the regression lines
x_range <- seq(min(sp500_monthly_returns), max(sp500_monthly_returns), length.out = 100)

# Calculate y-values for both quantile lines
y_50 <- intercept_0.5_monthly + slope_0.5_monthly * x_range
y_85 <- intercept_0.85_monthly + slope_0.85_monthly * x_range

# Add the quantile regression lines
lines(x_range, y_50, col = "#E69F00", lwd = 2, lty = 1)
lines(x_range, y_85, col = "#9E1C60", lwd = 2, lty = 1)

# Add legend
legend("topleft", 
       legend = c("Data points", "50% Quantile", "85% Quantile"),
       col = c(rgb(1, 0, 0, 0.5), "#E69F00", "#9E1C60"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2),
       bty = "n")

# Close the device
dev.off()

# Compute the design matrices without intercept
matrix_s_daily_wi <- matrix_s_daily[, -1]
matrix_s_monthly_wi <- matrix_s_monthly[, -1]

# For 85% quantile daily we have that
problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "CVaR-regression-85%"

# Problem statement for cvar
problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.85, matrix_s_daily_wi)"
problem.list$problem_statement[3] <- "value"
problem.list$problem_statement[4] <- "var_risk(0.85, matrix_s_daily_wi)"

# Reassign the design matrix
problem.list$matrix_s_daily_wi <- matrix_s_daily_wi

results_0.85_daily_cvar <- rpsg_solver(problem.list)

slope_0.85_daily_cvar <- results_0.85_daily_cvar$point_problem_1
slope_0.85_daily_cvar <- as.numeric(slope_0.85_daily_cvar)
slope_0.85_daily <- as.numeric(slope_0.85_daily)

diff = abs(slope_0.85_daily_cvar - slope_0.85_daily)

var_risk_0.85 = 1.069609789460E-02
intercept_0.85_daily <- as.numeric(intercept_0.85_daily)

diff_intercept <- abs(var_risk_0.85 - intercept_0.85_daily)
diff_intercept

# For 85% quantile monthly we have that
problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "CVaR-regression-85%"

# Problem statement for cvar
problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.85, matrix_s_monthly_wi)"
problem.list$problem_statement[3] <- "value"
problem.list$problem_statement[4] <- "var_risk(0.85, matrix_s_monthly_wi)"

# Reassign the design matrix
problem.list$matrix_s_monthly_wi <- matrix_s_monthly_wi

results_0.85_monthly_cvar <- rpsg_solver(problem.list)

slope_0.85_monthly_cvar <- results_0.85_monthly_cvar$point_problem_1
slope_0.85_monthly_cvar <- as.numeric(slope_0.85_monthly_cvar)
slope_0.85_monthly <- as.numeric(slope_0.85_monthly)

diff = abs(slope_0.85_monthly_cvar - slope_0.85_monthly)
diff

results_0.85_monthly_cvar$output[6]

var_risk_0.85_monthly = 8.443137393789E-02
intercept_0.85_monthly <- as.numeric(intercept_0.85_monthly)

diff_intercept <- abs(var_risk_0.85_monthly - intercept_0.85_monthly)
diff_intercept

save.image(file = "Quantile_Regression_Problem.RData")
