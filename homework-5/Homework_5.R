library(PSG)
library(dplyr)
library(tidyr)
library(xts)


setwd("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-5/data")

# Set path to Rdata file including the RData file's name with CS:
load("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-5/data/problem_CVaR2_err_075.RData")

problem.list$problem_name

# Check what value is stored for alpha
problem.list$problem_statement

# Replace alpha value
problem.list$problem_statement[2] <- "cvar2_err(0.7,matrix_s)"

# Check again to see if correct value was stored properly
problem.list$problem_statement

# Run PSG Solver to optimize problem stored in problem.list
results_0.7 <- rpsg_solver(problem.list)

results_0.7$point_problem_1


# Quantile regression with confidence levels 0.7, 0.75, 0.85, 0.9 and 0.95

# alpha  = 0.7
problem.list$problem_statement[2] <- "kb_err(0.7, matrix_s)"
results_quantile_0.7 <- rpsg_solver(problem.list)

# alpha  = 0.75
problem.list$problem_statement[2] <- "kb_err(0.75, matrix_s)"
results_quantile_0.75 <- rpsg_solver(problem.list)

# alpha  = 0.85
problem.list$problem_statement[2] <- "kb_err(0.85, matrix_s)"
results_quantile_0.85 <- rpsg_solver(problem.list)

# alpha  = 0.9
problem.list$problem_statement[2] <- "kb_err(0.9, matrix_s)"
results_quantile_0.9 <- rpsg_solver(problem.list)

# alpha  = 0.95
problem.list$problem_statement[2] <- "kb_err(0.95, matrix_s)"
results_quantile_0.95 <- rpsg_solver(problem.list)

q_0.7 <- results_quantile_0.7$point_problem_1
q_0.75 <- results_quantile_0.75$point_problem_1
q_0.85 <- results_quantile_0.85$point_problem_1
q_0.9 <- results_quantile_0.9$point_problem_1
q_0.95 <- results_quantile_0.95$point_problem_1

sum <- q_0.7 + q_0.75 + q_0.85 + q_0.9 + q_0.95
average <- sum / 5

average <- average[-1]

cvar_0.7 <- results_0.7$point_problem_1
cvar_0.7 <- cvar_0.7[-1]

relative_error = abs(cvar_0.7 - average) / abs(cvar_0.7)
relative_error

save.image(file = "problem_CVaR2_err_075.RData")
