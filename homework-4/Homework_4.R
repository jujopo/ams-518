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
design_matrix <- design_matrix[, c(2, 3)]
design_matrix <- design_matrix %>% mutate(intercept = 1)
design_matrix <- as.matrix(design_matrix)
design_matrix <- design_matrix[, c(3, 1, 2)]

# Be aware that in PSG all the matrices we define have to start by matrix_...
matrix_s <- design_matrix

problem.list <- problem.list[-3]

# Reassign and correctly define the problem statement
problem.list$problem_name <- "Quantile-regression-85%"
problem.list$problem_statement[2] <- "kb_err(0.85, matrix_s)"

problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.85, matrix_s)"
problem.list$problem_statement[3] <- "value"
problem.list$problem_statement[4] <- "var_risk(0.85, matrix_s)"

# Reassign the design matrix
problem.list$matrix_s <- matrix_s

# Run PSG Solver to optimize problem stored in problem.list
results_0.85 <- rpsg_solver(problem.list)
