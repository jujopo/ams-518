library(PSG)

load('C:/Users/JUANJO/Documents/GitHub/ams-518/problem_cdohedge_1_R_data/problem_cdohedge_1.RData')

is.list(problem.list)

names(problem.list)

nrow(problem.list$matrix_scenarios)
ncol(problem.list$matrix_scenarios)

matrix_const_budget <- problem.list$matrix_constraint_budget
point_lowerbounds <- problem.list$point_lowerbounds
point_upperbounds <- problem.list$point_upperbounds


output.list$status
names(output.list)
point_problem_1 <- output.list$point_problem_1
hedging_points <- point_problem_1[point_problem_1 != 0]
hedging_points
length(hedging_points)

output_ns.list <- rpsg_solver(problem.list)

point_problem_1_ns <- output_ns.list$point_problem_1
hedging_points_ns <- point_problem_1_ns[point_problem_1_ns != 0]
hedging_points_ns
length(hedging_points_ns)

output_ns.list$output[7]
output_ns.list$output[10]

save(output.list, output_ns.list, problem.list, file = 'C:/Users/JUANJO/Documents/GitHub/ams-518/problem_cdohedge_1_R_data/problem_cdohedge_1.RData')
