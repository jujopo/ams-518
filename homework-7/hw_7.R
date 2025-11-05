# Load the libraries we need to use
library(PSG)
library(dplyr)
library(tidyr)
library(xts)
library(ggplot2)

setwd("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-7/data")

# Set path to Rdata file including the RData file's name with CS:
load("C:/Users/JUANJO/Documents/GitHub/ams-518/homework-7/data/problem_st_dev_risk.RData")

# Check what is the current problem statement
problem.list$problem_statement

# Modify the problem statement
problem.list$problem_statement <- ""
problem.list$problem_statement[1] <- "minimize"
problem.list$problem_statement[2] <- "cvar_dev(0.95,matrix_fact_in)"
problem.list$problem_statement[3] <- "Value:"
problem.list$problem_statement[4] <- "cvar_dev(0.95,matrix_fact_out)"

# Save the problem statement for future iterations
problem_statement <- problem.list$problem_statement

# Algorithm starts here

# Extract the original 3-factor matrix (e.g., 1000 rows, 4 cols)
matrix_scenarios_3fact <- problem.list$matrix_scenarios

# Last name: Perez, hence I should pick variables x2 and x3
matrix_scenarios_2fact <- matrix_scenarios_3fact[, c(2, 3, 4)]

# Set up our simulation parameters

# Create a sequence of training sizes of 5, 10, 15, 20, 40, 60, ..., 900
training_sizes <- c(5, 10, 15, seq(20, 900, by = 20))

# Number of times we are going to run each simulation (minimum = 20)
n_repetitions <- 25

# Total number of rows
n_total_rows <- nrow(matrix_scenarios_3fact)

# Testing set will always contain 100 rows
n_test <- 100

all_indices <- 1:n_total_rows

# Initialize the results storage
# This data frame will store the final average for each training size
results_df <- data.frame(
  TrainingSize = integer(),
  # 2-Factor CVaR In-Sample
  Avg_InSample_2F = double(), # CVaR in sample
  
  # 2-Factor CVaR Out-of-Sample
  Avg_OutOfSample_2F = double(), # CVaR out of sample
  
  # 3-Factor CVaR In-Sample
  Avg_InSample_3F = double(), # CVaR in sample
  
  # 3-Factor Out-of-Sample
  Avg_OutOfSample_3F= double() # CVaR out of sample
)

# Initialize the problem lists
problem.list.in.2f <- problem.list # Use the existing problem.list
problem.list.in.3f <- problem.list # Use the existing problem.list

# Outer loop: to iterate over each training size
for (n_train in training_sizes) {
  
  print(paste("Starting Training Size:", n_train))
  
  # Temporary matrices to store the average vectors from each rep
  # Matrix: (n_repetitions x 2 factors)
  in_sample_2f_reps <- matrix(NA, nrow = n_repetitions, ncol = 1)
  # Matrix: (n_repetitions x 3 factors)
  in_sample_3f_reps <- matrix(NA, nrow = n_repetitions, ncol = 1)
  
  # Temporary vectors to store results for the 30 repetitions
  out_sample_2f_reps <- matrix(NA, nrow = n_repetitions, ncol = 1)
  out_sample_3f_reps <- matrix(NA, nrow = n_repetitions, ncol = 1)
  
  # Repeat the experiment 30 times
  for (i in 1:n_repetitions) {
    
    # Step I: Randomly select 100 rows for testing set
    test_indices <- sample(all_indices, n_test)
    remaining_indices <- setdiff(all_indices, test_indices) # 900 remaining
    
    # Step II: Randomly select n_train rows from remaining
    train_indices <- sample(remaining_indices, n_train)
    
    # Create the training and testing matrices for this iteration
    matrix_train_set_3f <- matrix_scenarios_3fact[train_indices, ]
    matrix_test_set_3f  <- matrix_scenarios_3fact[test_indices, ]
    
    matrix_train_set_2f <- matrix_scenarios_2fact[train_indices, ]
    matrix_test_set_2f  <- matrix_scenarios_2fact[test_indices, ]
    
    #Step III and IV: Fit models and get In-Sample and Out of sample CVaR Deviation
    
    # 2-Factor In-Sample
    
    # Modify the first entry of the statement
    problem.list.in.2f$matrix_fact_in <- matrix_train_set_2f
    problem.list.in.2f$matrix_fact_out <- matrix_test_set_2f
    
    # Generate output for 2 factor model
    output_2f <- rpsg_solver(problem.list.in.2f)
    
    # Extract the CVaR value 
    cvar_in_2f <- output_2f$output[5]
    cvar_in_2f <- strsplit(cvar_in_2f, "=")[[1]]
    cvar_in_2f <- as.numeric(trimws(cvar_in_2f[2]))
    
    cvar_out_2f <- output_2f$output[6]
    cvar_out_2f <- strsplit(cvar_out_2f, "=")[[1]]
    cvar_out_2f <- as.numeric(trimws(cvar_out_2f[2]))
    
    # Fill in the averaged values
    in_sample_2f_reps[i,] <- cvar_in_2f
    out_sample_2f_reps[i, ] <- cvar_out_2f
    
    # 3-Factor In-Sample
    
    # Modify the first entry of the statement
    problem.list.in.3f$matrix_fact_in <- matrix_train_set_3f
    problem.list.in.3f$matrix_fact_out <- matrix_test_set_3f
    
    output_3f <- rpsg_solver(problem.list.in.3f)
    
    # Extract the CVaR value 
    cvar_in_3f <- output_3f$output[5]
    cvar_in_3f <- strsplit(cvar_in_3f, "=")[[1]]
    cvar_in_3f <- as.numeric(trimws(cvar_in_3f[2]))
    
    cvar_out_3f <- output_3f$output[6]
    cvar_out_3f <- strsplit(cvar_out_3f, "=")[[1]]
    cvar_out_3f <- as.numeric(trimws(cvar_out_3f[2]))
    
    # Fill in the averaged values
    in_sample_3f_reps[i,] <- cvar_in_3f
    out_sample_3f_reps[i, ] <- cvar_out_3f
    
  } # End of middle loop (n_repetitions)
  
  # Step V: Calculate averages
  avg_in_2f  <- colMeans(in_sample_2f_reps, na.rm = TRUE)
  avg_in_3f  <- colMeans(in_sample_3f_reps, na.rm = TRUE)
  
  avg_out_2f  <- colMeans(out_sample_2f_reps, na.rm = TRUE)
  avg_out_3f  <- colMeans(out_sample_3f_reps, na.rm = TRUE)
  
  # Store the averages in the main results data frame
  results_df <- rbind(results_df, data.frame(
    TrainingSize = n_train,
    Avg_InSample_2F = avg_in_2f[1], # Corresponds to CVaR in Sample 2F
    Avg_OutOfSample_2F = avg_out_2f[1], # Corresponds to CVaR our of Sample 2F
    
    Avg_InSample_3F = avg_in_3f[1], # Corresponds to CVaR in Sample 3F
    Avg_OutOfSample_3F = avg_out_3f[1] # Corresponds to CVaR out of Sample 3F
  ))
} # End of outer loop (training_sizes)

# Final Results
print("Simulation Complete. Final averaged results:")
print(results_df)

# Reshape data for ggplot2
# "Melt" the data frame from wide to long format for ggplot
# This creates a single column for the 'Metric' and 'CVaR_Value'

results_long <- results_df %>%
  pivot_longer(
    cols = c("Avg_InSample_2F", "Avg_OutOfSample_2F", "Avg_InSample_3F", "Avg_OutOfSample_3F"),
    names_to = "Metric",
    values_to = "CVaR_Deviation"
  ) %>%
  # Create helper columns for Model and Sample Type based on the Metric name
  mutate(
    Model = ifelse(grepl("2F", Metric), "2-Factor (Less Flexible)", "3-Factor (More Flexible)"),
    SampleType = ifelse(grepl("InSample", Metric), "In-Sample", "Out-of-Sample")
  )

# Graph 1: "Figure 4" Reproduction

print("Generating Graph 1: 'Figure 4' Reproduction Plot...")

# Create the plot with 4 lines
# (IS-2F, OOS-2F, IS-3F, OOS-3F)
figure4_plot <- ggplot(results_long, 
                       aes(x = TrainingSize, 
                           y = CVaR_Deviation, 
                           color = Model,        # Color by Model
                           linetype = SampleType)) +  # Dash/Solid by Sample Type
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Model Performance vs. Training Set Size",
    subtitle = "Comparing In-Sample (dashed) and Out-of-Sample (solid) CVaR Deviations",
    x = "Number of Rows in Training Set (m)",
    y = "Average 95% CVaR Deviation (Error)",
    color = "Model",
    linetype = "Sample Type"
  ) +
  scale_linetype_manual(values = c("In-Sample" = "dashed", "Out-of-Sample" = "solid")) +
  theme_minimal()

# Display the plot
print(figure4_plot)

# --- TO SAVE THIS PLOT ---
# Use ggsave() to save the *last printed plot*
# You can change the dpi (dots per inch) for higher resolution.
ggsave(
  "figure4_reproduction.png", 
  plot = figure4_plot, 
  width = 12, 
  height = 8, 
  dpi = 300
)
print("Saved 'figure4_reproduction.png'")


# 4. Graph 2: Out-of-Sample "Advantage" Plot 

print("Generating Graph 2: 'Advantage' Plot...")

# This plot shows the *difference* in OOS error.
# We add a new column 'Error_Advantage_3F' to the original results_df
advantage_df <- results_df %>%
  mutate(
    Error_Advantage_3F = Avg_OutOfSample_2F - Avg_OutOfSample_3F
  )

# Create the single-line plot
advantage_plot <- ggplot(advantage_df, 
                         aes(x = TrainingSize, y = Error_Advantage_3F)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  # Add the critical y=0 line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Out-of-Sample Advantage: 3-Factor vs. 2-Factor Model",
    x = "Number of Rows in Training Set (m)",
    y = "OOS Error (2-Factor) - OOS Error (3-Factor)"
  ) +
  # Add text annotations
  annotate("text", x = 700, y = max(advantage_df$Error_Advantage_3F) * 0.8, 
           label = "3-Factor Model is better\n(Error is lower)", 
           color = "darkgreen", hjust = 0.5) +
  annotate("text", x = 200, y = min(advantage_df$Error_Advantage_3F) * 0.8, 
           label = "2-Factor Model is better\n(Error is lower)", 
           color = "red", hjust = 0.5) +
  theme_minimal()

# Display the plot
print(advantage_plot)

# TO SAVE THIS PLOT
ggsave(
  "advantage_plot.png", 
  plot = advantage_plot, 
  width = 12, 
  height = 8, 
  dpi = 300
)
print("Saved 'advantage_plot.png'")


# 4. Find the Break-Even Point

print("Calculating Break-Even Point...")

# Find all rows where the 3-factor OOS error is better (lower)
break_even_rows <- results_df %>%
  filter(Avg_OutOfSample_3F < Avg_OutOfSample_2F)

if (nrow(break_even_rows) > 0) {
  # Find the *smallest* training size where this happens
  break_even_point <- min(break_even_rows$TrainingSize)
  
  print(paste("BREAK-EVEN POINT ANALYSIS:"))
  print(paste("The break-even point occurs at TrainingSize:", break_even_point))
  print("This is the first point where the 3-factor model's average OOS error")
  print("became lower than the 2-factor model's.")
  
} else {
  print("BREAK-EVEN POINT ANALYSIS:")
  print("The 3-factor model never outperformed the 2-factor model")
  print("in the tested training size range.")
}

save.image(file = "problem_st_dev_risk.RData")
