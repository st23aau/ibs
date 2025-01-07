# Load necessary libraries
library(tidyverse)

# Reading dataset
EmployeeData <- read_csv("file.csv", show_col_types = FALSE)

# Print the head of the dataset
head(EmployeeData)

# Print the tail of the dataset
tail(EmployeeData)

# Print column names of the dataset
colnames(EmployeeData)

# Check for missing values
colSums(is.na(EmployeeData))

# Print structure of the cleaned dataset
str(EmployeeData)

# Plotting the distribution of Age with trend bell
ggplot(EmployeeData, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "blue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Age Distribution with Trend Bell", x = "Age", y = "Density") +
  theme_minimal()

# Plotting the distribution of Monthly Income with trend bell
ggplot(EmployeeData, aes(x = MonthlyIncome)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1000, fill = "green", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Monthly Income Distribution with Trend Bell", x = "Monthly Income", y = "Density") +
  theme_minimal()

# Plotting Scatter plot between Age vs Monthly Income
ggplot(EmployeeData, aes(x = Age, y = MonthlyIncome)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Age vs Monthly Income", x = "Age", y = "Monthly Income") +
  theme_minimal()

# Performing statistical tests for Spearman's rank correlation
cor.test(EmployeeData$Age, EmployeeData$MonthlyIncome, method = "spearman")
