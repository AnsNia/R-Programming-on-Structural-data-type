################################################### DATA TYPE STRUCTURAL ############################################################


## IMPORT DATA  ######

employee_data <- read.csv("C:/Users/Anshum/Desktop/Learning curve/R/plans/structure/company_employee.csv")

######  DATA EXPLORATION, PREPROCESSING, DESCRIPTIVE STATISTICS AND DATA OVERVIEW #######################################

######  structure of data #
str(employee_data)

######  summary statistics #
summary(employee_data)

## first few rows of data #
head(employee_data)

## check for missing values #

any_missing <- any(is.na(employee_data))
if (any_missing){
    cat("There are missing values in the dataset.\n")
}

####### Changing the class of some columns from character to factor
employee_data$company <- as.factor(employee_data$company)
employee_data$department <- as.factor(employee_data$department)
employee_data$employee_id <- as.factor(employee_data$employee_id)
employee_data$age <- as.factor(employee_data$age)
employee_data$age_when_joined <- as.factor(employee_data$age_when_joined)
employee_data$years_in_the_company <- as.factor(employee_data$years_in_the_company)
employee_data$prior_years_experience <- as.factor(employee_data$prior_years_experience)

#### checking the structure again ##
str(employee_data)

#### Explore unique values in factor columns
unique_companies <- levels(employee_data$company)
unique_departments <- levels(employee_data$department)
unique_employee_ids <- levels(employee_data$employee_id)
unique_age_levels <- levels(employee_data$age)
unique_age_when_joined_levels <- levels(employee_data$age_when_joined)
unique_years_in_the_company_levels <- levels(employee_data$years_in_the_company)
unique_prior_years_experience <- levels(employee_data$prior_years_experience)

cat("Unique Companies:", unique_companies, "\n")
cat("Unique Departments:", unique_departments, "\n")
cat("Unique Employee IDs:", unique_employee_ids, "\n")
cat("Unique Age Levels:", unique_age_levels, "\n")
cat("Unique Age When Joined Levels:", unique_age_when_joined_levels, "\n")
cat("Unique Years in the Company Levels:", unique_years_in_the_company_levels, "\n")
cat("Unique Prior Years Experience Levels:", unique_prior_years_experience, "\n")


summary(employee_data)


###### Descriptive statistics ##
summary(employee_data$age)
summary(employee_data$salary)
summary(employee_data$annual_bonus)




#################################### DEMOGRAPHIC ANALYSIS ###################################################

# Summary statistics of age
summary(employee_data$age)

##distribution of age##
# Histogram of ages
hist(employee_data$age, main = "Distribution of Ages", xlab = "Age")

# Age distribution by department
library(ggplot2) 
ggplot(employee_data, aes(x = age, fill = department)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Age Distribution by Department", x = "Age", y = "Count")

# Calculate average age by department
avg_age_by_department <- tapply(employee_data$age, employee_data$department, mean)
print(avg_age_by_department)



######################################  EMPLOYEE TENURE AND ATTRITION ############################################

# Calculate employee tenure
employee_data$employee_tenure <- employee_data$years_in_the_company + employee_data$age - employee_data$age_when_joined

# Print the data frame with tenure
cat("\nEmployee Data with Tenure:\n")
print(employee_data[c('company', 'department', 'employee_id', 'age', 'years_in_the_company', 'employee_tenure')])

# Tenure analysis plot
hist(employee_data$employee_tenure, 
     breaks = seq(floor(min(employee_data$employee_tenure)), ceiling(max(employee_data$employee_tenure)), by = 1),
     col = 'skyblue', main = 'Employee Tenure Distribution', xlab = 'Employee Tenure (Years)', ylab = 'Number of Employees')


# Identify employees who left (attrition)
left_employees <- employee_data[employee_data$years_in_the_company == 1, ]

# Attrition analysis plot
barplot(table(left_employees$department), col = 'skyblue', main = 'Attrition by Department', xlab = 'Department', ylab = 'Number of Employees')

# Print average tenure by department
avg_tenure_by_department <- tapply(employee_data$employee_tenure, employee_data$department, mean)
cat("\nAverage Employee Tenure by Department:\n")
print(avg_tenure_by_department)


########################################## SALARY COMPENSATION ANALYSIS ###########################################


# Summary statistics of salary
summary(employee_data$salary)

# Boxplot of salary by department
boxplot(salary ~ department, data = employee_data, col = 'skyblue', main = 'Salary Distribution by Department', 
        xlab = 'Department', ylab = 'Salary')

# Scatter plot of salary and age
plot(employee_data$age, employee_data$salary, col = 'blue', main = 'Scatter Plot of Salary and Age', 
     xlab = 'Age', ylab = 'Salary')

# Correlation between salary and prior years of experience
cor(employee_data$salary, as.numeric(employee_data$prior_years_experience))

# Regression analysis of salary based on prior years of experience
lm_model <- lm(salary ~ as.numeric(prior_years_experience), data = employee_data)
summary(lm_model)

# Plotting the regression line
plot(employee_data$prior_years_experience, employee_data$salary, col = 'blue', main = 'Salary vs. Prior Years of Experience',
     xlab = 'Prior Years of Experience', ylab = 'Salary')
abline(lm_model, col = 'red')

# Histogram of annual bonus
hist(employee_data$annual_bonus, col = 'skyblue', main = 'Distribution of Annual Bonus', 
     xlab = 'Annual Bonus', ylab = 'Number of Employees')

# Correlation between salary and annual bonus
cor(employee_data$salary, employee_data$annual_bonus)


################################### COMPARATIVE ANALYSIS BETWEEN COMPANIES #############################################

# Subset data for each company
glasses_data <- subset(employee_data, company == "Glasses")
cheerper_data <- subset(employee_data, company == "Cheerper")

# Compare average salary between companies
avg_salary_glasses <- mean(glasses_data$salary)
avg_salary_cheerper <- mean(cheerper_data$salary)

cat("Average Salary in Glasses:", avg_salary_glasses, "\n")
cat("Average Salary in Cheerper:", avg_salary_cheerper, "\n")

# Compare age distribution between companies
hist(glasses_data$age, col = 'skyblue', main = 'Age Distribution in Glasses', xlab = 'Age', ylab = 'Number of Employees')
hist(cheerper_data$age, col = 'orange', main = 'Age Distribution in Cheerper', xlab = 'Age', ylab = 'Number of Employees')

# Compare years of experience distribution between companies
hist(glasses_data$prior_years_experience, col = 'skyblue', main = 'Years of Experience in Glasses', 
     xlab = 'Years of Experience', ylab = 'Number of Employees')
hist(cheerper_data$prior_years_experience, col = 'orange', main = 'Years of Experience in Cheerper', 
     xlab = 'Years of Experience', ylab = 'Number of Employees')


# Compare distribution of full-time, part-time, and contractor employees between companies
full_time_data <- table(employee_data$company, employee_data$full_time)
part_time_data <- table(employee_data$company, employee_data$part_time)
contractor_data <- table(employee_data$company, employee_data$contractor)

# Combine the data into a single data frame
combined_data <- data.frame(
  Company = rownames(full_time_data),
  FullTime = full_time_data[, 2],
  PartTime = part_time_data[, 2],
  Contractor = contractor_data[, 2]
)

# Reshape the data for better plotting
combined_data_long <- reshape2::melt(combined_data, id.vars = "Company")

# Create a grouped bar plot
barplot(
  matrix(c(combined_data$FullTime, combined_data$PartTime, combined_data$Contractor), ncol = 3, byrow = TRUE),
  beside = TRUE,
  col = c('skyblue', 'lightblue', 'darkblue'),
  legend.text = c('Full Time', 'Part Time', 'Contractor'),
  names.arg = combined_data$Company,
  main = 'Employment Type Distribution by Company',
  xlab = 'Company',
  ylab = 'Number of Employees'
)


############################# MORE POSSIBLE DATA VISUALIZATIONS #################################################

# Boxplot of salary by department
boxplot(salary ~ department, data = employee_data, col = 'skyblue', main = 'Salary Distribution by Department', 
        xlab = 'Department', ylab = 'Salary')

# Histogram of annual bonus
hist(employee_data$annual_bonus, col = 'skyblue', main = 'Distribution of Annual Bonus', 
     xlab = 'Annual Bonus', ylab = 'Number of Employees')

# Correlation heatmap
cor_matrix <- cor(employee_data[, c('age', 'years_in_the_company', 'salary', 'annual_bonus')])
library(corrplot)
corrplot(cor_matrix, method = 'color', type = 'upper', order = 'hclust', tl.col = 'black')





