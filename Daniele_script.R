# Setting the correct working directory (Location where our data & R files are stored)
# We set it via relative paths such that we can work on the same code without 
# the need to change it all the time

# Clear current working environment
rm(list=ls())
# Get the directory of the current script
script_dir <- getwd()
general_data <- read.csv("./data/general_data.csv")
employee_survey_data <- read.csv("./data/employee_survey_data.csv")

################## LIBRARIES ##################
library(corrplot) # For plotting correlation matrix
library(MASS) # For stepAIC, glm, lda, qda
library(pROC) # For ROC curve
#library(ISLR2) For dataset so useless
library(car) # For vif
library(faraway) # For ANCOVA
library(e1071)
library(class)
library(boot)
library(gRbase)
library(leaps)
library(glmnet)
library(igraph)

################## FUNCTIONS ################## 

# REMOVE PLOTS
shutoff_plots <- function() {
  # Reset graphic windows
  if (!is.null(dev.list())) {
    dev.off()
  }
}


# MULTIPLE PLOTS 
setup_multiple_plots <- function(dataframe) {
  # Get number of columns
  num_of_columns <- length(names(dataframe))
  # Set up sizes
  par(mar=c(2,2,2,2))
  # Set up the graphics window to have a suitable number of rows and columns
  par(mfrow = c(ceiling(sqrt(num_of_columns)), ceiling(sqrt(num_of_columns))))
  
}

# FOR PAIRS PLOTS (from Lecture)
## panel.hist function
## puts histograms on the diagonal

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## panel.cor function
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

one_hot_encoding <- function(dataframe, col_name) {
  unique_values <- unique(dataframe[[col_name]])
  for (val in unique_values) {
    dataframe[paste(col_name, val, sep = " ")] <- ifelse(dataframe[[col_name]] == val, 1, 0)
  }
  dataframe[[col_name]] <- NULL
  return(dataframe)
}


######################################### STEP 2 DATA CLEANING & FILTERING #########################################

##### OVERVIEW OF DATA ######

str(general_data)
str(employee_survey_data)

data <- merge(general_data, employee_survey_data, by = "EmployeeID")

##### HANDLE MISSING DATA ######

# Check for missing values
missing_values <- sum(is.na(data))
missing_values

# Drop missing values
data <- na.omit(data)

##### HANDLE DUPLICATES ######

# Check how many duplicates there are in the dataset
duplicates <- sum(duplicated(data))
duplicates

##### DATA PREPROCESSING ######

str(data)
summary(data)

# Count the number of unique values in each column
unique_values <- sapply(data, function(x) length(unique(x)))
unique_values

##### REMOVE UNNECESSARY COLUMNS ######

# Remove unnecessary columns, e.g. EmployeeID, Over18, StandardHours, EmployeeCount
#Couldn't over 18 be a good one ? we should maybe check how many are over 18, how many under
data <- data[, !(names(data) %in% c("EmployeeID", "Over18", "StandardHours", "EmployeeCount"))]

# Check if columns have been removed
str(data)

##### CONVERT CATEGORICAL VARIABLES TO FACTORS ######

# Convert categorical variables to factors
data$Attrition <- as.factor(data$Attrition)
data$Gender <- as.factor(data$Gender)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$JobRole <- as.factor(data$JobRole)
data$Department <- as.factor(data$Department)
data$EducationField <- as.factor(data$EducationField)
data$MaritalStatus <- as.factor(data$MaritalStatus)

# One-hot encoding of categorical variables
#data <- one_hot_encoding(data, "JobRole")
#data <- one_hot_encoding(data, "Department")
#data <- one_hot_encoding(data, "EducationField")
#data <- one_hot_encoding(data, "MaritalStatus")

# See MonthlyIncome distribution
hist(data$MonthlyIncome)

# Log-Scale MonthlyIncome
data$MonthlyIncome <- log(data$MonthlyIncome)

# Now see the histogram
hist(data$MonthlyIncome)


# Check preprocessed data
str(data)

######################################### STEP 3 EXPLORATORY DATA ANALYSIS #########################################

##### CORRELATION MATRIX ######

shutoff_plots()     

# Select only numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Compute correlation matrix
correlation_matrix <- cor(numeric_data)

# Plot correlation matrix bigger
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)

######################################### STEP 4 MODELING #########################################

#### Model 1: Simple Linear Regression

# Fit linear regression model for TotalWorkingYears using Age
model_slr <- lm(TotalWorkingYears ~ Age, data = data)
summary(model_slr)

# Plot the linear regression model
plot(data$Age, data$MonthlyIncome, xlab = "Age", ylab = "Total Working Years", main = "Total Working Years vs Age")
abline(model_slr, col = "red")

#### Model 2: Polynomial Regression

# Fit polynomial regression model for TotalWorkingYears using Age (Grade 2)
model_pr2 <- lm(TotalWorkingYears ~ poly(Age, 2, raw = TRUE), data = data)
summary(model_pr2)

# Fit polynomial regression model for TotalWorkingYears using Age (Grade 3)
model_pr3 <- lm(TotalWorkingYears ~ poly(Age, 3, raw = TRUE), data = data)
summary(model_pr3)

# Fit polynomial regression model for TotalWorkingYears using Age (Grade 5)
model_pr5 <- lm(TotalWorkingYears ~ poly(Age, 5, raw = TRUE), data = data)
summary(model_pr5)

# Fit polynomial regression model for TotalWorkingYears using Age (Grade 10)
model_pr10 <- lm(TotalWorkingYears ~ poly(Age, 10, raw = TRUE), data = data)
summary(model_pr10)

#### Model 3: Log-Linear Regression

# Fit log-linear regression model for TotalWorkingYears using Age
#model_llr <- lm(log(TotalWorkingYears) ~ Age, data = data)
#summary(model_llr)

#### Model 4: Multiple Linear Regression

# Fit linear regression model for TotalWorkingYears using all numeric variables
model_mlr <- lm(TotalWorkingYears ~ ., data = data)
summary(model_mlr)

#### Model 5: Ridge Regression

# Fit ridge regression model for TotalWorkingYears using all numeric variables
model_rr <- glmnet(as.matrix(data[, !(names(data) %in% c("TotalWorkingYears"))]), data$TotalWorkingYears, alpha = 0)
summary(model_rr)

#### Model 6: Lasso Regression

# Fit lasso regression model for TotalWorkingYears using all numeric variables
model_lr <- glmnet(as.matrix(data[, !(names(data) %in% c("TotalWorkingYears"))]), data$TotalWorkingYears, alpha = 1)
summary(model_lr)






# MODEL 1 FINAL 
# this is using no one-hot encoding, just all variables as categorical ones bc. the model autmatically
# selects categorical ones if needed

# Fit the multiple linear regression model
model <- lm(YearsAtCompany ~ ., data = data)

# Summarize the model
summary(model)

# First, we will check for multicollinearity
vif_values <- vif(model)
print(vif_values)

# We consider VIF values over 3 high. Thus, we want to remove them.
# We find the two problematic variables to be Department and EducationField

columns_to_remove <- c("Department", "EducationField")

data_reduced <- data[, !names(data) %in% columns_to_remove]

# Refit the model
model <- lm(YearsAtCompany ~ ., data = data_reduced)

# See that it worked 
vif_values <- vif(model)
print(vif_values)

# DIAGNOSTIC CHECKS

# Residual vs Fitted plot
plot(model, which = 1)
# We see a non-linear relationship ! Let us try to make it more clear by 
# doing a log-transform (I tried this and it works nicely, but does it make sense?)

# Apply a log transformation to the response variable
data_reduced$log_YearsAtCompany <- log(data_reduced$YearsAtCompany + 1)  # Add 1 to avoid log(0)

# Fit the model with the transformed response variable
model_log <- lm(log_YearsAtCompany ~ ., data = data_reduced)

# Summary of the transformed model
summary(model_log)

# Plot Residuals vs Fitted for the transformed model
plot(model_log, which = 1)

# Now we see the non-linear pattern way better !
# It look a lot like a polynomial regression, so we try this as a model now

# Create a function to generate polynomial terms
generate_poly_formula <- function(data, degree = 2) {
  predictors <- colnames(data)
  predictors <- predictors[!predictors %in% c("log_YearsAtCompany")]  # Exclude the response variable
  
  # Create polynomial terms for each predictor
  poly_terms <- sapply(predictors, function(x) paste0("poly(", x, ", ", degree, ", raw = TRUE)"))
  formula <- paste("log_YearsAtCompany ~", paste(poly_terms, collapse = " + "))
  
  return(as.formula(formula))
}

# Apply the function to create the polynomial formula
poly_formula <- generate_poly_formula(data_reduced, degree = 2)

# Print the polynomial formula
print(poly_formula)

# Fit the polynomial regression model
model_poly <- lm(poly_formula, data = data_reduced)

summary(model_poly)

# We get an R-squared score of 0.95 ! This is very good, but we have to be careful with overfitting


# Next, we do feature selection, since we still have many features and a lot of them are not significant

# Perform backward selection using AIC
backward_model <- step(model_poly, direction = "backward")

# Summary of the final model after backward selection
summary(backward_model)

# Plot Residuals vs Fitted for the transformed model
plot(backward_model, which = 1)


# Use Cook’s distance or leverage values to identify influential points.

# Calculate Cook's distance
cooksd <- cooks.distance(backward_model)

# Plot Cook's distance
plot(cooksd, pch = "*", cex = 2, main = "Cook's Distance")
abline(h = 4/(nrow(data_reduced) - length(backward_model$coefficients)), col = "red")  # Add a cutoff line

# Identify the points with high Cook's distance
influential <- which(cooksd > 4/(nrow(data_reduced) - length(model_poly$coefficients)))
print(influential)

# Optionally, remove influential points and refit the model
data_reduced_clean <- data_reduced[-influential, ]
backward_model_clean <- lm(poly_formula, data = data_reduced_clean)
summary(backward_model_clean)

# Check residual diagnostics for the new model
par(mfrow = c(2, 2))
plot(backward_model_clean)









# Let us check some more plots : 

# Normal Q-Q plot
plot(model, which = 2)

# Scale-Location plot
plot(model, which = 3)

# Residuals vs Leverage plot
plot(model, which = 5)













################################# OLD STUFF #################################
###### MODEL 1 : SIMPLE LINEAR REGRESSION ######
# Linear regression for years at company 
# We first start off with a multiple linear regression of years at company to all other variables


# Fit the multiple linear regression model
model <- lm(YearsAtCompany ~ ., data = data)

# Summarize the model
summary(model)

# The first thing we notice it that we have 4 coefficients that are not defined due to singularities
# This means that certain predictors are perfectly correlated with each other
# We can identify the ones by the NA values
# They are :
#	JobRole Manager
#	Department Human Resources
#	EducationField Human Resources
# MaritalStatus Divorced

# Therefore, we remove the problematic predictors
data_fix <- subset(data, select = -c(`JobRole Manager`, `Department Human Resources`, `EducationField Human Resources`, `MaritalStatus Divorced`))

# Then refit the model
model <- lm(YearsAtCompany ~ ., data = data_fix)

# Now we want to check for multicollinearity 
vif_values <- vif(model)
print(vif_values)



##### way to calculate the upper 4, but we also see them directly and I dont know if we did alias() in class #####
# Compute correlation matrix for numeric predictors
numeric_predictors <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_predictors)
print(cor_matrix)
# Check for perfect collinearity among factor variables
alias(model)
##### way to calculate the upper 4, but we also see them directly and I dont know if we did alias() in class #####



# Let us check and see if we have multicollinearity

# Compute VIF
vif_values <- vif(model)

# We see, as also expected, that we have high VIF values (so high collinearity) especially for our one-hot encoded variables
# Thus, we check in our multiple linear regression model how important they are (i.e. the p-value) ; depending on that
# we keep them or remove them

# We find the categorical variables
# JobRole Healthcare Representative (2 stars)
# JobRole Research Scientist (3 stars)
# JobRole Sales Executive (3 stars)
# JobRole Laboratory Technician (3 stars)
# JobRole Sales Representatitve (2 stars)

# to be significant. These we want to keep. To this effect, let us see how high their
# VIF values were

# JobRole Healthcare Representative (2.109646)
#	JobRole Research Scientist (3.190587)
#	JobRole Sales Executive (3.375146)
#	JobRole Human Resources (1.485364)
#	JobRole Research Director (1.709357)
#	JobRole Laboratory Technician (2.991093)
#	JobRole Manufacturing Director (2.222043)
#	JobRole Sales Representative (1.732274)
# We can see that all JobRoles were at highest around 3, which is fine. 


# Let us now remove the probablematic categorical variables, that is
#•	Department Sales (9.883511)
#•	Department Research & Development (9.778514)
#•	EducationField Life Sciences (23.672286)
#•	EducationField Other (5.895711)
#•	EducationField Medical (21.368588)
#•	EducationField Marketing (10.784193)
#•	EducationField Technical Degree (8.675798)

# These are just the complete 2 variables Department and EducationField, so we remove them


# used for the one hot encoding approach
# We will remove Deparment and Education Field
#columns_to_remove <- c("Department Sales", 
#                       "Department Research & Development", 
#                       "EducationField Life Sciences", 
#                       "EducationField Other", 
#                       "EducationField Medical", 
#                       "EducationField Marketing", 
##                       "EducationField Technical Degree")


columns_to_remove <- c("Department", "EducationField")

# Subset the dataframe to exclude the specified columns
# data_fix if we do via one hot encoding
#data_reduced <- data_fix[, !names(data_fix) %in% columns_to_remove]
data_reduced <- data[, !names(data) %in% columns_to_remove]



# Now we do the model again

# Fit the multiple linear regression model
model <- lm(YearsAtCompany ~ ., data = data_reduced)

# Summarize the model
summary(model)

vif_values <- vif(model)
print(vif_values)

# Now everything is okay ! We fixed the multicollinearity problem

# Check residual diagnostics

# Examine the residuals of your model to ensure that the assumptions of linear regression are met:

#•	Linearity: The relationship between the predictors and the response should be linear.
#•	Homoscedasticity: The residuals should have constant variance.
#•	Normality: The residuals should be approximately normally distributed.
#•	Independence: The residuals should be independent.

# Residual vs Fitted plot
plot(model, which = 1)

# What we can see : 
#1.	Non-Linearity: The red line (lowess smoother) shows a clear non-linear pattern. 
#Ideally, the red line should be close to the horizontal zero line if the model is appropriate. 
#The curvature in the red line suggests that a non-linear relationship may exist between the predictors and the response variable.
#2.	Heteroscedasticity: The spread of the residuals appears to increase with the fitted values,
#suggesting heteroscedasticity (non-constant variance of residuals).
#This is another indication that the linear model might not be the best fit.
#3.	Outliers: There are some points that are far away from the zero line, 
#indicating potential outliers or influential observations that could unduly affect the model.

#Given the indication of non-linearity and heteroscedasticity, you might consider the following steps:

#1.	Polynomial Regression: Incorporate polynomial terms to model the non-linear relationship.
#2.	Transformation: Transform the response variable or predictors to stabilize the variance.
#3.	Non-linear Models: Consider more flexible non-linear models such as Generalized Additive Models (GAMs).


# Incoroprating polynomial terms is hard because we have multiple linear regression : we would have to try so many different ones (I think)
# Instead, lets try to transform the response variable

# Apply a log transformation to the response variable
data_reduced$log_YearsAtCompany <- log(data_reduced$YearsAtCompany + 1)  # Add 1 to avoid log(0)

# Fit the model with the transformed response variable
model_log <- lm(log_YearsAtCompany ~ ., data = data_reduced)

# Summary of the transformed model
summary(model_log)

# Plot Residuals vs Fitted for the transformed model
plot(model_log, which = 1) # Again, we see a clear non-linear pattern 

# Logarithmizing makes the non-linear shape much clearer, so we can model nicer in this way !

# Now let us do feature selection

# Backward selection

# Perform backward selection using AIC
backward_model <- step(model_log, direction = "backward")

# Summary of the final model after backward selection
summary(backward_model)

# Residual vs Fitted plot
plot(backward_model, which = 1)




# Lasso regression
####### DOES NOT WORK YET --> the problem is that lasso chooses certain
# categorical variables values, but we do not have them one hot encoded (or atleast not all)
# so then it cant make the model

# Prepare data for glmnet
x <- model.matrix(YearsAtCompany ~ ., data = data_reduced)[, -1]  # Remove the intercept
y <- data_reduced$YearsAtCompany

# Fit a Lasso regression model
lasso_model <- cv.glmnet(x, y, alpha = 1, standardize = TRUE)

# Coefficients from the Lasso model at the best lambda
lasso_coef <- coef(lasso_model, s = "lambda.min")
print(lasso_coef)

# Convert to a matrix
lasso_coef_matrix <- as.matrix(lasso_coef)

# Extract the names of the selected features (excluding the intercept)
selected_features <- rownames(lasso_coef_matrix)[lasso_coef_matrix[, 1] != 0]
selected_features <- selected_features[selected_features != "(Intercept)"]
print(selected_features)

# Create formula with selected features
selected_formula <- as.formula(paste("YearsAtCompany ~", paste(selected_features, collapse = " + ")))

# Refit the model using only selected features
final_model <- lm(selected_formula, data = data_reduced)

# Check column names in data_reduced
colnames(data_reduced)

# Summary of the final model
summary(final_model)

# Residual vs Fitted plot
plot(final_model, which = 1)











