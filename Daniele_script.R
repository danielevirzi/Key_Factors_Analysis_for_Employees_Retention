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
library(MASS) # For step, glm, lda, qda
library(pROC) # For ROC curve
#library(ISLR2) For dataset so useless
library(car) # For vif
library(faraway) # For ANCOVA
library(e1071)
library(class) # for K-Nearest Neighbors (KNN)
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
# Make the labels more suitable for the plot

add_rotated_labels <- function(labels, vertical_adjustment = 1) {
  text(x = seq_along(labels), y = par("usr")[3] - vertical_adjustment, srt = 45, adj = 1, labels = labels, xpd = TRUE, cex = 1)
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
data$Attrition <- factor(data$Attrition)
data$Gender <- factor(data$Gender)
data$BusinessTravel <- factor(data$BusinessTravel)
data$JobRole <- factor(data$JobRole)
data$Department <- factor(data$Department)
data$EducationField <- factor(data$EducationField)
data$MaritalStatus <- factor(data$MaritalStatus)
data$StockOptionLevel <- factor(data$StockOptionLevel)
data$Education <- factor(data$Education)
data$JobLevel <- factor(data$JobLevel)
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction)
data$JobSatisfaction <- factor(data$JobSatisfaction)
data$WorkLifeBalance <- factor(data$WorkLifeBalance)
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


# BASIC ANALYSIS OF CATEGORICAL VARIABLES
# TODO : some labels not showing, but I did not manage to implement the rorate thing here for some reason
#shutoff_plots()
par(mfrow=c(2,3), 
    mar=c(4,2,2,2))


plot(data$EnvironmentSatisfaction,col=c(2:5), main="Environment Satisfaction", las=1)
plot(data$JobSatisfaction,col=c(2:5),main="Job Satisfaction", las=1)
plot(data$WorkLifeBalance,col=c(2:5),main="Work Life Balance", las=1)
plot(data$JobLevel,col=c(2:6),main="Job Level", las=1)
plot(data$Education,col=c(2:6),main="Education", las=1)
plot(data$StockOptionLevel,col=c(2:4),main="Stock Option Level", las=1)


par(mfrow=c(2,2), 
    mar=c(4,2,2,2))
plot(data$Gender,col=c(2:3),main="Gender", las=1)
plot(data$BusinessTravel,col=c(2:4),main="Business Travel", las=1)
plot(data$MaritalStatus,col=c(2:4),main="Marital Status", las=1)
plot(data$Department,col=c(2:4),main="Department", las=1)

par(mfrow=c(2,1), 
    mar=c(2,12,2,2))
plot(data$JobRole,col=c(2:10),main="Job Role", las=1, horiz=TRUE)
plot(data$EducationField,col=c(2:6),main="Education Field", las=1, horiz=TRUE)

# Rotate the x axis labels for better readability

# We note that JobSatisfaction and EnvironmentSatisfaction are extremely similar.
# In fact, let us calculate the chi squared statistic between these two

contingency_table <- table(data$EnvironmentSatisfaction, data$JobSatisfaction)

chi_squared <- chisq.test(contingency_table) # I guess we did smth like this in stat 1 ?
# Print the correlation coefficient
print(chi_squared)

# p-value of 0.08
# As Job Satisfaction includes Environment Satisfaction, we could remove Environment Satisfaction from our analysis

# Remove Environment Satisfaction from the data
data$EnvironmentSatisfaction <- NULL



# BASIC ANALYSIS OF NUMERICAL VARIABLES
#shutoff_plots()
par(mfrow=c(2,4),  
    mar=c(2,2,2,2))

# Density plots for numerical variables
plot(density(data$Age), main = "Age", xlab = "Age", col = "skyblue", border = "white", lwd = 4)
plot(density(data$DistanceFromHome), main = "Distance from Home", xlab = "Distance from Home", col = "skyblue", border = "white", lwd = 4)
plot(density(data$TotalWorkingYears), main = "Total Working Years", xlab = "Total Working Years", col = "skyblue", border = "white", lwd = 4)
plot(density(data$PercentSalaryHike), main = "Percent Salary Hike", xlab = "Percent Salary Hike", col = "skyblue", border = "white", lwd = 4)

# Histograms for numerical variables
hist(data$TrainingTimesLastYear, main = "Training Times Last Year", xlab = "Training Times Last Year", col = "skyblue", border = "white")
hist(data$YearsSinceLastPromotion, main = "Years Since Last Promotion", xlab = "Years Since Last Promotion", col = "skyblue", border = "white")
hist(data$YearsWithCurrManager, main = "Years With Current Manager", xlab = "Years With Current Manager", col = "skyblue", border = "white")
hist(data$NumCompaniesWorked, main = "Number of Companies Worked", xlab = "Number of Companies Worked", col = "skyblue", border = "white")






# In our project we are interested in
# Attrition (To model on which grounds people stayed or left their job)
# Years at the Company (To model which factors were deceicive of people staying longer or shorter in the company)


##### YEARS AT COMPANY ANALYSIS ######

# Distribution of YearsAtCompany

#shutoff_plots()
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2) + 0.1, cex.axis = 0.8, cex.lab = 0.8)
hist(data$YearsAtCompany,
     main = "Distribution of Years at Company",
     xlab = "Years at Company",
     col = "skyblue",
     border = "white",
     breaks = 20) # Adjust the number of breaks as needed


# DA MODIFICARE

# Boxplots with years at company as references for all categorical data
#shutoff_plots()
par(mfrow = c(2, 5),
    mar = c(2, 2, 2, 2))

# Function to add rotated x-axis labels with vertical adjustment
add_rotated_labels <- function(labels, vertical_adjustment = 1) {
  text(x = seq_along(labels), y = par("usr")[3] - vertical_adjustment, srt = 45, adj = 1, labels = labels, xpd = TRUE, cex = 0.8)
}

boxplot(YearsAtCompany ~ Attrition, data = data, main = "Attrition", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$Attrition))

boxplot(YearsAtCompany ~ BusinessTravel, data = data, main = "Business Travel", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$BusinessTravel))

boxplot(YearsAtCompany ~ Department, data = data, main = "Department", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$Department))

boxplot(YearsAtCompany ~ Education, data = data, main = "Education", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$Education))

boxplot(YearsAtCompany ~ EducationField, data = data, main = "Education Field", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$EducationField))

boxplot(YearsAtCompany ~ Gender, data = data, main = "Gender", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$Gender))

boxplot(YearsAtCompany ~ JobLevel, data = data, main = "Job Level", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$JobLevel))

boxplot(YearsAtCompany ~ JobRole, data = data, main = "Job Role", col = c(4, 6), xaxt = "n", xlab = "") 
add_rotated_labels(levels(data$JobRole))

boxplot(YearsAtCompany ~ MaritalStatus, data = data, main = "Marital Status", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$MaritalStatus))

boxplot(YearsAtCompany ~ StockOptionLevel, data = data, main = "Stock Option Level", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$StockOptionLevel))

boxplot(YearsAtCompany ~ JobSatisfaction, data = data, main = "Job Satisfaction", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$JobSatisfaction))

boxplot(YearsAtCompany ~ WorkLifeBalance, data = data, main = "Work-Life Balance", col = c(4, 6), xaxt = "n", xlab = "")
add_rotated_labels(levels(data$WorkLifeBalance))





##### ATTRITION ANALYSIS ######

# Distribution 
attrition_distribution <- table(data$Attrition)
print(attrition_distribution)

# Plot the bar plot
par(mfrow = c(1, 1))
barplot(attrition_distribution,
        main = "Distribution of Attrition",
        xlab = "Attrition Categories",
        ylab = "Frequency",
        col = "skyblue",
        border = "white",
        las = 0) # las = 2 for vertical labels

# Our data is unbalanced, but we have many samples

# Boxplots with attrition as references for all numerical data

par(mfrow=c(2,5),mar=c(2,1.85,2.5,0))
boxplot(Age~Attrition,data=data,main="Age",col=c(4,6))
boxplot(DistanceFromHome~Attrition,data=data,main="DistanceFromHome",col=c(4,6))
boxplot(MonthlyIncome~Attrition,data=data,main="MonthlyIncome",col=c(4,6))
boxplot(NumCompaniesWorked~Attrition,data=data,main="NumCompaniesWorked",col=c(4,6))
boxplot(PercentSalaryHike~Attrition,data=data,main="PercentSalaryHike",col=c(4,6))
boxplot(TotalWorkingYears~Attrition,data=data,main="TotalWorkingYears",col=c(4,6))
boxplot(TrainingTimesLastYear~Attrition,data=data,main="TrainingTimesLastYear",col=c(4,6))
boxplot(YearsAtCompany~Attrition,data=data,main="YearsAtCompany",col=c(4,6))
boxplot(YearsSinceLastPromotion~Attrition,data=data,main="YearsSinceLastPromotion",col=c(4,6))
boxplot(YearsWithCurrManager~Attrition,data=data,main="YearsWithCurrManager",col=c(4,6))





##### CORRELATION MATRIX ######

#shutoff_plots()    
par(mfrow = c(1, 1))

# Select only numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Compute correlation matrix
correlation_matrix <- cor(numeric_data)

# Plot correlation matrix bigger
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)









######################################### STEP 4 MODELING #########################################

set.seed(123)

n <- dim(data)[1]

test <- sample(1:n, n*0.2) # indexes of data in the validation set
train <- setdiff(1:n, test) # indexes of data in training set

test.data <- data[test, ] # validation set
train.data <- data[train, ] # training set

# Print the dimensions of the data
dim(train.data)
dim(test.data)


######### LINEAR REGRESSION FOR YEARS AT COMPANY #########


### SIMPLE LINEAR REGRESSION ###

# Simple linear regression model using YearsWithCurrManager on YearsAtCompany
model_slr <- lm(YearsAtCompany ~ YearsWithCurrManager, data = train.data)
summary(model_slr)

# Plot the model
par(mfrow = c(2, 2))
plot(model_slr)

par(mfrow = c(1, 1))
# Scatter plot of YearsAtCompany vs YearsWithCurrManager
plot(train.data$YearsWithCurrManager, train.data$YearsAtCompany, xlab = "YearsWithCurrManager", ylab = "YearsAtCompany")
# Regression line
abline(model_slr, col = "red", lwd = 3)


### MULTIPLE LINEAR REGRESSION ###

# Fit the multiple linear regression model using all variables on the target variable YearsAtCompany
model_mlr1 <- lm(YearsAtCompany ~ ., data = train.data)

# Summarize the model
summary(model_mlr1)

# Plot the model
plot(model_mlr1, which = 1)

# First, we will check for multicollinearity
vif_values <- vif(model_mlr1)
print(vif_values)

# We consider VIF values over 3 high. Thus, we want to remove them.
# We find the two problematic variables to be Department and EducationField

columns_to_remove <- c("Department", "EducationField")

data_reduced <- train.data[, !names(data) %in% columns_to_remove]
# We also need to remove them in test # TODO : do we need this ? 
data_reduced_test <- test.data[, !names(data) %in% columns_to_remove]

# Refit the model
model_mlr2 <- lm(YearsAtCompany ~ ., data = data_reduced)
summary(model_mlr2)

# See that it worked 
vif_values <- vif(model_mlr2)
print(vif_values)

# DIAGNOSTIC CHECKS

# Residual vs Fitted plot
plot(model_mlr2, which = 1)
# We see a non-linear relationship ! Let us try to make it more clear by 
# doing a log-transform (I tried this and it works nicely, but does it make sense?)

# Apply a log transformation to the response variable
#data_reduced$log_YearsAtCompany <- log(data_reduced$YearsAtCompany + 1)  # Add 1 to avoid log(0)

# Fit the model with the transformed response variable
#model_log <- lm(log_YearsAtCompany ~ ., data = data_reduced)

# Summary of the transformed model
#summary(model_log)

# Plot Residuals vs Fitted for the transformed model
#plot(model_log, which = 1)

# Now we see the non-linear pattern way better !
# It look a lot like a polynomial regression, so we try this as a model now

### POLYNOMIAL REGRESSION ###

# Create a function to generate polynomial terms
generate_poly_formula <- function(data, degree = 2) {
  predictors <- colnames(data)
  predictors <- predictors[!predictors %in% c("YearsAtCompany")]  # Exclude the response variable
  
  # Create polynomial terms for each predictor
  poly_terms <- sapply(predictors, function(x) paste0("poly(", x, ", ", degree, ", raw = TRUE)"))
  formula <- paste("YearsAtCompany ~", paste(poly_terms, collapse = " + "))
  
  return(as.formula(formula))
}

# Apply the function to create the polynomial formula
poly_formula <- generate_poly_formula(data_reduced[], degree = 2)

# Print the polynomial formula
print(poly_formula)

# Fit the polynomial regression model
model_poly <- lm(poly_formula, data = data_reduced)

summary(model_poly)



# Next, we do feature selection, since we still have many features and a lot of them are not significant

# Perform backward selection using AIC
backward_model <- step(model_poly, direction = "backward")

# Summary of the final model after backward selection
summary(backward_model)

# Plot Residuals vs Fitted for the transformed model
plot(backward_model, which = 1)
plot(backward_model, which = 2)


### FEATURE SELECTION ###

# Model with the best set of features 
best_model <- lm(YearsAtCompany 
                  ~ poly(Age, 2, raw = TRUE) +
                    poly(Education, 2, raw = TRUE) +
                    Gender +
                    poly(NumCompaniesWorked , 2, raw = TRUE) +
                    TotalWorkingYears +
                    poly(TrainingTimesLastYear, 2, raw = TRUE) +
                    poly(YearsSinceLastPromotion, 2, raw = TRUE) +
                    YearsWithCurrManager +
                    poly(JobSatisfaction, 2, raw = TRUE),
                  
                  data = data_reduced)

summary(best_model)

# Check residual diagnostics for the new model
par(mfrow = c(2, 2))
plot(best_model)





### COOK'S DISTANCE ###

# Use Cook’s distance or leverage values to identify influential points.

# Calculate Cook's distance

cooksd <- cooks.distance(backward_model)

# Plot Cook's distance
par(mfrow = c(1, 1))
plot(cooksd, pch = "*", cex = 2, main = "Cook's Distance")
# 3 times the mean Cook's distance is a common threshold for identifying influential points
abline(h = 3 *mean(cooksd, na.rm=TRUE),  col = "green")

# Identify the points with high Cook's distance 
influential <- which(cooksd > 4/(nrow(data_reduced) - length(model_poly$coefficients)))
print(influential)
print(length(influential))

# Put the plot as a side-by-side comparison
par(mfrow = c(1, 2))
boxplot(data_reduced, main = "Original Data")
boxplot(data_reduced[influential, ], main = "Influential Points")


# Optionally, remove influential points and refit the model
data_reduced_clean <- data_reduced[-influential, ]


# Final model 
final_model <- lm(YearsAtCompany 
                   
                   ~ poly(Age, 2, raw = TRUE) +
                     poly(Education, 2, raw = TRUE) +
                     Gender +
                     poly(NumCompaniesWorked , 2, raw = TRUE) +
                     TotalWorkingYears +
                     poly(TrainingTimesLastYear, 2, raw = TRUE) +
                     poly(YearsSinceLastPromotion, 2, raw = TRUE) +
                     YearsWithCurrManager +
                     poly(JobSatisfaction, 2, raw = TRUE),
                     
                   data = data_reduced_clean)

summary(final_model)

# Check residual diagnostics for the new model
par(mfrow = c(2, 2))
plot(final_model)


# Let us see the effect of lasso regression instead of feature selection,
# as it pushes values to 0 (effectively doing feature selection)

# Prepare the data
x_train <- model.matrix(poly_formula, data = data_reduced_clean)[,-1] # Use data where we removed outliers ; but use the poly_model (last model before we applied feature selection)


y_train <- data_reduced_clean$YearsAtCompany 

# Fit Lasso regression model (alpha = 1) 
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)










### EVALUATION ###


##### FINAL MODEL USING BACKWARDS FEAT SELECT

# Predicting on the test set
predictions <- predict(final_model, newdata = test.data)

# Calculate the Mean Squared Error (MSE)
mse <- mean((test.data$YearsAtCompany - predictions)^2)
print(paste("MSE on validation set:", mse))

# Calculate residuals
residuals <- test.data$YearsAtCompany - predictions

# Plot residuals
shutoff_plots()
plot(predictions, residuals, 
     xlab = "Predicted Values", 
     ylab = "Residuals", 
     main = "Residual Plot",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Actual vs predicted 
plot(test.data$YearsAtCompany, predictions,
     xlab = "Actual Values", 
     ylab = "Predicted Values", 
     main = "Actual vs Predicted Values",
     pch = 20, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Line with slope 1 to indicate perfect prediction


# R-squared on the test set
# Calculate R-squared
ss_total <- sum((test.data$YearsAtCompany - mean(test.data$YearsAtCompany))^2)
ss_res <- sum((test.data$YearsAtCompany - predictions)^2)
r_squared <- 1 - (ss_res / ss_total)
print(paste("R-squared on validation set for backwards feature selection:", r_squared))

#### FINAL MODEL USING LASSO REGRESSION 

# Create design matrix for predictors in the test set
x_test <- model.matrix(poly_formula, data = test.data)[,-1]

# Predictions
lasso_predictions <- predict(lasso_model, newx = x_test, s = "lambda.min")

# Calculate the Mean Squared Error (MSE) for Lasso
lasso_mse <- mean((test.data$YearsAtCompany - lasso_predictions)^2)
print(paste("Lasso MSE on validation set:", lasso_mse))

# Calculate residuals
lasso_residuals <- test.data$YearsAtCompany - lasso_predictions


# Plot residuals
shutoff_plots()
plot(lasso_predictions, residuals, 
     xlab = "Predicted Values", 
     ylab = "Residuals", 
     main = "Residual Plot",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Plot predictions vs actual
plot(test.data$YearsAtCompany, lasso_predictions,
     xlab = "Actual Values", 
     ylab = "Predicted Values", 
     main = "Actual vs Predicted Values",
     pch = 20, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Line with slope 1 to indicate perfect prediction

# R-squared on the test set
# Calculate R-squared
# ss_total same as above
ss_res_lasso <- sum((test.data$YearsAtCompany - lasso_predictions)^2)
r_squared_lasso <- 1 - (ss_res_lasso / ss_total)
print(paste("R-squared on validation set for lasso regression:", r_squared_lasso))


#BALANCING THE DATASET (put after 6.2 Classification Model)

positive_samples <- train.data[train.data$Attrition == 'Yes', ]
negative_samples <- train.data[train.data$Attrition == 'No', ]

set.seed(123)

negative_sampled <- negative_samples[sample(nrow(negative_samples), 1200), ]

balanced_train <- rbind(positive_samples, negative_sampled)

balanced_train <- balanced_train[sample(nrow(balanced_train)), ]

print(table(balanced_train$Attrition))

#Overwriting train.data as to not have to change all the models below
train.data <- balanced_train
