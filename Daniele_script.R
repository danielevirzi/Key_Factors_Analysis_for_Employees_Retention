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
data <- data[, !(names(data) %in% c("EmployeeID", "Over18", "StandardHours", "EmployeeCount"))]

# Check if columns have been removed
str(data)

##### CONVERT CATEGORICAL VARIABLES TO FACTORS ######

# Convert categorical variables to factors
data$Attrition <- as.factor(data$Attrition)
data$Gender <- as.factor(data$Gender)
data$BusinessTravel <- as.factor(data$BusinessTravel)

# One-hot encoding of categorical variables
data <- one_hot_encoding(data, "JobRole")
data <- one_hot_encoding(data, "Department")
data <- one_hot_encoding(data, "EducationField")
data <- one_hot_encoding(data, "MaritalStatus")

# See MonthlyIncome distribution
hist(data$MonthlyIncome)

# Log-Scale MonthlyIncome
data$MonthlyIncome <- log(data$MonthlyIncome)


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









