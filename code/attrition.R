# Setting the correct working directory (Location where our data & R files are stored)
# We set it via relative paths such that we can work on the same code without 
# the need to change it all the time

# Clear current working environment
rm(list=ls())
# Get the directory of the current script
script_dir <- getwd()
general_data <- read.csv("./data/general_data.csv")
employee_survey_data <- read.csv("./data/employee_survey_data.csv")


dati<-cbind(employee_survey_data, general_data)
dati<-dati[,-12]
dati<-dati[,-12]
dati<-dati[ ,-18]
dati<-dati[ ,-19]
dati<-dati[ ,-1]
dati$Attrition<-factor(dati$Attrition)
dati$BusinessTravel<-factor(dati$BusinessTravel)
dati$Department<-factor(dati$Department)
dati$EducationField<-factor(dati$EducationField)
dati$EducationField<-factor(dati$EducationField)
dati$JobRole<-factor(dati$JobRole)
dati$StockOptionLevel<-factor(dati$StockOptionLevel)
dati$Gender<-factor(dati$Gender)
dati$MaritalStatus<-factor(dati$MaritalStatus)
dati$EnvironmentSatisfaction<-factor(dati$EnvironmentSatisfaction)
dati$JobSatisfaction<-factor(dati$JobSatisfaction)
dati$WorkLifeBalance<-factor(dati$WorkLifeBalance)
dati$Education<-factor(dati$Education)
dati$JobLevel<-factor(dati$JobLevel)



str(dati)

dati <- na.omit(dati)



model <- glm(Attrition ~ ., data = dati, family = binomial)
summary_model <- summary(model)





model_good <- stepAIC(model, direction = "both")
summary_model_good <- summary(model_good)

summary_model_good

# McFadden's R-squared
rsq <- 1 - (summary_model_good$deviance / summary_model_good$null.deviance)

print(paste("McFadden's R-squared: ", rsq))

hist(dati$MonthlyIncome)
model_2 <- lm(YearsAtCompany ~ ., data = dati)
summary_model_2 <- summary(model_2)

summary_model_2

model_2_good <- stepAIC(model_2, direction="both")
summary_model_2_good <- summary(model_2_good)

summary_model_2_good

#confront attrition and yearsatcompany to see if there is a difference in what makes you stay for the year and what makes you stay longer
plot(model_2_good)
plot(model)

summary_model_2_good
summary_model_good
