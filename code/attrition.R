library(readr)
general_data <- read.csv("general_data.csv")
employee_survey_data <- read.csv("employee_survey_data.csv")
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

model <- lm(Attrition ~ ., data = dati)
summary(model)
