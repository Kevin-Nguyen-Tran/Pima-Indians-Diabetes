#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

# PREREQS:
rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(Rmisc) # transform integers to factors, must be first or will mask other packages. We only need multiplot!
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(dplyr)
library(wesanderson)
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(ggcorrplot) # pearon's correlation and heatmaps

# URL: https://www.kaggle.com/uciml/pima-indians-diabetes-database

#READ DATA SET AND STORE INTO OBJECT:
data <- read.csv("C:/Users/Kevin/Desktop/Pima-Indians-Diabetes #6/diabetes.csv")

diabetes_data <- as.tibble(data)

# Units:
# Pregnancies: number of times pregnant
# glucose: in mg/dL
# blood pressure: in mm Hg
# skinthickness: triceps skin fold in mm
# Insulin: mu U/ml
# BMI in kg/m^2^
# DiabetesPedigree: likelihood of having diabetes based on familial history
# Age: in years
# Outcome: 0 if no diabetes and 1 if diabetic

# Noticed that first 6 variables had 0. Which doesn't seem realistic. 0 BP, 0 BMI, 0 insulin and etc.

# NEED TO TIDY DATA SET:
glimpse(diabetes_data)

#it just seems that Outcome needs to be changed to a factor. All other variables are in correct class. **** IF YOU DO THIS, YOU CAN'T USE PEARSON'S CORRELATION, SINCE IT IS NOT NUMBERIC!

diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

# Now that outcome is in the correct class we can brainstorm some possible routes of analysis.

#=============================================================================================================================================================================================================================
# ANALYSIS & INITIAL BRAINSTORM
#=============================================================================================================================================================================================================================

# Potential research and analysis opportunities: (**) = green light analysis & (xx) = red light (cannot do it)
# Run a PCA or use a heat map to see the highest correlation between certain variables: we are the most interested in the outcome of diabetics
# Run a logistic linear regression model (binomial) to find good predictors of diabetes based on the variables provided
# which ever variables are correlated to diabetes, further explore with added research on the normal ranges and etc.
# Let's do pearson's correlation and create a heat map to see correlation between the continuous numeric variables!

#=============================================================================================================================================================================================================================
# 1. PEARSON'S CORRELATION - HEAT MAP
#=============================================================================================================================================================================================================================

ggcorrplot(cor(diabetes_data), hc.order = TRUE)
# based on this correlation plot, we are more interested in how outcome fares with the other numeric variables.
# Pregnancy, age, diabetes pedigree function, and insulin have a slightly positive correlation with outcome
# Glucose and BMI have a strong positive correlation with Outcome.
# Blood pressure and skin thickness does not appear to show either a positive or negative correlation with Outcome.
# This will be our overhead analysis and we will now dive a bit deeper into each and see how it correlates and if it matches our linear regression model.

cor(diabetes_data)

#=============================================================================================================================================================================================================================
# 2. PREGNANCY, AGE, DIABETES PEDIGREE FUNCTION, AND INSULIN VS DIABETES
#=============================================================================================================================================================================================================================

diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

ggplot(diabetes_data, aes(x = Outcome, y = Pregnancies)) +
  geom_boxplot()
# Showing a positive relation: diabetic patients have more preganancies on average than non-diabetic patients
t.test(diabetes_data$Pregnancies ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)


ggplot(diabetes_data, aes(x = Outcome, y = Age)) +
  geom_boxplot()
# Showing a positive relation: diabetic patients are older on average than non-diabetic patients
t.test(diabetes_data$Age ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)


ggplot(diabetes_data, aes(x = Outcome, y = DiabetesPedigreeFunction)) +
  geom_boxplot()
# Showing a positive relation: diabetic patients have a higher diabetes pedigree function score on average than non-diabetic patients
t.test(diabetes_data$DiabetesPedigreeFunction ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)


ggplot(diabetes_data, aes(x = Outcome, y = Insulin)) +
  geom_boxplot()
# Showing a positive relation: diabetic patients have on average higher Insulin levels than non-diabetic
t.test(diabetes_data$Insulin ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

# All variables above show a statistically significant positive relationship with diabetic outcome.


















