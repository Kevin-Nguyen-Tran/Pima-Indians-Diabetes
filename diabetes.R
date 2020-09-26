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
# glucose: in mg/dL - based on the numbers, it seems that our values are of DBP (diastolic blood pressure)
# blood pressure: in mm Hg
# skinthickness: triceps skin fold in mm
# Insulin: mu U/ml
# BMI in kg/m^2^
# DiabetesPedigree: likelihood of having diabetes based on familial history
# Age: in years
# Outcome: 0 if no diabetes and 1 if diabetic

# Noticed that first 6 variables had 0. Which doesn't seem realistic. 0 BP, 0 BMI, 0 insulin and etc.

# NEED TO TIDY DATA SET:
summary(diabetes_data)
str(diabetes_data)

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

ggcorrplot(cor(data), hc.order = TRUE, lab = TRUE, lab_size = 3)
# based on this correlation plot, we are more interested in how outcome fares with the other numeric variables.
# Pregnancy, age, diabetes pedigree function, and insulin have a slightly positive correlation with outcome
# Glucose and BMI have a strong positive correlation with Outcome.
# Blood pressure and skin thickness does not appear to show either a positive or negative correlation with Outcome.
# This will be our overhead analysis and we will now dive a bit deeper into each and see how it correlates and if it matches our linear regression model.

cor(data)

#=============================================================================================================================================================================================================================
# 2. BLOOD PRESURE AND SKIN THICKNESS VS DIABETES
#=============================================================================================================================================================================================================================

ggplot(diabetes_data, aes(x = Outcome, y = BloodPressure)) +
  geom_boxplot()

t.test(diabetes_data$BloodPressure ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# based on the t.test there is not a statistically significant difference between the average blood pressure of someone that is diabetic vs not diabetic.
# zero is also in the confidence interval meaning that there is a chance of no difference (zero indicates no difference). 

ggplot(diabetes_data, aes(x = Outcome, y = SkinThickness)) +
  geom_boxplot()

t.test(diabetes_data$SkinThickness ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# based on the t-test, there is not a statistically significant difference between the Skin Thickness of the tricep of a diabetic and non-diabetic
# the confidence interval includes a zero meaning there is a chance of no difference between the average skin thickness

# Both outcomes reflect the pearson's correlation/heat map as it claims that both blood pressure and skin thickness do not positively nor negatively relate to Outcome (diabetic vs not diabetic)


#=============================================================================================================================================================================================================================
# 3. PREGNANCY, AGE, DIABETES PEDIGREE FUNCTION, AND INSULIN VS DIABETES
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
# Next, we will look at glucose levels and BMI and their relationship to diabetes. 
# Based on the heat map, we will assume a strong positive correlation (small p-values/closer to zero)

#=============================================================================================================================================================================================================================
# 4. GLUCOSE LEVELS AND BMI VS DIABETES
#=============================================================================================================================================================================================================================

# Glucose and BMI have a strong positive correlation with Outcome.
ggplot(diabetes_data, aes(x = Outcome, y = Glucose)) +
  geom_boxplot()

t.test(diabetes_data$Glucose ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

ggplot(diabetes_data, aes(x = Outcome, y = BMI)) +
  geom_boxplot()

t.test(diabetes_data$BMI ~ diabetes_data$Outcome, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)


# As expected, both outcomes show a strong positive correlation with Outcome. The p-values for both t.test showed 2.2e-16 which was the p-value closest to zero out of all t.tests ran.
# we will hypothesize that on a linear regression model, that both glucose and BMI will be good predictors of diabetes and that both will have small p-values and the largest effect sizes of all variables.

#=============================================================================================================================================================================================================================
# 5. LINEAR REGRESSION MODEL - BINOMIAL
#=============================================================================================================================================================================================================================

predicted <- glm(Outcome ~ ., family = "binomial", data = diabetes_data)
summary(predicted)

# As shown in the summary: Pregnancy, Glucose and BMI have the smallest p-value of all variables (as indicated by the three stars), indicating statistical significance.
# Next, DiabetesPedigree function and Blood pressure show statistical significant but with slightly higher p-values.
# Lastly, Age, Insulin, and Skin thickness all show large p-values (greater than 5%) which shows they are not good predictors of diabetes

# Now we will plot the graph of our generalized linear model to see if it effectively captures the relationship. If so, closer to zero should represent no diabetes and closer to one should represent diabetes.

probability_data <- data.frame(fitted.values = predicted$fitted.values, outcome = diabetes_data$Outcome)

probability_data <- probability_data %>%
  arrange(fitted.values)


probability_data <- probability_data %>%
  mutate(rank = 1:nrow(probability_data))

ggplot(probability_data, aes(x = rank, y = fitted.values, color = outcome)) +
  geom_point(alpha = 1, shape = 1, stroke = 2) +
  xlab("Rank") +
  ylab("Predicted Probability of Having Diabetes")

# Our graph makes sense, which means our glm above (predicted object) accurately captures the relationship between variables.
# Given a sample of the same variables, we can predict the likelihood of an individual having diabetes.

#=============================================================================================================================================================================================================================
# GOOD PREDICTORS AND DIABETES
#=============================================================================================================================================================================================================================

# after, we can do deeper analysis for the good predictors as shown above in the linear regression model:
# Pregnancy, glucose, bmi, diabetespedigree function, and bp**

# BRAIN STORM MODES OF ANALYSIS:
# density plots for each variables comparing those with and without diabetes
# add details of normal ranges from online resources

preg_dp <- ggplot(diabetes_data, aes(x = Pregnancies, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

gluc_dp <- ggplot(diabetes_data, aes(x = Glucose, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

bmi_dp <- ggplot(diabetes_data, aes(x = BMI, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

dpf_dp <- ggplot(diabetes_data, aes(x = DiabetesPedigreeFunction, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

bp_dp <- ggplot(diabetes_data, aes(x = BloodPressure, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

multiplot(preg_dp, gluc_dp, bmi_dp, dpf_dp, bp_dp, cols = 2)

# As we can see, non-diabetic patients will have a tall peak where the majority of patients will fall into that bin depending on the variables
# for example: majority of non-diabetic patients will have 0-1 preganancies as shown by the tall peak!
# majority of non-diabetic patients will have a glucose level of 100 as shown by the tall peak.
# We notice that the strong predictors (those with small p-values) have a slightly shifted density plot (they do not overlap completely) or have a much wider density as seen in Pregnancies.
# Variables with statistically significant variability (< 5%), but are not as strong predictors as the top 3) have overlapping density plots (with slight rightward shift) and similar wideness in density (slightly wider)
# although diabetespedigreefunction and blood pressure are slightly different, it is still statistically significant as shown by our linear regression model
# even if it is statistically significant, is it medically/scientifically significant?

# pregnancy resources:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6274679/
# https://care.diabetesjournals.org/content/26/5/1646 ::for multiple pregnancies and findings for increase risk of diabetes

# glucose resources:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2797383/ ::there is a graph for criteria for the diagnosis of diabetes based on glucose levels in mg/dl or mmol/l

# BMI resources:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4457375/ ::BMIs for males and females

# no resources on what diabetespedigreefunction is outside of this data set on kaggle. It seems to be specific to this data set and not used elsewhere
# All we know is that it calculates the liklihood of diabetes onset depending on your family history

# bp rersources:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3942662/ ::has treatment bp and what it should be for diabetic pts
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4609903/ ::shows that treatment should start if bp is >140mmHg
# it seems that our values are of diastolic blood pressure but the link above 4609903 has treatment for both SBP and DBP
# but our values/averages are below that of treatment.













