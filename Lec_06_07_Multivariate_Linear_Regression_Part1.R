#############################################################
# Multiple Linear Regression codes
# Taken from ISLR Chapter 3
#############################################################
## Set up the work environment ##

# Clearing the workspace
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

###### Help ######
# if you have trouble finding the folder path, try the following:
# (1) Save both the R codes file and the data file you downloaded from Canvas in the same folder
# (2) Go to "Session" in RStudio -> "Set working directory" -> "To source file location"
# source file is the R codes file. 
# (3) You'll see "setwd(...)" show up in the output panel below
# (4) Copy that line and paste onto (replace) the "setwd()" line above
# Many thanks to Andreas Vasiliadis in Sec 32 and Alp Aygen in Sec 33 for their helpful suggestions.

# Call data libraries (which house datasets we may use later)
library(MASS)
library(ISLR) 
# if you don't have these library packages installed already
# install these two packages
# (1) click the "Packages" option in the lower right panel
# (2) click "install"
# (3) type the package name and click "Install"

#################################################################################################
#################################################################################################
# (1) Cross-Correlations and Simple Multivariate Linear Regression
#################################################################################################
#################################################################################################

# Dataset: Advertising Dataset
# The dataset contains statistics about the sales of a product in 200 different markets, 
# together with advertising budgets in each of these markets for different 
# media channels: TV, radio and newspaper. 
# The sales are in thousands of units and the 
# budget is in thousands of dollars.
  
## Loading the data ##
advertising_data=read.csv("Advertising.csv")

# Keep only columns 2-5 (for running cross-correlations in the next step)
advertising_data=advertising_data[,2:5]

## (1.1) Correlations ##
# Let's first look at the correlations among these variables
cor(advertising_data)

# round the correlation output to 4 decimals
round(cor(advertising_data),4)


## (1.2) Multiple Regression ##
# Estimate the full Model
model=lm(sales ~ TV+radio+newspaper, data=advertising_data)

# Save the "summary" output in model_summary
model_summary=summary(lm(sales~TV+radio+newspaper,data=advertising_data))

# View the summary output 
model_summary
# What does the F-statistics suggest about these three variables as a whole?

#################################################################################################
#################################################################################################
# (2) Model Selection: Picking the best model in Multivariate Linear Regressions
#################################################################################################
#################################################################################################
## (2.1) Simple brute-force way: try all possible combinations ##

# Since we have 3 predictors here, 
# the number of all possible models is 2^3 = 8 (including the model with the intercept only) 
# Below we skip the model that includes the intercept only
# Hence, only 7 models

model_1=summary(lm(sales~radio,data=advertising_data))
model_2=summary(lm(sales~newspaper,data=advertising_data))
model_3=summary(lm(sales~TV,data=advertising_data))
model_4=summary(lm(sales~radio+newspaper,data=advertising_data))
model_5=summary(lm(sales~radio+TV,data=advertising_data))
model_6=summary(lm(sales~newspaper+TV,data=advertising_data))
model_7=summary(lm(sales~radio+newspaper+TV,data=advertising_data))

# View the (unadjusted) R-squared of all models
model_1$r.squared
model_2$r.squared
model_3$r.squared
model_4$r.squared
model_5$r.squared
model_6$r.squared
model_7$r.squared
# model 7 has the highest R^2 (just a bit higher than Model 5)

# Report the adjusted R-squared for all models
model_1$adj.r.squared
model_2$adj.r.squared
model_3$adj.r.squared
model_4$adj.r.squared
model_5$adj.r.squared
model_6$adj.r.squared
model_7$adj.r.squared


model_5
# Our criteria is adjusted R-squared (more than one X's)
# Hence, the best model is model 5: lm(sales~radio+TV,data=advertising_data))
# The insignificant variable newspaper is not included in Model 5

################################################################################################
################################################################################################
## (2.2) Forward Selection ##

# Step 1. Estimate all the univariate models and compare R2
model_1_1=summary(lm(sales~radio,data=advertising_data))
model_1_2=summary(lm(sales~newspaper,data=advertising_data))
model_1_3=summary(lm(sales~TV,data=advertising_data))

model_1_1$adj.r.squared
model_1_2$adj.r.squared
model_1_3$adj.r.squared
# Best model is the one with TV (highest Adj R-squared)

# Step 2. Estimate all the bivariate models with TV in it
model_2_1=summary(lm(sales~TV+radio,data=advertising_data))
model_2_2=summary(lm(sales~TV+newspaper,data=advertising_data))

model_2_1$adj.r.squared
model_2_2$adj.r.squared
# Best model is TV + radio (highest Adj R-squared)

# Step 3. Estimate all the tri-variate models with TV and radio in it
model_3=summary(lm(sales~TV+radio+newspaper,data=advertising_data))
model_3$adj.r.squared
# Newspaper is not significant
# Adj R-squared is lower than TV and radio only
# We will not include newspaper, and go back to TV and radio only

# Conclusion: 
# Overall, Best model is TV+radio: lm(sales~radio+TV,data=advertising_data))

################################################################################################
################################################################################################
## Lec 06 In-Class exercise: Backward Selection ##
################################################################################################
################################################################################################
## (2.3) Backward Selection ##

# Step 1. Estimate the model with all the variables
# drop the the variable with the least p-value

rm(list=ls())    

setwd("/Users/andrewkwok/Documents/FINC_3103")

advertising_data=read.csv("Advertising.csv")

advertising_data = advertising_data[,2:5]

backward_selection=lm(sales~radio+newspaper+TV,data=advertising_data)

summary(backward_selection)


# Step 2. Estimate the bivariate model 
# drop the the variable with the least p-value (unless all variables have p-values<5%)

backward_selection=lm(sales~radio+TV, data=advertising_data)
summary(backward_selection)

# Step 3: try to drop one more variable and see what happens


# Step 4: Determine which one is the best model


#################################################################################################
#################################################################################################
# (3) Qualitative Predictors - Credit Card Dataset (change dataset)
#################################################################################################
#################################################################################################

# Clearing the workspace
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")


# Credit Card Dataset
# A simulated data set containing information on ten thousand customers. 
# The aim here is to predict which customers will default on their credit card debt.
# You can find the example from ISLR version 1 (the older version)

# VARIABLES: 
# Income: Income in $1,000's
# Limit: Credit limit
# Rating: Credit rating
# Cards: Number of credit cards
# Age: Age in years
# Education: Number of years of education
# Gender: A factor with levels Male and Female
# Student: A factor with levels No and Yes indicating whether the individual was a student
# Married: A factor with levels No and Yes indicating whether the individual was married
# Ethnicity: A factor with levels African American, Asian, and Caucasian indicating the individual's ethnicity
# Balance: Average credit card balance in $.

##################################################################
## (3.1) Loading data and observe data patterns ##
##################################################################
# Loading the data
credit_data=read.csv("Credit.csv")

# Delete data not needed (the 1st column)
credit_data=credit_data[,-1]
# Alternatively, keep columns 2-12 (see below) like we did in Part 1
# credit_data=credit_data[,2:12]

# Observe the cross-correlations
# cor(credit_data)
# Error in cor(credit_data) : 'x' must be numeric

# Let's use a slightly different expression to look at cross-correlations
# of numerical columns

# have the variable names printed out, 
# so we can enter them properly (spelling, upper/lower cast)
names(credit_data)

cor(credit_data[, c("Balance","Income","Limit","Rating","Cards","Age","Education")])
# show the correlation matrix among quantitative variables

cor(credit_data[, c("Balance","Income")])
# if we only want to look at the correlation between "Balance" and "Income"
cor(credit_data[,c(11,1)])
# we can also just refer to column numbers (instead of column labels)

round(cor(credit_data[, c("Balance","Income","Limit","Rating","Cards","Age","Education")]),2)
# round output to 2 decimals

# We can also show the cross-correlation in scatter plots
pairs(credit_data[, c("Balance","Age","Cards","Education","Income","Limit","Rating")])

##################################################################
## (3.2) Qualitative factor 1: Gender ##
##################################################################
## First let's look at the Gender column and its data class ##
credit_data$Gender
# look at each value in the column

summary(credit_data$Gender)
# summary of the column

class(credit_data$Gender)
# the Gender variable is "character" (i.e. just text)

## Convert the categorical variables to "factors" (qualitative variables) for regression ##
credit_data$Gender = as.factor(credit_data$Gender)

# look at summary and data class again
summary(credit_data$Gender)
class(credit_data$Gender)
# now R divides the data into two categories in summary: femaie and male

## Conduct the regression: Balance against Gender ##
model_gender=lm(Balance~Gender,data=credit_data)

# Show the regression summary
summary(model_gender)
# Q: how to interpret the results?
# Q: What is the baseline variable here?

## if we want to have "male" as the baseline variable ##
credit_data$Gender = relevel(as.factor(credit_data$Gender), "Male")
summary(credit_data$Gender)
# the first variable in the summary output is the "baseline"

model_gender_2=lm(Balance~Gender,data=credit_data)
summary(model_gender_2)
# here we obtain the same table as that in the lecture slide
# (also ILSR v1 p.84 Table 3.7)

# or we can generate a dummy variable for "Female"
credit_data$Female= ifelse(credit_data$Gender=="Female",1,0)
# generate a new variable "Female" 
# Female=1 if Gender="Female"; 0 otherwise

model_gender_3=lm(Balance~Female,data=credit_data)
summary(model_gender_3)

##################################################################
## (3.3) In-Class Exercise ##
## Qualitative factor 2: Ethnicity ##
##################################################################
## Step 1: convert Ethnicity to factors
credit_data$Ethnicity = as.factor(credit_data$Ethnicity)


## Step 2: Run the regression: Balance against Ethnicity
model_ethnicity = lm(Balance ~ Gender, data=credit_data)

summary(model_ethnicity)
## Step 3: Re-Run the regression (Balance against Ethnicity) 
## with Asian (American) as the baseline variable
credit_data$Ethnicity = relevel(as.factor(credit_data$Ethnicity), "Asian")
model_ethnicity_2 = lm(Balance ~ Gender, data=credit_data)

summary(model_ethnicity_2)
##################################################################
## (3.4) In-Class Exercise ##
## Qualitative factor 3: Student or not ##
##################################################################
## Step 1: convert Student status to factors
credit_data$Student = as.factor(credit_data$Student)

## Step 2: Run the regression (Balance against Student) 
## with not a student (Student=No) as the baseline variable

#Student = No baseline
model_student = lm(Balance ~ Student, data=credit_data)
summary(model_student)

#Student = Yes baseline
credit_data$Student = relevel(as.factor(credit_data$Student), "Yes")
model_student_2 = lm(Balance ~ Student, data = credit_data)

summary(model_student_2)

summary(l)
## Step 3: Answer the following questions
## (1) What are the average levels of balance for non-student and student repectively?
## (2) Is the student status a statistically significant predictor of balance
## at the 5% significance level?


################################################################################################
################################################################################################

## END ##
