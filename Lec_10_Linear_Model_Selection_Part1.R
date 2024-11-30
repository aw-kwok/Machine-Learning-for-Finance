#############################################################
# Lec 10: Model Section
# Reference: ISLR v2 Chapter 6 pp.267-274

#############################################################
## Set up the work environment ##
#############################################################

# Clear the workspace
rm(list=ls())

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Loading the required packages
# Note: "leaps" is new (we are using it for the first time)
library(ISLR)
library(leaps)

# if you don't have these library packages installed already
# install these two packages
# (1) click the "Packages" option in the lower right panel
# (2) click "install"
# (3) type the package name and click "Install"

####################################################################################
# (0) Load the credit dataset
####################################################################################

# Loading the credit data
attach(Credit)
Credit=Credit
dim(Credit)

# Delete the first column (just indexing)
Credit=Credit[,-1]

# Obtain the size of Credit
dim(Credit)
## [1] 400  11 (400 rows/obs, 11 columns/variables)

####################################################################################
# (1) Best Subset Selection using Mallow's CP, BIC, Adjusted R-square
####################################################################################

# Perform regressions of all possible combinations
regfit.full=regsubsets(Balance ~. ,data=Credit, nvmax=20)
## The regsubsets() function (part of the leaps library) performs best subset selection
## here the outcome variable is Balance, data used is "Credit", maximum # of var=20 
## var=20: although we have only 11 variables here, adding a few more here to accommodate dummy variables generated for qualitative variables
## The syntax of regsubsets() is similr to lm() or glm()

# Saving the summary of the subset regressions
reg.summary=summary(regfit.full)

# View the results summary
reg.summary

# Looking at all the statistics reported by R and saved in summary()
names(reg.summary)

# Reporting R-square for M0, M1, .., Mp
reg.summary$rsq

# See how many models have been estimated
length(reg.summary$rsq)
## 11

# Results for the residual-sum-of-squares (RSS)
x=seq(1:length(reg.summary$rsq))
x

# Plot RSS vs. # variables
plot(x, reg.summary$rss, xlab="Number of Variables ",
     ylab="RSS")

# Plot Adjusted R2 vs. # variables
plot(x, reg.summary$adjr2, xlab="Number of Variables ",
       ylab="Adjusted RSq")

# Identifying which model has the highest adjusted R-square
reg.summary$adjr2
max_adj_r2=which.max(reg.summary$adjr2)
max_adj_r2

# Putting a red dot on the model that maximizes the adjusted R-square
points(max_adj_r2,reg.summary$adjr2[max_adj_r2], col="red", pch=19)
# pch=19 is the code for solid dots

max_adj_r2
reg.summary$adjr2[max_adj_r2]
reg.summary$adjr2[7]

# Repeating the analysis before for CP
plot(x, reg.summary$cp,xlab="Number of Variables",ylab="Cp")
min_cp=which.min(reg.summary$cp)
points(min_cp,reg.summary$cp[min_cp],col="red", pch=19)

reg.summary$cp
min_cp
reg.summary$cp[min_cp]
reg.summary$cp[6]

# Repeating the analysis for BIC
min_BIC=which.min(reg.summary$bic)
plot(x, reg.summary$bic,xlab="Number of Variables",ylab="BIC")
points(min_BIC,reg.summary$bic[min_BIC],col="red", pch=19)

reg.summary$bic
min_BIC
reg.summary$bic[min_BIC]
reg.summary$bic[4]

# Report the coefficients for the best model with 4 regressors
coef(regfit.full,4)

####################################################################################
# (2) Forward and Backward Stepwise Selection
####################################################################################
## Adding method = "forward" or method = "backward" to regsubsets()
## to perform forward/backward stepwise selection

# Forward Stepwise Selection
regfit.fwd=regsubsets(Balance~.,data=Credit,nvmax=20, method="forward")
summary(regfit.fwd)

# Backward Stepwise Selection
regfit.bwd=regsubsets(Balance~.,data=Credit,nvmax=20, method="backward")
summary(regfit.bwd)

# the model selected may not be the same across various methods.
# here best subset selection is the same as backward selection
coef(regfit.full,4)
coef(regfit.fwd,4)
coef(regfit.bwd,4)

# the model selected may not be the same across various methods
# here best subset selection is the same as forward selection
coef(regfit.full,3)
coef(regfit.fwd,3)
coef(regfit.bwd,3)

####################################################################################
## (2.1) In-Class exercise: Forward Stepwise Selection ##
##################################################################
# First conduct the forward stepwise selection
# Then find the optimal number of regressors based on (a) adj R2 
# (b) Cp (b) BIC
###################################################################################
regfwd.summary = summary(regfit.fwd)
regfwd.summary

max_adj_r2=which.max(regfwd.summary$adjr2)
max_adj_r2

min_cp=which.min(regfwd.summary$cp)
min_cp

min_BIC=which.min(regfwd.summary$bic)
min_BIC

####################################################################################
## (2.2) In-Class exercise: Backward Stepwise Selection ##
##################################################################
# First conduct the backward stepwise selection
# Then find the optimal number of regressors based on (a) adj R2 
# (b) Cp (b) BIC
###################################################################################

regbwd.summary = summary(regfit.bwd)

max_adj_r2=which.max(regbwd.summary$adjr2)
max_adj_r2

min_cp=which.min(regbwd.summary$cp)
min_cp

min_BIC=which.min(regbwd.summary$bic)
min_BIC



### The END ###













