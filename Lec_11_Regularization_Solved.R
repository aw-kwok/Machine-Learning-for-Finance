################################################################################################
# Codes for Ridge regression and the LASSO
# This code contains extracts from Chapter 6 of ISLR v2
# You can find more details there. Pages 274-279.
################################################################################################

#############################################################
## Set up the work environment ##
#############################################################

# Clear the workspace
rm(list=ls())

# Set the working directory to the folder under which you have saved data files
#setwd("C:/data/0_Teaching/0_FINC_3103_ML/02_R_Codes")

# Loading the required packages
# Note: "glmnet" is new (we are using it for the first time)
library(ISLR)
library(leaps)
library(glmnet)

# if you don't have these library packages installed already
# install these packages
# (1) click the "Packages" option in the lower right panel
# (2) click "install"
# (3) type the package name and click "Install"

#############################################################
## Load and clean data we are about to use ##
#############################################################

attach(Hitters)
Hitters=Hitters

# find information about this dataset
?Hitters

# Displaying the name of the variables for the Hitters dataset
names(Hitters)

# Find the dimensions of the dataset
dim(Hitters)
## we have 322 obs and 20 variables

# Computing the number of players for which we do not have a salary
sum(is.na(Hitters$Salary))
# is.na() checks whether some values are missing for a specific variable
# 59 obs do not have a value for "Salary"

# Omit obs whose "Salary" has no value
# (otherwise we may see error messages later)
Hitters=na.omit(Hitters)

# Re-computing the dimensions of the dataset
dim(Hitters)
## now we have only 263 obs

# Making sure that all players in the data have salaries
sum(is.na(Hitters$Salary))
# now there is 0 obs with no Salary data 
# we are good to go!

################################################################################################
## (1) Ridge Regression
################################################################################################
## (1.1) Generate the x and y matrix/vector for glmnet()
###########################################################
# Creating the matrix of regressors (without the intercept)
# and the vector of dependent variables for later use in glmnet()
# glmnet() has a different syntax than lm()

# generate the x matrix
x=model.matrix(Salary~.,data=Hitters)

# drop the "intercept" column (the first column)
# from the x matrix (for later use in glmnet)
# glmnet() does not need the column of 1's for the intercept
x=x[,-1]

# generate the y-vector for the outcome variable
y=Hitters$Salary

###########################################################
## (1.2) Use glmnet() to perform Ridge regression
###########################################################

# Set the grid (a range of values) for lambda
# we choose 100 values between 10^10 and 10^(-2)
grid=10^seq(10,-2,length=100)

round(seq(10,-2,length=100),4)
## seq(10,-2,length=100) gives us 100 values, equally spaced between
## 10 and -2

grid

############################################################
## (1.2a) use glmnet() to perform ridge regression ##
############################################################

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
## Alpha=0 is for RIDGE
## Alpha=1 is for LASSO in glmnet()
## glmnet() does standardization of predictor variables (x's) for us

# We can look at the coefficients using the plot() function
plot(ridge.mod, xvar="lambda")

# The procedure returns a vector of 20 coefficients for each value of the 
# penalty grid
dim(coef(ridge.mod))
# 20 100
# There are 20 rows (coef estimates for each of the 20 variables) 
# and 100 columns (100 lambda's)

###########################################################
## (1.2b) let's look the relation between lambda and coef's
###########################################################
## let's look at the 1st column of coef(ridge.mod)
# (a huge lambda: 10^10)
ridge.mod$lambda[1]
#1e+10 (10^10)
# this is the biggest value of tested lambda values (biggest in "grid")

coef(ridge.mod)[,1]
# all rows (20 rows for 20 variables) and 1st column
# almost all coef's are close to zero (very small)

## next we look at the last column of coef(ridge.mod)
## (a very small lambda: 10^-2 = 0.01)
ridge.mod$lambda[100]
# 0.01 (10^-2)
# this is the smallest value of tested lambda values (smallest in "grid")

coef(ridge.mod)[,100]
# the coef's are now bigger

## how about OLS coef's?
coefficients(lm(y~x))
# not same as lambda=0.01, but closer in magnitude

###########################################################
## (1.2c) Introduce the predict() function in glmnet()
###########################################################
# Other than predicting y, we can also use the predict() in glmnet()
# to find coefficients for a lambda that is outside our grid

## for example, find coefficients associated with lambda = 200 ##
predict(ridge.mod, s=200, type="coefficients")[1:20,]
# s=200 means the actual lambda value=200 
# type="coefficients" means the output is coef's for the specified lambda

############################################################################
## (1.3) Use the validation set approach to choose the optimal lambda
############################################################################

# Setting the seed (for random sampling later)
set.seed(1)
## again set.seed here is for teaching purposes (so we always get the same outputs)
## no need to set.seed in practice

# Separating the data into training and test datasets
nrow(x)

train=sample(1:nrow(x), nrow(x)/2)
## randomly pick half of the obs

test=seq(1:nrow(x))[-train]
## obs whose row numbers not in "train" are in the "test" set

sort(train)
sort(test)

y.test=y[test]

# Fit the ridge regression model using the training data
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)

## "thresh" is the convergence threshold, telling the algorithm 
## when to stop searching for a solution 
## i.e., when the incremental change is less than the threshold
## the program will stop searching for a better solution
## the default value for "thresh" is 1e-7

############################################################################
## (1.3.1) Look at the test errors of a few randomly selected lamda's
############################################################################

# test the lambda=4 model #
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
mean((y.test-ridge.pred)^2)
# 142199.2

# try a larger lambda: lambda=100 #
ridge.pred = predict(ridge.mod, s=100, newx=x[test,])
mean((y.test-ridge.pred)^2)
# 143554 (MSE > s=4)

# try OLS (lambda=0) #
ridge.pred = predict(ridge.mod, s=0, newx=x[test,])
mean((y.test-ridge.pred)^2)
# 167789.8 (MSE > s=4)

############################################################################
## (1.3.2) Use a loop to find the lowest MSE on the test data set ##
############################################################################

# generate a variable val.errors to store test results
val.errors=rep(NA,100)

# Use a loop to find MSE associated with lambda values 
# in our grid

for(i in 1:100){
  ridge.pred = predict(ridge.mod, s=grid[i], newx=x[test,])
  val.errors[i]=mean((y.test-ridge.pred)^2)
}

min_val_errors=which.min(val.errors)
val.errors
min_val_errors
#59

## let's look at the optimal lambda and its MSE
bestlam=ridge.mod$lambda[min_val_errors]
bestlam
# 932.6033

val.errors[min_val_errors]
# 136816.9

grid[59]
#932.6033

val.errors[59]
# 136816.9 (better than s=4)

# make a plot of lambda vs test error
plot(log(grid),val.errors)
points(log(bestlam),val.errors[min_val_errors],col="red", pch=19)
log(bestlam)
# 6.83798

# the corresponding coef's (estimated on the training data)
predict(ridge.mod, s=bestlam, type="coefficients")[1:20,]

############################################################################
## (1.3.3) Finally, use the "full" sample to estimate the coefficients
## with the optimal lambda
############################################################################

out=glmnet(x,y,alpha=0,lambda=grid)
predict(out, s=bestlam, type="coefficients")[1:20,]

############################################################################
## (1.4) Use the built-in cv function: glmnet() to find best lambda
############################################################################

# Setting the seed
set.seed(1)
# the seed will affect the k-fold split later in cv.glmnet()

# cv.glmnet() to conduct k-fold CV (default: k=10)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
# the default in cv.glmnet() is 10-fold CV

# Plotting the resuls 
plot(cv.out)

# Computing the optimal lambda and displaying it
bestlam=cv.out$lambda.min
bestlam
# 326.0828

log(bestlam)
# 5.78

# Test the best model chosen by cv.glmnet 
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])

# test the out-of-sample performance of the cross-validated Ridge Regression
mean((y.test-ridge.pred)^2)
# 139856.6

# Finally, let's re-estimate the ridge regression on the "full" dataset 
# and report the shrunk coefficient estimates.
out=glmnet(x,y,alpha=0,lambda=grid)
predict(out,type="coefficients",s=bestlam)[1:20,]

## as we can see, none of the variables is zero
## Ridge does not perform variable selection

# Compare the coefficients on OLS estimated on the full data
lm(y~x)

################################################################################################
## (2) Lasso Regression
################################################################################################

# Doing the analysis using LASSO rather than Ridge regression
# Notice that the alpha is 1 and not zero

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
## Alpha=0 is for RIDGE
## Alpha=1 is for LASSO in glmnet()
## glmnet() does standardization of predictor variables (x's) for us


# plotting lasso as a function of the shrinkage parameter
plot(lasso.mod, xvar="lambda")

# Compute cross-validation to determine the optimal lambda
set.seed(1)
# the seed will affect the k-fold split later in cv.glmnet()

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

# Reporting the optimal lambda 
bestlam=cv.out$lambda.min
bestlam
# 9.286955

log(bestlam)
#2.2286

# Predicing the test sample observations using the optimal lambda
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

# Computing the mean-squared error out of sample 
mean((y.test-lasso.pred)^2)
# 143673.6

# Finally estimate the final model on the full sample 
out=glmnet(x,y,alpha=1,lambda=grid)

# Reporting the lasso coefficients 
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]

# Look at all coef's
lasso.coef
# some coef's are zero 
# Lasso performs variable selection!

# Look at coef's that are not zero
round(lasso.coef[lasso.coef!=0],2)
# Q: how many predictors have a non-zero estimated beta
# (not including the intercept)?


#### The END ####
