#############################################################
# Lec 10: Model Section: Cross-Validation
# Reference: ISLR v2 Chapter 6 pp.267-274

#############################################################
## Set up the work environment ##
#############################################################

# Clear the workspace
rm(list=ls())

# Set the working directory to the folder under which you have saved data files
#setwd("C:/data/0_Teaching/0_FINC_3103_ML/02_R_Codes")

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
# (3) Best Subset Selection Using the validation-set approach 
# and Cross-validation
####################################################################################

###########################################
# (3.1): Validation-set Approach
###########################################

# set seed (so we get the same output in each run)
set.seed(2)
## we are setting a seed here for illustration purposes
## in practice, you don't need to set seeds

# Randomly split the sample into two subsets
nrow(Credit)

train=sample(nrow(Credit),nrow(Credit)*3/4)
sort(train)

test=seq(1:nrow(Credit))[-train]
sort(test)

# Obtaining the best models for training data
regfit.best=regsubsets(Balance~.,data=Credit[train,],nvmax=20)
summary(regfit.best)

# Save the predictor values of test data in a matrix called "test.mat"
test.mat=model.matrix(Balance~.,data=Credit[test,])
dim(test.mat)
## model.matrix() extracts the values of predictors 
## a column of 1's is generated for the "intercept"

# Constructing validation errors for the best models of 
# each size
val.errors=rep(NA,11)
val.errors
## from previous tests, we know there are only 11 models estimated
## i.e. p=11

## Save the estimated coefficients of the 3rd model (best model with 3 predictors)
# in coefi
coefi=coef(regfit.best,id=3)
coefi

pred=test.mat[,names(coefi)]%*%coefi
# the operator %*% is the product of a matrix and a vector
# or for matrix multiplication
# find values of "named" variables and multiply them by 
# the corresponding coefficients

val.errors[3]=mean((Credit$Balance[test]-pred)^2)
val.errors

###########################################
# (3.1a): Introduction to Matrix operations
###########################################

#################################################
## Multiplication of matrix and a vector ##
#################################################
A = matrix( c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3, byrow=TRUE)
A
# A is a 2 by 3 matrix (2 row, 3 columns)

B = c(2,3,4)
B

C = A%*%B
C

# first element of C is the first row of A times B
# C[1]=1*2+2*3+3*4 = 2+6+12 = 20
C[1]

# 2nd element of C is the end row of A times B
# C[2]=4*2+5*3+6*4 = 8+15+24 = 47 
C[2]

#################################################
## Multiplication of two matrices ##
#################################################
D = matrix( c(1, 1, 1, 2, 2, 2), nrow=3, ncol=2)
# if byrow=TRUE is omiitted, the matrix layout will be by column
D
# D is a 3 (rows) by 2 (columns) matrix

## A%*%D ##
E = A%*%D
A
D
E

## first row first column of E is first row of A times first column of D
## E[1,1] = 1*1 + 2*1 + 3*1 = 6
E[1,1]

## first row 2nd column of E is first row of A times 2nd column of D
## E[1,1] = 1*2 + 2*2 + 3*2 = 12
E[1,2]

## 2nd row first column of E is 2nd row of A times first column of D
## E[2,1] = 4*1 + 5*1 + 6*1 = 15
E[2,1]

## 2nd row 2nd column of E is 2nd row of A times 2nd column of D
## E[2,2] = 4*2 + 5*2 + 6*2 = 30
E[2,2]

## note a vector of three elements (like B) can be viewed as a 
## 3 (rows) by 1 (column) matrix
###########################################
# (3.1b): Back to our validation set 
###########################################
## Now let's look at the 4th model (best model with 4 predictors)
coefi=coef(regfit.best,id=4)
coefi

pred=test.mat[,names(coefi)]%*%coefi
val.errors[4]=mean((Credit$Balance[test]-pred)^2)
val.errors

## Use a loop to find the test errors for Models 1-11 ##
for(i in 1:11){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Credit$Balance[test]-pred)^2)
}

val.errors

## plot val.errors vs. # variables ##
x=seq(1:11)
plot(x,val.errors)

# Identifying the minimum validation errors and reporting 
# the coefficients obtained from the "training" subset
min_val_errors=which.min(val.errors)
min_val_errors
coef(regfit.best,min_val_errors)

## add the min_val_errors point in the plot
points(min_val_errors,val.errors[min_val_errors],col="red", pch=19)
# 6-variable model is the best

# Last step: Re-estimate the best model with as many regressors 
# as determined by the validation set approach, using "all" data in Credit
# using "all" data (not just training subset) will get more accurate
# coefficient estimates

regfit.best=regsubsets(Balance~.,data=Credit,nvmax=20)
coef(regfit.best,min_val_errors)

##################################
## (3.1c) in-class exercise ##
##################################
## Q1: try set.seed(1) - what is the optimal number of variables?
## Q2: try other seeds yourself - do you always get the same result?

set.seed(1)

# Randomly split the sample into two subsets
nrow(Credit)

train=sample(nrow(Credit),nrow(Credit)*3/4)
sort(train)

test=seq(1:nrow(Credit))[-train]
sort(test)

# Obtaining the best models for training data
regfit.best=regsubsets(Balance~.,data=Credit[train,],nvmax=20)
summary(regfit.best)

# Save the predictor values of test data in a matrix called "test.mat"
test.mat=model.matrix(Balance~.,data=Credit[test,])
dim(test.mat)
## model.matrix() extracts the values of predictors 
## a column of 1's is generated for the "intercept"

## Use a loop to find the test errors for Models 1-11 ##
for(i in 1:11){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Credit$Balance[test]-pred)^2)
}

x=seq(1:11)
plot(x,val.errors)

val.errors
which.min(val.errors)


#####################################################
# (3.2):  Cross-Validation Approach
#####################################################

# Performing ten-fold cross-validation approach
k=10
k

n=nrow(Credit)
n

# Split the sample if 10 groups at random
set.seed(1)

folds=sample(1:k,n, replace=TRUE)
## draw n numbers from 1-10 (n>k here), "replace=TRUE" means we can draw on
## a number repeatedly.

# take a look at folds
folds

# look at the distribution of folds (hist() is histogram)
hist(folds)
hist(folds,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
# breaks=c(...) specifies the breakpoints

##############################################
## (3.2a) Sample and Matrix examples: In-Class Exercise
##############################################
## Try the codes below. Do you get different answers

# Sample Example
set.seed(1)

sample(1:5, 4, replace=FALSE)
sample(1:5, 4, replace=TRUE)

# Matrix Example
mdat = matrix(c(2,2,2,3,3,3), nrow=2, ncol=3, byrow=TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat
## mdat is a matrix with 2 rows, 3 columns
## all values are NA
## rows are named row1, row2
## columns are named c.1, c.2, c.3

######################################################
## Back to our CV task here
######################################################
# Prepare the matrix to store the results 
cv.errors=matrix(NA, nrow=k, ncol=11, dimnames=list(1:k,1:11))

# Do the analysis across cross-validation blocks
for(j in 1:k){
  
  # fit the best model using the training data (on one of the k-folds)
  best.fit=regsubsets(Balance~.,data=Credit[folds!=j,],nvmax=20)
  # folds!=j means folds not equal to j
  
  # Test the best model of each size found above on test data
  for(i in 1:11){
    coefi = coef(best.fit, id = i)
    test.mat = model.matrix(Balance ~ ., data = Credit[folds == j, ])
    pred = test.mat[, names(coefi)] %*% coefi
    
  # Compute mean-squared errors
    cv.errors[j,i]=mean((Credit$Balance[folds==j]-pred)^2)
  }
  print(j)
}

# Take a look at our output - What do the numbers in each row/column mean?
cv.errors

# Compute the average errors across cross-validation rounds
mean.cv.errors=apply(cv.errors,2,mean)
# apply the "mean" function to elements in the matrix "cv.errors"
# "1" here for all elements in a ROW; "2" for all elements in a Column
round(mean.cv.errors,0)

########################################################
## More on apply() functions: try the following ##
# min by column
apply(cv.errors,2,min)

# max by column
apply(cv.errors,2,max)
########################################################

best_model_cv=which.min(mean.cv.errors)
best_model_cv
# 6-variable model is the best

# Plot the performance for the models of various dimensions
plot(x, mean.cv.errors)

# add a point to where cv.errors is lowest
points(best_model_cv,mean.cv.errors[best_model_cv],col="red", pch=19)

# Plot the final model and assoicated coefficients
reg.best=regsubsets(Balance~.,data=Credit, nvmax=20)
coef(reg.best,best_model_cv)

##################################
## (3.2b) in-class exercise ##
##################################
## Q1: try set.seed(2) - what is the optimal number of variables?
## try other seeds yourself
## Q2: which method yields (validation set or 10-fold CV) more
## stable results? Validation set approach or 10-fold CV?

## Try the codes below. Do you get different answers

# Sample Example
set.seed(3)

sample(1:5, 4, replace=FALSE)
sample(1:5, 4, replace=TRUE)

# Matrix Example
mdat = matrix(c(2,2,2,3,3,3), nrow=2, ncol=3, byrow=TRUE,
              dimnames = list(c("row1", "row2"),
                              c("C.1", "C.2", "C.3")))
mdat
## mdat is a matrix with 2 rows, 3 columns
## all values are NA
## rows are named row1, row2
## columns are named c.1, c.2, c.3

######################################################
## Back to our CV task here
######################################################
# Prepare the matrix to store the results 
cv.errors=matrix(NA, nrow=k, ncol=11, dimnames=list(1:k,1:11))

# Do the analysis across cross-validation blocks
for(j in 1:k){
  
  # fit the best model using the training data (on one of the k-folds)
  best.fit=regsubsets(Balance~.,data=Credit[folds!=j,],nvmax=20)
  # folds!=j means folds not equal to j
  
  # Test the best model of each size found above on test data
  for(i in 1:11){
    coefi = coef(best.fit, id = i)
    test.mat = model.matrix(Balance ~ ., data = Credit[folds == j, ])
    pred = test.mat[, names(coefi)] %*% coefi
    
    # Compute mean-squared errors
    cv.errors[j,i]=mean((Credit$Balance[folds==j]-pred)^2)
  }
  print(j)
}

# Take a look at our output - What do the numbers in each row/column mean?
cv.errors

# Compute the average errors across cross-validation rounds
mean.cv.errors=apply(cv.errors,2,mean)
# apply the "mean" function to elements in the matrix "cv.errors"
# "1" here for all elements in a ROW; "2" for all elements in a Column
round(mean.cv.errors,0)

########################################################
## More on apply() functions: try the following ##
# min by column
apply(cv.errors,2,min)

# max by column
apply(cv.errors,2,max)
########################################################

best_model_cv=which.min(mean.cv.errors)
best_model_cv
# 6-variable model is the best

# Plot the performance for the models of various dimensions
plot(x, mean.cv.errors)

# add a point to where cv.errors is lowest
points(best_model_cv,mean.cv.errors[best_model_cv],col="red", pch=19)

# Plot the final model and assoicated coefficients
reg.best=regsubsets(Balance~.,data=Credit, nvmax=20)
coef(reg.best,best_model_cv)


### The END ###













