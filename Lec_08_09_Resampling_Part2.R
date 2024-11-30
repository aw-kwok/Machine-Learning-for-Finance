#############################################################
# Resampling
# Taken from ISLR Chapter 5 - Part 2 (LOOCV)
#############################################################
## Set up the work environment ##
#############################################################

# Clearing the workspace
rm(list=ls())

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Loading the library needed
# Note: "boot" is new (we are using it for the first time)
library(ISLR)
library(boot)

# if you don't have these library packages installed already
# install these two packages
# (1) click the "Packages" option in the lower right panel
# (2) click "install"
# (3) type the package name and click "Install"

## call Auto from the ISLR library to R's working memory ##
attach(Auto)
Auto=Auto

# look at our data
# View(Auto)
dim(Auto)
#392 rows and 9 columns

dim(Auto)[1]
# if we want to extract the row number of Auto

nrow(Auto)
# alternatively, we can use nrow(Auto) to find the number of rows in the Auto dataset

# label rows (by number), so we can trace them later
rownames(Auto)=1:nrow(Auto)

##################################################################################################
# (2) "Leave-One-Out Cross-Validation" (LOOCV)
##################################################################################################

#################################################
## (2.1) Background: glm() function and cv.glm()
#################################################
## The "boot" library in R has the cv.glm() function that conducts
## cross-validation tests for regressions
## However, we have to use glm() to conduct regressions, instead of lm()
## in order to use cv.glm()

## The function glm() gives the same output as lm(), but is more general
lm.fit=lm(mpg ~ horsepower, data=Auto)
coef(lm.fit)

glm.fit=glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)
# The default regression in glm() is linear regression, but glm() can
# also do other types of regressions, such as logistic regression (output 0 and 1)
# if interested, check out this page: https://bookdown.org/introrbook/intro2r/glm-function-for-regression.html

#################################################
## (2.2) Conduct LOOCV for the linear model
#################################################

# Estimate the linear model
glm.fit=glm(mpg ~ horsepower, data=Auto)

# Use the cv.glm() to conduct LOOCV using the Auto dataset and the glm.fit model
# and save the results in cv.err
cv.err=cv.glm(Auto, glm.fit)

# The "delta" column in cv.err saves the MSE for CV
cv.err$delta
## The 1st value is the raw MSE, which we will focus on in this class
## the 2nd value is adjusted MSE, which we will skip

# Report/view only the raw MSE of the CV (the 1st element of cv.err$delta)
cv.err$delta[1]

#################################################
## (2.3) Introduction to the poly() function
#################################################

## (2.3.1) Quadratic 

# option 1: type out all the terms
test.fit1=summary(lm(mpg ~ horsepower+I(horsepower^2), data=Auto))
test.fit1

# option 2: use the poly() function
test.fit2=summary(lm(mpg~poly(horsepower,2,raw=TRUE),data=Auto))
test.fit2

# if we drop the "raw=TRUE" inside poly()
test.fit3=summary(lm(mpg~poly(horsepower,2),data=Auto))
test.fit3
# Note: If we skip "raw=TRUE", the coef's will be different. 
# This is b/c R orthogonalizes poly terms.
# "orthogonalize" means removing correlations
# but the R-squared or predicted values remain the same, 
# i.e. essentially the same model as "raw=TRUE"

## (2.3.2) Cube 
test.fit4=summary(lm(mpg ~ horsepower+I(horsepower^2)+I(horsepower^3), data=Auto))
test.fit4

test.fit5=summary(lm(mpg~poly(horsepower,3,raw=TRUE),data=Auto))
test.fit5

## Later we will use the poly() function to conduct non-linear regressions ##

#################################################
## (2.4) Review of "Loops"
#################################################
## Loop is a very important and useful tool in coding

## Here is a simple loop example - print 1 to 10 ##

for (i in 1:10){
  print(i)
  }

#################################################
## (2.4.1) Loop: In-Class Exercise
#################################################
## print 10, 20, 30,...,90, 100

for (i in 1:10) {
  print(i*10)
}


###############################################################
## (2.5) Conduct LOOCV for polynomial model up to tenth order
###############################################################

# Generate a vector with 10 values to store CV outputs (1 to 10th order poly)
cv.error=rep(NA,10)
# rep(NA,10) repeats NA 10 times
# so we have generated a vector that houses 10 values
# current the values are all 0 (temporary)
# later on we will replace these zeros with the CV outputs

###############################################################
## (2.5.1) Without using a loop: up to 3rd power
###############################################################

# 1st order polynomial (linear)
glm.fit=glm(mpg ~ poly(horsepower,1),data=Auto)
cv.error[1]=cv.glm(Auto,glm.fit)$delta[1]
cv.error[1]

# 2nd order polynomial (quadratic)
glm.fit=glm(mpg ~ poly(horsepower,2),data=Auto)
# we also use glm.fit here to store the results
# so it will over-write the output of the previous linear model
# this is ok, b/c we are only interested in the output in the next step

cv.error[2]=cv.glm(Auto,glm.fit)$delta[1]
cv.error[2]
# here we want to save the CV MSE in "element 2" of cv.error

######################################################
# 3rd order polynomial (cubic): In-Class Exercise
######################################################
glm.fit=glm(mpg ~ poly(horsepower,3), data=Auto)

###############################################################
## (2.5.2) Using a loop: up to 10th power - In-Class Exercise
###############################################################

for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

# view the output vector (all 10 values)
round(cv.error,4)

#################################################
## plot the cross-validation error
#################################################
# first generate a vector for the x-axis
x=seq(1:10)
# generate a vector x that is a sequence of 1,2,...,10
## note: both rep() and seq() are very useful functions in R

# make the plot
plot(x,cv.error)


## find the lowest MSE
min(cv.error)

# use the which() function to find the order of polynomial that yields the lowest MSE
which(cv.error==min(cv.error))

# which() returns the position of the element that satisfies
# the relation, a very convenient function in R.
# here the answer is 7, so the 7th order polynomial yields the lowest
# mean test error, 
# although the mean test error has not changed much after the 2nd order

###############################################################
## The END
###############################################################

