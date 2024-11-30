#############################################################
# Resampling
# Taken from ISLR Chapter 5 - Part 1 (Validation Set)
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

##################################################################################################
## (1) Validation Set Approach
##################################################################################################

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

#### set.seed ####
## Choosing a subset of the data as our training set ##
set.seed(2)
# set.seed(1) for random sampling (so we always get the same results)

train=sample(392,196)
# randomly choose 196 numbers from 1 to 392
# set seed, so the randomly selected set is the same in each run
# you can try by skipping the line "set.seed(1)" or try "set.seed(2)" instead

# let's sort "train" and see what are the chosen values
sort(train)

# The unchosen numbers will be used in our testing set
test=seq(1:392)[-train]
# remove numbers in the "train" subset from the sequence of 1 to 392

# sort and take a look at numbers chosen by the "test" subset
sort(test)

## divide the sample to train and test
Auto.train=Auto[train,]
# choose rows chosen by the "train" subset
# and all the columns associated with these rows

Auto.test=Auto[test,]
# choose rows chosen by the "test" subset
# and all the columns associated with these rows

#########################################################
## (1.1) Linear Model 
#########################################################
# Estimate the parameters on the training data 
lm.fit=lm(mpg ~ horsepower, data=Auto.train)

# Apply the estimated model (lm.fit) to the test data (Auto.test)
# and generate predicted values (predicted mpg)
predict.test=predict(lm.fit, Auto.test)

# Calculate the test error
error.test=Auto.test$mpg-predict.test
# use the "mpg column" in the Auto.test data frame to compare with predict.test

# generate a variable (mean.test.error) to store results 
mean.test.error=rep(NA,3)

# Computing the mean squared error (on the test data for a linear model)
# and save the results in the first element of mean.test.error
mean.test.error[1]=mean(error.test^2)
mean.test.error[1]
# 23.26601 (seed=1)
# 25.72651 (seed=2) in-class exercise solution

#########################################################
## (1.2) Quadratic Model
#########################################################
# Estimate the parameters on the training data 
lm.fit2=lm(mpg ~ horsepower+I(horsepower^2), data=Auto.train)

# Apply the estimated model to the test data
predict.test2=predict(lm.fit2, Auto.test)

# Calculate the test error
error.test2=Auto.test$mpg-predict.test2
# use the "mpg column" in the Auto.test data frame to compare with predict.test

# Computing the mean squared error (on the test data for a quadratic model)
# and save the results
mean.test.error[2]=mean(error.test2^2)
mean.test.error[2]
# 18.71646 (seed=1)
# 20.43036 (seed=2) in-class exercise solution

#########################################################
## (1.3) Cubic Model -- In-Class Exercise
#########################################################
# Estimate the parameters on the training data 
lm.fit3=lm(mpg ~ horsepower+I(horsepower^2)+I(horsepower^3), data=Auto.train)

# Apply the estimated model to the test data
predict.test3=predict(lm.fit3, Auto.test)

# Calculate the test error
error.test3=Auto.test$mpg-predict.test3


# Computing the mean squared error (on the test data for a cubic model)
# and save the results
mean.test.error[3]=mean(error.test3^2)
mean.test.error[3]
# Print results from all three fits, round to 4 decimals
round(mean.test.error,4)

# Q: which model yields the lowest mean.test.error?

##################################################################################################
## (1.4) Set a different seed (set.seed (2)) and repeat the exercise -- In-Class Exercise
##################################################################################################

## You can simply change set.seed(1) to set.seed(2) in the beginning of Section (1)
## and re-run all the commands in Sections (1.1) - (1.3)



###############################################################
## The END
###############################################################

