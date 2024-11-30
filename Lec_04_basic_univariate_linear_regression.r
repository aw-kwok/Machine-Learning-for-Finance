###########################################################################
# This code simulates data and computes ols coefficients

# Author: Alberto Rossi & Claire Liang
###########################################################################
# Clearing the workspace

rm(list=ls())                                 

# rm() removes objects in the workspace
# ls() lists all of the objects in the workspace
# rm(list=ls()) means removing all objects currently in the workspace
###########################################################################

###########################################################################
## Part 1: In-class Exercise - Linear Regression with the XYZ fund example ##
###########################################################################
# (1) read data from computer (pls save data in your laptop first) #


setwd("/Users/andrewkwok/Documents/FINC_3103")
# "setwd": set working directory - tell R where the file is saved
# note that in R the backslash "\" is replaced by the forwardslash"/" (a bit weird...)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# This R command automatically sets the working directory to the R file's directory. 
# This way, you can put the R file and data file in the same folder and R should be able to read in the data. 
# It works for PC. Hopefully it will work on Mac.
# Many thanks to Will Nemoy for making this helpful suggestion!
# When you first open this file, RStudio will ask you to download a package "rstudioapi"
# Please download the package(s) RStudio suggests
# Many thanks to Will Nemoy is Sec 32 for making this helpful suggestion

xyz = read.csv("Lec_04_Linear_Regression_Excel_XYZ_csv.csv")
# load the XYZ data (in csv format) onto R
# the csv format looks like excel but it has a simpler structure than xls/xlsx
# the csv format is often used by data providers

View(xyz)
# take a look at the data 
# or simply click "xyz" under the "Data" panel
# the data will open a new window

xyz
# another way to see xyz (xyz will show in the output concole) 

xyz$x
# look at x (market excess return) only

xyz$y
# look at y (xyz excess return) only
# this is how you tell R to read x (or y) values from the dataframe xyz

xyz.fit=lm (y~x, data = xyz)
# tell R to use the x and y values from the xyz data set we just loaded
# the results are saved in xyz.fit

summary(xyz.fit)
# print the summary of regression

###########################################################################
### Part 2: In-home exercise: Simulate data for linear regression ###
###########################################################################
# Clearing the workspace
rm(list=ls())  

###########################################################################
# (1) Generate simulated data #

# Setting the number of observations
obs=10000                                

# set a seed for the random number (so we always get the same results)
set.seed(1)

# Generating values for the regressor (x)
x=rnorm(obs, mean=0, sd=4)          
# rnorm() generates random variables that follow a normal distribution with mean 0 and stdev 4)
# sd=4 here are for x (different from for epsilon below)

###########################################################################
# Set the parameters for the OLS regression
alpha=1                                  # Setting the alpha coefficient
beta=3                                   # Setting the beta coefficient
standard_deviation=5                     # Setting the st.dev of the errors epsilon
###########################################################################
# Generating the errors (epsilon)
set.seed(100)
epsilon=rnorm(obs,mean=0,sd=standard_deviation)          

# Generating the y series under the truth.
y=-alpha+x*beta+epsilon                       
plot(x,y)

length(x)
length(y)
# length(x) gives you the length of the string x 
# (same as the # of obs of x; this is "T" on p.15 of Lec_04 slides)
# same for length(y); here we have 10,000 obs for both x and y

###########################################################################
## Look at the following at home yourself ##
## (best to wait until after you've completed DataCamp Assignment 2)
###########################################################################
# (2) Calculate alpha (a_est) and beta (b_est) #

# Obtaining OLS estimates
numerator=sum(x*y)-length(y)*mean(x)*mean(y)  # Computing the beta numerator
denominator=sum(x^2)-length(y)*mean(x)^2      # Computing the beta denominator
b_est=numerator/denominator                   # Computing the beta coefficient
a_est=mean(y)-b_est*mean(x)                   # Computing the alpha coefficient

###########################################################################
# (3) Plotting the Results #

plot (x,y)
lines(x,(a_est+ x*b_est), col="red") 
# Plot the fitted values of the regression coefficient 
# by adding the fitted line

plot (x,y)
lines(x,(a_est+ x*b_est), col="blue", lty="dashed", lwd=4) 
# if you want to play with line styles, e.g. "solid", "dashed", "dotted"
# or line width (lwd) 

###########################################################################
# (4) Provide estimates of the parameters #

# a simple way to see our alpha and beta estimates
a_est
b_est

# a more sophisticated way to print/show our results
estimates = c(a_est, b_est)                    
# Defining a row vector with the estimates of alpha and beta

names(estimates) = c("alpha","beta")           
# Label variables in the row vector

print (estimates)
# print a_est and b_est

########################################################
# Computing the standard error for alpha and beta

residuals = y-a_est-b_est*x                     
# Computing the vector of residuals

s_squared = (1/(length(y)-2))*sum(residuals^2)  
# Computing the estimate of the residuals' variance

sum_x_squared = sum(x^2)                        
# Computing the numerator for the alpha standard error estimate

sum_x_minus_xbar = sum((x-mean(x))^2)           
# Computing the denominator for the alpha and beta standard error estimate

se_alpha = sqrt(s_squared)*
  (sqrt((1/length(y))*sum_x_squared)/
     sqrt(sum_x_minus_xbar))
se_beta = sqrt(s_squared)*(1/sqrt(sum_x_minus_xbar))

standard_errors = c(se_alpha,  se_beta)        
# Defining a row vector with the standard error estimates of alpha and beta

names(standard_errors) = c("SE_alpha","SE_beta")
# Label variables in "standard_errors"

print (estimates)
print (standard_errors)

###########################################################################
# (5) Use the lm function in R to conduct linear regression (linear model)

lm.fit = lm(y ~ x)
summary(lm.fit)
# lm() functions yields the OLS estimates, the same as what we
# manually calculated above.


######## The END ##########


