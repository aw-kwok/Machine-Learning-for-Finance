#############################################################
# Multiple Linear Regression codes - Part 2
# Taken from ISLR Chapter 3
#############################################################
## Set up the work environment ##

# Clearing the workspace
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

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
# (1) Interactions between quantitative variables - Advertising Dataset
#################################################################################################
#################################################################################################

# Loading the data
advertising_data=read.csv("Advertising.csv")

# drop the first column
advertising_data=advertising_data[,-1]

# Estimate the full Model (with interaction): TV and radio
model=summary(lm(sales~TV+radio+TV*radio,data=advertising_data))
model

# Estimate the Model without interaction: TV and radio
model_2=summary(lm(sales~TV+radio,data=advertising_data))
model_2

# Compare the coefficients from these two regressions
round(model_2$coefficients,4)
round(model$coefficients,4)

#################################################################################################
#################################################################################################
# (2) Interactions between quantitative and qualitative variables - Credit Dataset
#################################################################################################
#################################################################################################

# Clear the workspace 
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Loading the data
credit_data=read.csv("Credit.csv")
credit_data=credit_data[,-1]

####################################################################
## In-Class Exercise: Interactions between Student and Income ##
####################################################################
# (2.1) Convert Student to a factor variable

credit_data$Student = as.factor(credit_data$Student)
credit_data$Student

####################################################################
# (2.2) Estimating the model without interactions btw Student and Income
model_1 = lm(Balance ~ Student + Income, data = credit_data)
summary(model_1)

# Q: Do Students have higher or lower balances than non-students?
####################################################################
# (2.3) Estimating the model with interactions btw Student and Income
model_2 = lm(Balance ~ Student + Income + Student*Income, data = credit_data)
summary(model_2)


# Q(a): is the coefficient of StudentxIncome positive or negative?
# Q(b): based on the result, do balances increase less more with income for students?

####################################################################
# (2.4) Plot graphs

# fit the model with interaction between Income and Student
model=lm(Balance~Student+Income+Student*Income,data=credit_data)

# use the model to predict Balance
predict.balance=predict(model, credit_data)

# plot predicted Balance (y) against Income (x)
plot(credit_data$Income, predict.balance)

#################################################################################################
#################################################################################################
# (3) Non-linearities
#################################################################################################
#################################################################################################

# Clearing the workspace
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Call data libraries (which house datasets we may use later)
library(MASS)
library(ISLR) 
# if you don't have these library packages installed already
# install these two packages
# (1) click the "Packages" option in the lower right panel
# (2) click "install"
# (3) type the package name and click "Install"

# call Auto from the ISLR library to R's working memory
attach(Auto)
Auto=Auto

plot(horsepower, mpg)

###################################################
## (3.1) Polynomials of different degrees
###################################################

# (3.1.1) simple linear regression mpg against horsepower
lm.fit=lm(mpg~horsepower, data=Auto)
summary(lm.fit)

###################################################
# (3.1.2) Use polynomial y = x + x^2 up to 2nd power
###################################################
# NOTE: the following formulation seems intuitive
# but it does not work

#lm.fit2_wrong=lm(mpg~horsepower+horsepower^2,data=Auto)
#summary(lm.fit2_wrong)
## R reserves "^" in a function, such as lm(), for a special meaning
## Therefore, it does not recognize horsepower^2 inside lm() as the square of horsepower

# To get around this problem, we'll have to use I(horsepower^2) instead
# I() insulates the argument inside the (), i.e. telling R to 
# first calculate horsepower^2, and then treats (horsepower^2) as a variable

lm.fit2=lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(lm.fit2)


# Alternatively, generate a new variable for horsepower^2
# it gets the same results but has more steps
Auto$horsepower2=Auto$horsepower^2
lm.fit2a=lm(mpg~horsepower+horsepower2,data=Auto)
summary(lm.fit2a)

###############################################################
# (3.1.3) In-Class Exercise: y = x + x^2 + x^3 up to 3rd power
###############################################################
lm.fit3 = lm (mpg ~ horsepower + I(horsepower^2) + I(horsepower^3), data = Auto)
summary(lm.fit3)


###############################################################
## (3.1.4) In-Class Exercise: y = x + x^2 + x^3 + x^4 up to 4th power ##
###############################################################
lm.fit4 = lm (mpg ~ horsepower + I(horsepower^2) + I(horsepower^3) + I(horsepower^4), data = Auto)
summary(lm.fit4)

###############################################################
## (3.1.5) In-Class Exercise: y = x + x^2 + x^3 + x^4 + x^5 up to 5th power ##
###############################################################
lm.fit5 = lm (mpg ~ horsepower + I(horsepower^2) + I(horsepower^3) + I(horsepower^4) + I(horsepower^5), data = Auto)
summary(lm.fit5)

# Questions: 
# (1) How do the R2 and adj R2 change when we increase the power from 2 to 5?
# (2) Are higher power terms of horsepower significant at the 5% level
# in lm.fit3, lm.fit4, and lm.fit5
#########################################################################
#########################################################################
## (3.2) Plot the graph ##
#########################################################################
#########################################################################

plot(horsepower,mpg)
lines(horsepower,lm.fit$fitted.values,type="p", col="red", lwd=2)
# type "p" is point; col is color; lwd is line width

plot(horsepower,mpg)
lines(horsepower,lm.fit2$fitted.values,type="p",col="blue", lwd=2)

plot(horsepower,mpg)
lines(horsepower,lm.fit3$fitted.values,type="p",col="yellow", lwd=2)

plot(horsepower,mpg)
lines(horsepower,lm.fit4$fitted.values,type="p",col="pink", lwd=2)

plot(horsepower,mpg)
lines(horsepower,lm.fit5$fitted.values,type="p",col="green", lwd=2)



#### The END ####



