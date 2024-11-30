## Set up the work environment ##

# Clearing the workspace
rm(list=ls())                                       

# Set the working directory to the folder under which you have saved data files
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Loading the library needed
# Note: "boot" is new (we are using it for the first time)
library(ISLR)
library(boot)

attach(Auto)
Auto=Auto

#Q6
cv.error = rep(NA, 10)

set.seed(1)

for (i in 1:10){
  glm.fit = glm(horsepower ~ poly(weight, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit, K=5)$delta[1]
}

cv.error
which.min(cv.error)

#Q7
q7_data = read.csv("Quiz_02_Q7.csv")

set.seed(10)
train=sample(100, 80)

sort(train)

test=seq(1:100)[-train]

sort(test)

q7.train = q7_data[train,]
q7.test = q7_data[test,]

lm.fit = lm(y ~ x, data=q7.train)
summary(lm.fit)

predict.test = predict(lm.fit, q7.test)

error.test = q7.test$y - predict.test

mean(error.test^2)