#############################################################
## Solution to In Class Exercise ##
#############################################################
# Set up the workspace
#############################################################
# Clear the workspace 
rm(list=ls())                                       

library(boot)

#############################################################
# (0) Generate the simulated data
#############################################################

## Y=1*X-2X^2+epsilon ##
# Generate a simulated data set as follows:
set.seed(1)
x=rnorm(100, mean=0, sd=1)   
y=x -2*x^2 +rnorm(100, mean=0, sd=1)

# rnorm(100, mean=0, sd=1) generates 100 random variables that follow a normal distribution
# with mean=0 and stdev=1

# Create a scatterplot of X against Y. 
plot(x,y)

# Set a random seed, and then compute the LOOCV errors for the various models
data=data.frame(y,x)

#############################################################
# (1) LOOCV: In-class exercise
#######################################################################
# (1.1) In-Class Exercise: 
# Set a seed and then estimate polynomial models up to 4th order
# and conduct LOOCV
#######################################################################

set.seed(34)
res=rep(NA,4)

for(i in 1:4) {
  glm.fit=glm(y ~ poly(x,i),data=data)
  res[i]=cv.glm(data,glm.fit)$delta[1]
}
round(res,4)
which(res==min(res))

# Q(a): What is the lowest mean test error from these 4 models?
# Q(b): Which of the models has the smallest LOOCV error? Is this what you expected? 

# A: The model with the quadratic term only is the best. It is expected given how 
# the data is generated

##############################################################
## (1.2) In-Class Exercise:
## Set a different seed and repeat what you did in (1.1)
##############################################################

## set a different seed 
set.seed(43)
res2=rep(NA,4)


## estimate using LOOCV


## compare results from both tries


# Q: Do you get the same results as in (1.1)? why?

##############################################################
## (1.3) Examine fitted models - in-class exercise
##############################################################

for(i in 1:4) {
  print(summary(lm(y~poly(x,i,raw=TRUE),data = data)))
}


# Q: Comment on the statistical significance of the coefficient estimates
# that results from fitting each of the models using 
# least squares. 

# Q: Do these results agree with the conclusions drawn 
# based on the cross-validation results?
  
#############################################################
# (2) K-fold CV: in-Class exercise
#############################################################
# (2.1) Now try K-fold CV - in-class exercise
# set.seed(1) and then estimate polynomial models up to 4th order
# K=10
#######################################################################

set.seed(1)
res=rep(NA,4)

for(i in 1:4) {
  glm.fit=glm(y ~ poly(x,i),data=data)
  res[i]=cv.glm(data,glm.fit,K=10)$delta[1]
}

round(res,4)
which(res==min(res))

##############################################################
## (2.2) In-Class Exercise:
## Now set.seed(50) and repeat what you did in (2.1)
##############################################################

set.seed(50)
res=rep(NA,4)

for(i in 1:4) {
  glm.fit=glm(y ~ poly(x,i),data=data)
  res[i]=cv.glm(data,glm.fit,K=10)$delta[1]
}

round(res,4)
which(res==min(res))


# Q: do you get the same results from both tries? Why or why not?

####################################################
## The END ##
####################################################

