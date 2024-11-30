#############################################################
# Resampling
# Taken from ISLR Chapter 5 - Part 3 (k-fold CV)
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
## (3) "k-fold Cross-Validation" (K=10 here)
##################################################################################################

##################################################################################################
## (3.1) No loop: 10-fold CV (polynomial degree 1 to 3)
##################################################################################################

# generate a vector to store cv outputs
cv.error.10=rep(NA,10)

# set seed so we can get the same results every time (for teaching purposes)
set.seed(17)
## Note: In general, there is no need to set seed in practice
## (unless you want the same results in each run)

# 1st order
glm.fit=glm(mpg ~ poly(horsepower,1), data=Auto)
cv.error.10[1]=cv.glm(Auto,glm.fit,K=10)$delta[1]
## It turns out that we can just add the option "K=10" to cv.glm()
## for 10-fold CV (note: "K" is in upper case)
## in other words, the default is just LOOCV
print(cv.error.10[1])

# 2nd order
glm.fit=glm(mpg ~ poly(horsepower,2), data=Auto)
cv.error.10[2]=cv.glm(Auto,glm.fit,K=10)$delta[1]
print(cv.error.10[2])

####################################################
# 3rd order: In-Class Exercise
####################################################
glm.fit=glm(mpg ~ poly(horsepower,3), data=Auto)
cv.error.10[3]=cv.glm(Auto,glm.fit,K=10)$delta[1]
print(cv.error.10[3])


##################################################################################################
# (3.2) Loop: 10-fold CV for polynomial model up to 10th order - In-Class Exercise
##################################################################################################

cv.error.10=rep(NA,10)
set.seed(17)

## use a loop to conduct K-fold CV (K=10) for polynomial order 1 to 10
for (i in 1:10){
  glm.fit=glm(mpg ~ poly(horsepower,i), data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

## print the K-fold CV results
round(cv.error.10, 4)

## Plot a graph
x=seq(1:10)
plot(x,cv.error.10)

## Identify the order of polynomial that yields the lowest MSE
min(cv.error.10)
which(cv.error.10==min(cv.error.10))



###############################################################
## The END
###############################################################

