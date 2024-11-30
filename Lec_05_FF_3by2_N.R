###########################################################################
# This Code Computes CAPM regressions for the 2 by 3 size and             #
# book-to-market fama-french portfolios                                   #
#                                                                         #
# Author: Alberto Rossi and Claire Liang                                                  #
###########################################################################
### Part 1: FF_3by2: 1st portfolio ##
######################################################
# (1) Import data #
######################################################

rm(list=ls())                                       
# Clearing the workspace

setwd("/Users/andrewkwok/Documents/FINC_3103")
# Set the working directory to the folder under which you have saved the data
# we will use here ("FF_3by2.csv")

# Loading monthly data for portfolio returns
ff <- read.csv("FF_3by2.csv")

# Extract the portfolio returns under consideration from ff
port_ret<-ff[,2:7]
# port_ret includes all rows and columns 2-7

# Extract the vector of market excess returns from ff
mkt_exc<-ff[,8]

# Extract the risk-free rate vector from ff
rf<-ff[,9]

# Constructing excess returns for the portfolios under consideration
port_exc_ret<-port_ret-rf
# subtract rf from every column

# Obtaining the number of observations for the market returns
n<-length(mkt_exc)
# mkt_exc is a string of values

n
# see the value of n

dim(port_exc_ret)
# port_exc_ret is a panel/matrix
# output: [1] 1035    6 (a panel/matrix with 1035 rows and 6 columns)
# Alternatively, n<-dim(port_exc_ret)[1], the first output of "dim(port_exc_ret)"

############################################################
#(2) Conducting the OLS analysis on the first portfolio #
# (SMALL and Low Book-to-Market) #
############################################################

results=summary(lm(port_exc_ret[,1]~mkt_exc))
# port_exc_ret[,1] means cells in all rows, 1st column
# lm(port_exc_ret[,1]~mkt_exc): regress port_exc_ret[,1] o mkt_exc
# 1st column is smlo (small, low bm)
# results stores the main output of the regression

results
# view the results

plot(mkt_exc,port_exc_ret[,1])
# do a plot of mkt_exc (horizontal axis) vs. port_exc_ret[,1] (vertical axis)

# print/view "coefficients" as part of results
print(results$coefficients)

# another way to view coefficients
results$coefficients

class(results$coefficients)
# the data type of results$coefficients is a matrix/array

dim(results$coefficients)
# it's a matrix with two rows and 4 columns

############################################################
# (3) Reporting alpha (intercept) and relevant statistics #
############################################################

results$coefficients

# view the coefficient estimate of alpha (intercept)
print(results$coefficients[1,1])
# extract the element in the 1st row and 1st column

# Reporting the t-statistic
print(results$coefficients[1,3])
# extract the element in the 1st row and 3rd column

# Reporting the p-value
print(results$coefficients[1,4])
# extract the element in the 1st row and 4th column

# or simply
results$coefficients[1,4]

# Computing and reporting the confidence interval for the first portfolio
conf_int=confint(lm(port_exc_ret[,1]~mkt_exc),level=0.95)
# generate 95% confidence interval and save the results in "conf_int"

print (conf_int)
# take a look at conf_int

print(conf_int[1,])
# 1st row, all columns of conf_int
# i.e. 2.5% and 97.5% values for the intercept

############################################################
# (4) Let's generate these test statistics ourselves #
############################################################
# null: alpha (the intercept) = 0 #

results$coefficients

# first verify that [1,1] is alpha and [1,2] is std error
results$coefficients[1,1]
results$coefficients[1,2]

# calculate the t-stat relative to the coef=0
tstat_alpha = (results$coefficients[1,1] - 0)/results$coefficients[1,2]
tstat_alpha

# find the p-value for a two-tail test

dim(results$coefficients)
# find the row and column numbers of the results$coefficients matrix
# 2 4 (2 rows, 4 columns)

results$coefficients
# 2 rows, 4 columns
# the row number is k (number of x variables, including intercept)

k=dim(results$coefficients)[1]
# extract the 1st element of [2 4], i.e. the row number, which is k
# k is the number of regressors in our linear regression model (including intercept)
k

df = n-k
df
# df is the degree of freedom n-k (number of obs - number of regressors)

pvalue_alpha = pt(tstat_alpha, df, lower.tail=TRUE)*2
pvalue_alpha
# pt() is return the probability cumulative density of the Student t-distribution
# (i.e. the area of the bell curve up to t)
# since our t-stat is negative, we want to look at the lower tail
# "*2" b/c it's a two-tail test

# find the threshold q for 95% confidence interval
q = qt(p = 0.025, df = df)
q
# qt() gives us the critical value of t in R for cumulative prob (p)=0.025
# note: q is negative here (-1.9622)

alpha_conf_int_low = results$coefficients[1,1]-abs(q)*results$coefficients[1,2]
alpha_conf_int_high = results$coefficients[1,1]+abs(q)*results$coefficients[1,2]

alpha_conf_int_low
alpha_conf_int_high
conf_int

## note: the t-stat and p-value reported by R is testing against 
## the null hypothesis of the coef=0

#######################################################################
### Part 2: In-class exercise ###
#######################################################################

# (1) find the coef for mkt_exc (this is beta)
# (2) find the t-stat, p-value, conf int of beta from results generated
# by R (against a default null H0: beta=0)
# (3) Manually calculate the t-stat and p-value for the null hypothesis H0: beta=1

#### (4)-(5) are hw #####
# (4) HW: repeat what we have done so far for the 2nd portfolio (smme)
# (5) HW: use a loop to automate the task for all 6 portfolios

#########################################################
## (1) the value of beta ##
#########################################################

results$coefficients[2,1]

##############################################################
## (2) t-stat, p-value, conf int for beta (null: beta=1) ##
## for the 1st portfolio ## 
##############################################################



#########################################################
## (3) t-stat and p-value for null: beta=1
#########################################################



#########################################################
## (4) repeat the steps above for 2nd pf (smme)
# Conducting the OLS analysis on the first portfolio 
# (SMALL and median Book-to-Market)
#########################################################



#######################################################################
## (5) Use a loop to find the estimates for all the 6 portfolios ##
#######################################################################




##### The END #####

