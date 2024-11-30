# README
# Class: FINC 3103-34
# Author: Andrew Kwok (awk42)

# Each question shares its variables internally (each question is meant to be run sequentially)

# ------------------------------Q1-----------------------------------
# Clearing the workspace
rm(list=ls())

# Set working directory
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Read file into variable data
market_data = read.csv("Project_1_Q1_bm_10_ew.csv")

# Q1(a)
# read s1 portfolio returns
s1 = market_data$s1

# read market risk premium and risk free rate
mrp = market_data$Mkt.RF
rf = market_data$RF

x = mrp
y = s1 - rf

# native lm = for reference
model = lm(y ~ x, data = market_data)
summary(model)

#Q1(a)(i) Obtaining OLS estimates
numerator=sum(x*y)-length(y)*mean(x)*mean(y)  # Computing the beta numerator
denominator=sum(x^2)-length(y)*mean(x)^2      # Computing the beta denominator
b_est=numerator/denominator                   # Computing the beta coefficient
a_est=mean(y)-b_est*mean(x)                   # Computing the alpha coefficient

a_est
b_est

#Q1(a)(ii) Calculate standard error
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

# Defining a row vector with the standard error estimates of alpha and beta
standard_errors = c(se_alpha,  se_beta)        

# Label variables in "standard_errors"
names(standard_errors) = c("SE_alpha","SE_beta")

print (standard_errors)

#Q1(a)(iii) Calculate t value
# calculate the t-stat relative to the coef=0
tstat_alpha = (a_est - 0)/se_alpha
tstat_alpha

tstat_beta = (b_est - 0)/se_beta
tstat_beta

#Q1(b)(iv) Calculate Pr(>|t|)
n = length(x)
n

# 1 regressor => k = 1
df = n - 1
df

pvalue_alpha = pt(tstat_alpha, df, lower.tail = ifelse(tstat_alpha > 0, FALSE, TRUE)) * 2
pvalue_alpha

pvalue_beta = pt(tstat_beta, df, lower.tail = ifelse(tstat_beta > 0, FALSE, TRUE)) * 2
pvalue_beta

# Q1(b)(i)
tstat_alpha

#Q1(b)(ii)
pvalue_alpha

#Q1(b)(iii) Can we reject null hypothesis at 5% significance level?
pvalue_alpha < .05

#Q1(c)(i)
q = qt(p = 0.025, df = df)
abs(q)

#Q1(c)(ii)
alpha_conf_int_low = a_est-abs(q)*se_alpha
alpha_conf_int_high = a_est+abs(q)*se_alpha

alpha_conf_int_low
alpha_conf_int_high

#Q1(c)(iii) Can we reject null hypothesis at 5% significance level?
print(0 > alpha_conf_int_high | 0 < alpha_conf_int_low)

#Q1(d)(i) 
tstat_beta = (b_est - 1.2)/se_beta
tstat_beta

pvalue_beta = pt(tstat_beta, df, lower.tail = ifelse(tstat_beta > 0, FALSE, TRUE)) * 2
pvalue_beta

#Q1(d)(ii) Can we reject null hypothesis at 5% significance level?
pvalue_beta < 0.05

# ------------------------------Q2-----------------------------------
# Clearing the workspace
rm(list=ls())

# Set working directory
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Read file into variable data
market_data = read.csv("Project_1_Q1_bm_10_ew.csv")

#Q2(a)(i)

# read market risk premium and risk free rate
mrp = market_data$Mkt.RF
rf = market_data$RF

x = mrp

alpha_vector = c()
beta_vector = c()

for (i in 2:11) {
  y = market_data[, i] - rf
  numerator=sum(x*y)-length(y)*mean(x)*mean(y)  # Computing the beta numerator
  denominator=sum(x^2)-length(y)*mean(x)^2      # Computing the beta denominator                  
  b_i=numerator/denominator                    # Computing the beta coefficient
  a_i = mean(y)-b_i*mean(x)                    # Computing the alpha coefficient
  alpha_vector = append(alpha_vector, a_i)
  beta_vector = append(beta_vector, b_i)
}
alpha_vector
beta_vector

#Q2(b)
pvalue_vector = c()
for (i in 1:length(alpha_vector)) {
  y = market_data[, i + 1] - rf
  x = mrp
  a_est = alpha_vector[i]
  b_est = beta_vector[i]
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
  
  #calculate df
  n = length(x)
  n
  
  # 1 regressor => k = 1
  df = n - 1
  df
  
  tstat_alpha = (a_est - 0) / se_alpha
  pvalue_alpha = pt(tstat_alpha, df, lower.tail = ifelse(tstat_alpha > 0, FALSE, TRUE)) * 2
  pvalue_vector = append(pvalue_vector, pvalue_alpha)
}
pvalue_vector

pvalue_vector < 0.05

#Q2(e)

beta = beta_vector
alpha = alpha_vector

plot(beta, alpha)

# ------------------------------Q3-----------------------------------
# Clearing the workspace
rm(list=ls())

# Set working directory
setwd("/Users/andrewkwok/Documents/FINC_3103")

# Read file into variable data
market_data = read.csv("Project_1_Q1_bm_10_ew.csv")

#Q3(a)
early_alpha = c()
early_beta = c()
early_pval = c()
#early_tstat = c()

dates = market_data$date

start_index = 1
end_index = match(199912, dates)

early_mrp = market_data$Mkt.RF[start_index:end_index]
early_rf = market_data$RF[start_index:end_index]

#early_check = c()

for (i in 2:11) {
  early_y = market_data[start_index:end_index, i] - early_rf
  
  # calculate alpha and beta
  numerator=sum(early_mrp*early_y)-length(early_y)*mean(early_mrp)*mean(early_y)  # Computing the beta numerator
  denominator=sum(early_mrp^2)-length(early_y)*mean(early_mrp)^2      # Computing the beta denominator
  b_early=numerator/denominator                   # Computing the beta coefficient
  a_early=mean(early_y)-b_early*mean(early_mrp)                   # Computing the alpha coefficient

  
  #calculate p
  # Computing the standard error for alpha and beta
  residuals = early_y-a_early-b_early*early_mrp 
  
  # Computing the vector of residuals
  s_squared = (1/(length(early_y)-2))*sum(residuals^2)  
  
  # Computing the estimate of the residuals' variance
  sum_x_squared = sum(early_mrp^2)        
  
  # Computing the numerator for the alpha standard error estimate
  sum_x_minus_xbar = sum((early_mrp-mean(early_mrp))^2)           
  
  # Computing the denominator for the alpha and beta standard error estimate
  se_alpha = sqrt(s_squared)*
    (sqrt((1/length(early_y))*sum_x_squared)/
       sqrt(sum_x_minus_xbar))
  
  # df
  n = length(early_mrp)
  n
  
  # 1 regressor => k = 1
  df = n - 1
  df
  
  tstat_alpha = (a_early - 0) / se_alpha
  pvalue_alpha = pt(tstat_alpha, df, lower.tail = ifelse(tstat_alpha > 0, FALSE, TRUE)) * 2
  
  #append values
  early_alpha = append(early_alpha, a_early)
  early_beta = append(early_beta, b_early)
  early_pval = append(early_pval, pvalue_alpha)
  
  
  # used to check against built in linear regression
  
  #early_tstat = append(early_tstat, tstat_alpha)
  
  #early_model = lm(early_y ~ early_mrp, data = market_data)
  #sum_early_model = summary(early_model)
  #print(summary(early_model))
  
  #early_values = c()
  #early_values = append(early_values, early_model$coefficients[1])
  #early_values = append(early_values, early_model$coefficients[2])
  #early_values = append(early_values, sum_early_model$coefficients[1,4])
  
  # check
  # early_check = append(early_check, early_values)
}

unname(early_alpha)
unname(early_beta)
unname(early_pval)
#matrix(unname(early_check), ncol = 3, byrow = TRUE)
#unname(early_tstat)

#Q3(b)
late_alpha = c()
late_beta = c()
late_pval = c()

dates = market_data$date

start_index = match(200001, dates)
end_index = length(dates)

late_mrp = market_data$Mkt.RF[start_index:end_index]
late_rf = market_data$RF[start_index:end_index]

for (i in 2:11) {
  late_y = market_data[start_index:end_index, i] - late_rf
  
  # calculate alpha and beta
  numerator=sum(late_mrp*late_y)-length(late_y)*mean(late_mrp)*mean(late_y)  # Computing the beta numerator
  denominator=sum(late_mrp^2)-length(late_y)*mean(late_mrp)^2      # Computing the beta denominator
  b_late=numerator/denominator                   # Computing the beta coefficient
  a_late=mean(late_y)-b_late*mean(late_mrp)                   # Computing the alpha coefficient
  
  
  #calculate p
  # Computing the standard error for alpha and beta
  residuals = late_y-a_late-b_late*late_mrp 
  
  # Computing the vector of residuals
  s_squared = (1/(length(late_y)-2))*sum(residuals^2)  
  
  # Computing the estimate of the residuals' variance
  sum_x_squared = sum(late_mrp^2)        
  
  # Computing the numerator for the alpha standard error estimate
  sum_x_minus_xbar = sum((late_mrp-mean(late_mrp))^2)           
  
  # Computing the denominator for the alpha and beta standard error estimate
  se_alpha = sqrt(s_squared)*
    (sqrt((1/length(late_y))*sum_x_squared)/
       sqrt(sum_x_minus_xbar))
  
  # df
  n = length(late_mrp)
  n
  
  # 1 regressor => k = 1
  df = n - 1
  df
  
  tstat_alpha = (a_late - 0) / se_alpha
  pvalue_alpha = pt(tstat_alpha, df, lower.tail = ifelse(tstat_alpha > 0, FALSE, TRUE)) * 2
  
  #append values
  late_alpha = append(late_alpha, a_late)
  late_beta = append(late_beta, b_late)
  late_pval = append(late_pval, pvalue_alpha)

}

unname(late_alpha)
unname(late_beta)
unname(late_pval)