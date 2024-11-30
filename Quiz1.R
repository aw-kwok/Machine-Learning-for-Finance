# QUIZ 1

#clear environment
rm(list=ls())

#set working directory
setwd("/Users/andrewkwok/Documents/FINC_3103")

#read file into data
data = read.csv("FF_3by2.csv")

#Q1

#read rf and mrp
rf = data$rf
mrp = data$mkt_exc

y = data$smhi - rf
x = mrp

results = summary(lm(y ~ x, data = data))
results
print(paste("alpha: ", results$coefficients[1,1]))

#Q2
print(paste("reject at 99%?", results$coefficients[1,4] < 0.01))

#Q3
pvalue_beta = results$coefficients[2,4]
pvalue_beta

n = length(mrp)
k=dim(results$coefficients)[1]
df = n - k

q = qt(p = 0.025, df = df)
beta_conf_int_low = results$coefficients[2,1]-abs(q)*results$coefficients[2,2]
print(paste("beta lower confidence interval:", beta_conf_int_low))

#Q4
tstat_beta = (results$coefficients[2,1] - 1.35)/results$coefficients[2,2]

pvalue_beta = pt(tstat_beta, df, lower.tail = ifelse(tstat_beta > 0, FALSE, TRUE)) * 2
pvalue_beta

#Q7
credit_data = read.csv("Credit.csv")

credit_data$Student = as.factor(credit_data$Student)

rating = credit_data$Rating
education = credit_data$Education
student = credit_data$Student

balance = credit_data$Balance

model_1_1 = summary(lm(balance ~ rating, data = credit_data))
model_1_2 = summary(lm(balance ~ education, data = credit_data))
model_1_3 = summary(lm(balance ~ student, data = credit_data))

model_1_1$adj.r.squared
model_1_2$adj.r.squared
model_1_3$adj.r.squared

#1st pass: keep rating

model_2_1 = summary(lm(balance ~ rating+education, data = credit_data))
model_2_2 = summary(lm(balance ~ rating+student, data = credit_data))

model_2_1$adj.r.squared
model_2_2$adj.r.squared

#2nd pass: rating and student

#Q8(i)
STEMB = 1
Female = 1
Salary_female = 60 + 20*STEMB - 10*Female

STEMB = 1
Female = 0
Salary_male = 60 + 20*STEMB - 10*Female

Salary_female < Salary_male

#Q8(ii)
STEMB = 1
Female = 1
Salary_female_stemb = 60 + 20*STEMB - 10*Female

STEMB = 0
Female = 0
Salary_male_nostemb = 60 + 20*STEMB - 10*Female

Salary_female_stemb > Salary_male_nostemb

#Q9(i)
STEMB = 1
Female = 1
Salary2_female_stemb = 65 + 15*STEMB - 12*Female + 20*STEMB*Female

STEMB = 1
Female = 0
Salary2_male_stemb = 65 + 15*STEMB - 12*Female + 20*STEMB*Female

Salary2_female_stemb < Salary2_male_stemb

#Q9(ii)
STEMB = 0
Female = 1
Salary2_female_nonstemb = 65 + 15*STEMB - 12*Female + 20*STEMB*Female

STEMB = 0
Female = 0
Salary2_male_nonstemb = 65 + 15*STEMB - 12*Female + 20*STEMB*Female

Salary2_female_nonstemb < Salary2_male_nonstemb

#Q10
STEMB = 1
Female = 1
Salary2_female_stemb = 65 + 15*STEMB - 12*Female + 20*STEMB*Female

Salary2_female_stemb