# SB
#MachineLearningR__Brett_Lantz.pdf
#Chapter 6: Forecasting Numeric Data – Regression Methods
### LAUNCH problem page 166

launch <- read.csv("/Users/simon/github_repos/R-exercises/regression/challenger.csv")
# b is a slope
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# a is an intercept
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# calculating correlation
# 1st way
r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature) * sd(launch$distress_ct))
r
r1 <- cor(launch$temperature, launch$distress_ct)
r1

# regression
reg <- function(x, y) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}
reg
str(launch)

launch[2]
reg(y = launch$distress_ct, x = launch[2])

reg(y = launch$distress_ct, x = launch[2:4])


### INSURANCE
#Example – predicting medical expenses using linear regression
# The goal of this analysis is to use patient data to estimate the average
# medical care expenses for such population segments. 

insurance <- read.csv("/Users/simon/github_repos/R-exercises/regression/insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
boxplot(insurance$expenses)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])
library("psych")
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

#ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model <- lm(expenses ~ ., data = insurance)

ins_model
summary(ins_model)


#improving model
insurance$age2 <- insurance$age^2
insurance$age2
# bmi > 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# new model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

#Relative to our first model, the R-squared value has improved from 0.75
#to about 0.87. Our model is now explaining 87 percent of the variation
#in medical treatment costs.Additionally, our theories about the model's
#functional form seem to be validated. The higher-order age2 term is
#statistically significant, as is the obesity indicator, bmi30. The
#interaction between obesity and smoking suggests a massive effect;
#in addition to the increased costs of over $13,404 for smoking alone,
#obese smokers spend another $19,810 per year. This may suggest that smoking
#exacerbates diseases associated with obesity.




