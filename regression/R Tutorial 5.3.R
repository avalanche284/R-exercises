# Multiple Linear Regression in R | R Tutorial 5.3 | MarinStatsLectures
# https://www.youtube.com/watch?v=q1RD5ECsSB0
# Read the data
LungCapData <- read.delim("/Users/simon/github_repos/R-exercises/regression/LungCapData.csv", header=T)
# attach the data
attach(LungCapData)
# Check names
names(LungCapData)
# Check the type of variable for Age and LungCap
class(Age)
class(LungCap)
class(Smoke)
levels(Smoke)
head(LungCapData)
plot(Age, LungCap, main = "Scatterplot")
mod <- lm(LungCap ~ Age)
# fit a model using Age and Height as X-variables
model1 <- lm(LungCapData$LungCap ~ Age + Height)
# Plot data to see whether there is a sense maing a prediction,
# whether there is a trend
plot(LungCap ~ Age)
plot(LungCap ~ Height)
# and indeed, there is.
summary(model1)
# Multiple R squared: 0.843
# Approximately 84% of variation in Lung Capacity can be explained
# by our model (Age and Height)
# Age 0.126368
# We associate an increase of 1 year in Age with an increase of 0.126
# in Lung Capacity adjusting or controling for Height


