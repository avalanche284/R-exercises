#CODE FOR FINANCIAL DATA SCIENCE CLASS

===========================================================================================================================#
  #=============================================================================================================================#
  #====================================================== BASICS OF R =======================================================#
  #=============================================================================================================================#
  #=============================================================================================================================#
  
  
  # We always start by cleaning the environment 
rm(list=ls())


# Evaluation
1+3


# Assignment
a <- 3


# Evaluation
a


# Spacing does not matter 
a<-3 
a <- 3 


# Combining 
b <- a + 1
b


# Other operations
b <- a * 2
b
b <- sqrt(a)
b
b <- a/2
b
b <- a - 1
b
b <- a^3
b
b <- (100*2) + (50/2)
b


a == b   # a is equal to b?
a != b   # a is not equal to b?


# Other comparison operators
5 > 6
6 > 5


# This command lists all objects in the global environment
ls()


# Remove an object
rm(a)


# Creating a vector a which contains three values 1,2,3
a <- c(1,2,3)
a


# Creating a vector b containing three elements "one", "two", "three"
b <- c("one", "two", "three")
b


# Combine
c <- c(a,b)
c

# create simulations
# Create a vector a with values from 1 to 5
a <- 1:5 # generate integers form 1 to 5
a<- 1: 1000



# Vector comparison 
v <- c(1,2,3,4,5)
v < 2
v == 3




# Create a matrix with values from 1 to 25, number of rows = 5 and number of columns = 5
A <- matrix(data=1:25, nrow=5, ncol=5)
A

# to do the merge
# Another way to do the same #6-10 is another vector
B <- cbind(1:5,6:10,11:15,16:20,21:25)
B

# How large 
# Get dimension of a vector or a matrix
dim(B) #dimensions
nrow(B)
ncol(B)
length(v)

# Selecting row and/or columns
bb <- B[,1]
bb
AA <- A[1:3,1:2]
AA


# R functions # we assign  # mean - arythetic mean
m   <- mean(bb)
med <- median(bb)


# User-defined functions
mediasq <- function(x) {
  m     <-sum(x^2)/length(x)   
  m
}

media(bb)
# Apply the same functions to the same rows o; apply to the matrix A 1 mean by column
m_row <- apply(A,1,mean) #[1] 11 12 13 14 15; 11 is the mean of the first row; 12 is the mean of... and dso on
m_col <- apply(A,2,mean)

# Getting help with R
help(vector)




#=============================================================================================================================#
#=============================================================================================================================#
#========================================== DESCRIPTIVE STATISTICS =====================================================#
#=============================================================================================================================#
#=============================================================================================================================#



# Clear the environment 
rm(list=ls())


# Packages that need to be installed if not already. You do this only once! 
# If you have installed a package once, you do not need to do it again in consecutive sessions.
install.packages("DescTools")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("corrplot")


# Calling the libraries. You need to do it always when you use a particular package.
library(DescTools)
library(ggplot2)
library(dplyr)
library(readxl)
library(Hmisc)
library(pastecs)
library(corrplot)


# Import dataset [data on bitcoin prices across different exchanges for the period May 2016 to April 2018 included 
# in file titled "exchanges.xlsx" ]
# There are several ways to import a dataset. 
# One is by changing the pathway in the line below so it matches the location where the file "exchanges.xlsx" 
# is on your computer
exchanges <- read_excel("~/Downloads/exchanges.xlsx")


# Another way to import a dataset is by clicking "Import Dataset" (from the Environment section), 
# click "From Excel", then "Browse", then find the file in your computer and 
# click "Import". Once you do that, the dataset will be visible in the environnmnent section!
# In summary, click Import Dataset --> From Excel --> Browse --> Find file --> Import 


# Import data
# Create a copy of the dataset on which we will run all operations
data <- exchanges


# Investigate the dataset 
# Check the dimensions of the dataset
dim(data)


# Check the types of variables 
str(data) 


# Check the names of the variables
colnames(data)


# Summarizing/presenting the data
# Find the mean of the bitcoin price on the coinbase exchange
mean(data$btc_coinbase)


# Find the median of the bitcoin price on the coinbase exchange
median(data$btc_coinbase)


# Find the variance of the bitcoin price on the coinbase exchange
var(data$btc_coinbase)


# Find the standard deviation of the bitcoin price on the coinbase exchange
sd(data$btc_coinbase)


# Find the mean value of all columns in a dataset 
# Here we use the function sapply. When do we use the apply function? 
# We use it when we have structured dataset we wish to perform operations on. 
# Note: with -1 we ignore the first column (no sense to calculate the mean of a date).
sapply(data[-1], mean)


# Find the variance of all columns in a dataset 
sapply(data[-1], var)



# Using more elaborate functions
# The function summary gives the mean, median, 25th and 75th quartiles, min, max value of all columns in the dataset
summary(data)



# The function stat.desc gives:
# the number of values (nbr.val), 
# the number of null values (nbr.null), 
# the number of missing values (nbr.na), 
# the minimal value (min), 
# the maximal value (max), 
# the range (range, that is, max-min) and 
# the sum of all non-missing values (sum) 
# the median (median), 
# the mean (mean), 
# the standard error on the mean (SE.mean), 
# the confidence interval of the mean (CI.mean) at the p level,
# the variance (var), 
# the standard deviation (std.dev) and 
# the variation coefficient (coef.var) defined as the standard deviation divided by the mean
stat.desc(data$btc_coinbase) 




# Histogram
hist(data$btc_coinbase)


# Boxplot 
boxplot(data$btc_coinbase)


# Add a title to the boxplot or any other plot
boxplot(data$btc_coinbase, main = "Boxplot of the Bitcoin price (Coinbase Exchange)")


# Plot the increase in bitcoin prices (bitcoin exchange) over time
head(data$Date)
data$Date <- as.Date(substr(data$Date, 1, 10))


# One way to plot is by using the function "plot"
plot(btc_coinbase ~ Date, data, xaxt = "n", type = "l") #xaxt='n' suppresses ticks on the x axis
axis(1, data$Date, format(data$Date))


# Second way to plot is using the package ggplot2
ggplot(data = data, aes(Date, btc_coinbase)) + geom_line(colour='red') #try geom_point()


# Comparison --> Comparing the movement in price between two variables (prices on two different exchanges)
ggplot(data, aes(Date)) + 
  geom_line(aes(y = btc_coinbase, colour = "btc_coinbase")) + 
  geom_line(aes(y = btc_kraken, colour = "btc_kraken"))


# Comparison --> Comparing the movement in price between three variables 
ggplot(data, aes(Date)) + 
  geom_line(aes(y = btc_coinbase, colour = "btc_coinbase")) + 
  geom_line(aes(y = btc_kraken, colour = "btc_kraken"))  + 
  geom_line(aes(y = oil, colour = "oil"))

data1<-data[-1]

# Investigate correlation between two returns 
cor(data2$btc_coinbase, data2$btc_kraken)
cor(data2$btc_coinbase, data2$oil) # very small correlation
cor(data2$btc_coinbase, data2$sp500) # very small correlation 


# Check all correlations by using the cor function 
correlations <- cor(data2)


# Visualize the correlation plot
corrplot(correlations, method="circle")


# Different visualization methods can be used: "circle", "square", "ellipse", "number", "shade", "color", "pie".
corrplot(correlations, method="square")
corrplot(correlations, method="number")




####################################################################################################
##################################### LINEAR REGRESSION MODELS ############################################
####################################################################################################


# Clear the environment 
rm(list=ls())


# Install packages by removing the # and running the lines 
install.packages("ggplot2")
 install.packages("readxl")
 install.packages("corrplot")




# Call libraries
library(ggplot2)
library(readxl)
library(corrplot)


 exchanges <- read_excel("exchanges.xlsx")


data <- exchanges

data1 <- data[-1]


# Create a separate dataset with returns instead of prices (log(x)-log(x-1))
data2 <- as.data.frame(sapply(data1, function(x) diff(log(x), lag=1)))


# Run a simple linear regression y=a+bx [btc_coinbase on sp500]
model <- lm(btc_coinbase ~ sp500, data = data2)
summary(model)


# Run a simple linear regression [btc_coinbase on btc_kraken]
model_2 <- lm(btc_coinbase ~ btc_kraken, data = data2)
summary(model_2)


# Run a multiple linear regression [btc_coinbase on all other variables]
model_3 <- lm(btc_coinbase ~., data = data2)
summary(model_3)


coeff <- model_3$coefficients


# Get and plot residuals
res <- model_3$residuals
# res <- residuals(model_3)
plot(res, type='l')


# Convert to DataFrame for gglpot
res <- as.data.frame(res)


# Check residuals' distribution 
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5, binwidth=0.003)


# Some plots for model diagnostics
plot(model_3) # hitting return key you move from a plot to another


# Get fitted values 
fit <- model_3$fitted.values
# fit <- fitted.values(model_3)

date_returns <- data$Date[2:nrow(data)]

results <- cbind.data.frame(date_returns, data2$btc_coinbase, fit)
colnames(results)<-c('Date','Observed','Fitted')


# Plot observed vs fitted values
ggplot(results, aes(Date)) + 
  geom_line(aes(y = Observed, colour = "Observed")) + 
  geom_line(aes(y = Fitted, colour = "Fitted")) +
  ylab('Returns') +
  ggtitle('Btc coinbase: in-sample fit') + theme(plot.title = element_text(hjust = 0.5))


# Evaluate model prediction 
# One of the most used measures is MSE (mean squared error)
mse <- mean((results$Fitted-results$Observed)^2)
print(mse)


# Root mean squared error
mse^0.5


# R-Squared Value for our model 
SSE = sum((results$Fitted - results$Observed)^2)
SST = sum( (results$Observed - mean(results$Observed) )^2)
R2 = 1 - SSE/SST
R2


# Split the sample in two, to make out-of-sample predictions 
data2_in  <- data2[1:500,]
data2_out <- data2[501:nrow(data2),]


# Estimate the model on the first subsample
model_out <- lm(btc_coinbase ~., data = data2_in)


# Apply the model to the second subsample
fit_out <- predict(model_out, data2_out)


date_returns_out <- date_returns[501:nrow(data2)]


results_out <- cbind.data.frame(date_returns_out, data2_out$btc_coinbase, fit_out)
colnames(results_out)<-c('Date','Observed','Predicted')


# Calculate RMSE for the out-of-sample predictions
plot(data2_out$btc_coinbase, fit_out)
rmse <- sqrt(mean((results_out$Predicted-results_out$Observed)^2))
print(rmse)


####################################################################################################
####################################### LOGISTIC REGRESSION MODELS ############################################
####################################################################################################

rm(list=ls())

# Packages that need to be installed 
install.packages("readxl")
install.packages("caret")


# Calling the libraries
library(readxl)
library(caret)



# Import dataset 
# 6018 companies for which we observe: balance sheet data in 2014, status (active/defaulted) in 2015
# "Default" is a binary variable assuming values 0 (no default) or 1 (default)

companies_data <- read_excel(SME_dataset.xlsx)

data <- SME_dataset


## Get default rate
def_perc <- sum(data$Default)/length(data$Default)
print(def_perc)


data$Default    <- as.factor(data$Default)


# Let's define our own function "percentage", to make some exercise with user-defined functions
percentage <- function(x) {
  y <- as.numeric(levels(x))[x]
  prc <- sum(y)/length(y)*100
  prc
}

percentage(data$Default)


# An example of comparing the distribution of a variable between the two groups
# Plot the distribution of ROE accross different status (Default = 0; Default = 1)
ggplot(data, aes(ROE, fill = Default)) + geom_density(alpha = 0.2)



# Split the dataset into training and testing samples 
# Sampling: a) Random Sampling and b) Stratified Sampling 

# a) Random Sampling 
# Set a random seed so that your results can be reproduced
set.seed(300)

perc        <- 0.7
n_train     <- round(perc*nrow(data))
data_sample <- data[sample(nrow(data)), ]          
data.train  <- data_sample[1:n_train, ]              
data.test   <- data_sample[(n_train+1):nrow(data_sample), ]    
percentage(data.train$Default)
percentage(data.test$Default) ## the percentage of defaults in the test sample is quite different 
                              ## from the percentage in the training sample

# With a simple random sampling technique, we are not sure whether 
# the subgroups of the Default variable are represented equally or proportionately within the two sub-samples.

# b) Stratified Sampling 
set.seed(300)
                            
div <- createDataPartition(y = data$Default, p = perc, list = F)
   
# Training Sample
data.train_1 <- data[div,] # 70% here
percentage(data.train_1$Default)

# Test Sample
data.test_1 <- data[-div,] # the rest of the data goes here
percentage(data.test_1$Default) # now the percentage of defaults in the two sub-samples is quite similar 


### MODEL WITH ALL VARIABLES (for the time being)
fit1 <- glm(Default ~ ., data=data.train_1, family=binomial())
summary(fit1)


## Odds ratios 
exp(coefficients(fit1)) # how do you interpret the exp of coefficients?


## Get predicted default probabilities
data.test_1$score <- predict(fit1, type='response', data.test_1)


# Decide a cut-off and get predictions
cut_off <- def_perc
data.test_1$pred <- ifelse(data.test_1$score<=cut_off, 0, 1)


# Does the model classify the companies well?
# Let's introduce some measures used to investigate the performance of models for binary variables:  
# false positive rate and false negative rate, sensitivity and specificity
# you will see them in more detail during next lesson


##false positive rate
n_neg <- nrow(data.test_1[data.test_1$Default=='0',])
data.test_1$fp_flag <- ifelse(data.test_1$pred==1 & data.test_1$Default=='0', 1, 0)
fpr <- sum(data.test_1$fp_flag)/n_neg #false positive rate


##false negative rate
n_pos <- nrow(data.test_1[data.test_1$Default=='1',])
data.test_1$fn_flag <- ifelse(data.test_1$pred==0 & data.test_1$Default=='1', 1, 0)
fnr <- sum(data.test_1$fn_flag)/n_pos #false negative rate


##sensitivity
1- fnr


##specificity
1- fpr

###################################################################################################
##################################### LINEAR MODEL SELECTION ############################################
####################################################################################################


# Clear the environment 
rm(list=ls())


# Install packages 
 install.packages("Hmisc")
 install.packages("ggplot2")
 install.packages("readxl")
 install.packages("corrplot")
 install.packages("ppcor")
 install.packages("forecast")


# Call libraries
library(readxl)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(ppcor)



# Import dataset SME_dataset_2.xlsx 


companies_data <- read_excel(SME_dataset_2.xlsx))

data <- companies_data



dim(data)
head(data)

summary(data)
str(data) 

data$NACE_group <- as.factor(data$NACE_group) #NACE_group is a number, but it cannot be used as a continuous variable



## Find the determinants of profitability (Return on Equity) of companies


# Check the distribution of ROE in the sample
boxplot(data$ROE)


# A way of dealing with (possible) outliers: winsorization
rmOutlier <- function(x){
  low  <- quantile(x, 0.05, na.rm = T) 
  high <- quantile(x, 0.95, na.rm = T) 
  out <- ifelse(x > high, high,ifelse(x < low, low, x)) 
  out }


clean      <- sapply(data[,-c(10:11)], rmOutlier)
data.clean <- cbind(clean, data[,10:11])


# Check all correlations by using the cor function 
correlations <- cor(data.clean[,-c(10:11)]) #we exclude non-numeric variables


# Visualize the correlation plot
corrplot(correlations, method="circle") 
corrplot(correlations, method="number") 


# Check partial correlations by using the pcor function 
p_correlations <- pcor(data.clean[,-c(10:11)])
pcor_mat       <- p_correlations$estimate


# Visualize the partial correlation plot
corrplot(pcor_mat, method="circle") 
corrplot(pcor_mat, method="number") 




# Set a random seed so that your results can be reproduced
set.seed(1000)

# Split the dataset into training and testing samples 
n_train <- round(nrow(data.clean)*0.8) 

data.all   <- data.clean[sample(nrow(data.clean)), ]          

data.train <- data.clean[1:n_train, ]              

data.test <- data.clean[(n_train+1):nrow(data.clean), ]    


# Multiple linear regression on the training dataset
fit1     <- lm(ROE ~ . - Default, data.train)
summary(fit1)


# Perform a stepwise linear regression 
 fit_step_f <- step(fit1, direction='forward')
 summary(fit_step_f)
 
 fit_step_b <- step(fit1, direction='backward')
 summary(fit_step_b)

fit_step   <- step(fit1, direction='both')
summary(fit_step)

# plot(fit_step)


# Get residuals and perform an F-test to compare the full model with the reduced one
res      <- fit_step$residuals
res_full <- fit1$residuals

SSE_red  <- sum(res^2)
SSE_full <- sum(res_full^2)

p <- length(coefficients(fit_step))-1 
k <- length(coefficients(fit1)) - length(coefficients(fit_step)) 
N <- nrow(data.train)

f_stat_num <- (SSE_red-SSE_full)/k
f_stat_den <- SSE_full/(N-p-k-1)

f_stat     <- f_stat_num/f_stat_den

f_pvalue   <- 1-pf(f_stat, df1=k, df2=N-p-k-1) # non-significant difference of residual variance between the two models


## ANOVA for model comparison
m0 <- lm(ROE ~ 1, data.train) #model with no predictors
m1 <- fit_step       

anova(m0,m1)

m2 <- fit1

anova(m1,m2)
  



## We found several significant relationships that explain companies' profitability: but is this a good predictive model? 

# Make predictions on the test dataset 
predictions       <- predict(fit_step,data.test)
results           <- cbind(data.test$ROE,predictions) 
colnames(results) <- c('Real','Predicted')
results           <- as.data.frame(results)


# Calculate error measures
## MSE
mse  <- mean((results$Real-results$Predicted)^2)

print(mse)


## RMSE
rmse     <- sqrt(mse)

print(rmse)


## MAE
mae <- mean(abs(results$Real-results$Predicted))

print(mae)

dmar<-dm.test(fit$residuals,fit1$residuals,alternative="two.sided") # Diebold-Mariano test

####################################################################################################
####################################### LOGISTIC MODEL SELECTION ############################################
####################################################################################################

rm(list=ls())

# Packages that need to be installed if not yet
install.packages("readxl")
install.packages("caret")
install.packages("ROCR")
install.packages("pROC")

# Calling the libraries
library(readxl)
library(caret)
library(ROCR)
library(pROC) # package for the computation of the AUROC measure and the implementation of the DeLong test



# Import dataset 
# 6018 companies for which we observe: balance sheet data in 2014, status (active/defaulted) in 2015
# "Default" is a binary variable assuming values 0 (no default) or 1 (default)

companies_data <- read_excel(SME_dataset)

data <- companies_data




data$Default    <- as.factor(data$Default)


# Split the dataset into training and testing samples 

# Stratified Sampling 
perc        <- 0.7

set.seed(500)
                            
div <- createDataPartition(y = data$Default, p = perc, list = F)
   
# Training Sample
data.train <- data[div,] # 70% here


# Test Sample
data.test <- data[-div,] # the rest of the data goes here


## Model including all predictors
fit1 <- glm(Default ~ ., data=data.train, family=binomial())
summary(fit1)


## Stepwise regression
fit_step <- step(fit1, direction = 'both')
summary(fit_step)


##ANOVA
anova(fit_step, fit1,test="Chisq")



## Get predicted default probabilities
data.test$score  <- predict(fit_step, type='response', data.test)
data.test$score1 <- predict(fit1, type='response', data.test)


## Plot AUROC
perf_auroc <- performance(prediction(data.test$score,data.test$Default),"auc")
auroc      <- as.numeric(perf_auroc@y.values)

perf_plot  <- performance(prediction(data.test$score,data.test$Default),"tpr","fpr")

plot(perf_plot, main='ROC', col='blue',lwd=2)


## Compare AUROC
### note: in this case the two AUROCs are very close, so the two ROC curves are overlapping
perf_auroc1 <- performance(prediction(data.test$score1,data.test$Default),"auc")
auroc1      <- as.numeric(perf_auroc1@y.values)

perf_plot1  <- performance(prediction(data.test$score1,data.test$Default),"tpr","fpr")


plot(perf_plot, col='blue', lwd=2) 
plot(perf_plot1, add=TRUE, col='red', lwd=2) 
legend("right", legend=c("Model from stepwise", "Full model"), lty=(1:1), col=c("blue", "red"))

roc1<-roc(data.test$Default,data.test$score )
roc2<-roc(data.test$Default,data.test$score1 )


dl<-roc.test(roc1,roc2,method="delong") # DeLong test
dl

####################################################################################################
##################################### CLUSTER ANALYSIS #############################################
####################################################################################################


# Clear the environment 
rm(list=ls())


# Install packages by removing the # and running the lines 
install.packages("readxl")
install.packages("e1071")
install.packages("ppcor")
install.packages("readr")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("cluster")
install.packages("moments")


# Call libraries 
library(readxl)
library(e1071)
library(ppcor)
library(readr)
library(readxl)
library(Hmisc)
library(pastecs)
library(cluster)
library(moments)



# For the first part of the code, we use a dataset containing balance sheet information on a sample of Italian companies 
# File name: companies_data
companies_data <- read_excel( 'SME_dataset.xlsx')


# Make a copy which we use for the analysis 
dataset <- companies_data


# Investigate the dataset 
head(dataset)


# Check the dimensions of the dataset
dim(dataset)


# Check the types of variables 
str(dataset) 


# Check and store the names of the variables
names <- colnames(dataset)


# Summarize the dataset
summary(dataset)


# Use sapply to list the type for each variable 
types <- sapply(dataset, class) 
type_table <- as.data.frame(cbind(names, types))


# Select only the columns that we should consider as numeric
data <- dataset[,2:9]
names_num <- colnames(data)

# Descriptive statistics
descr_stat <- as.data.frame(stat.desc(data)) 

write.csv(descr_stat, file=paste(out_dir, 'descr_stat.csv', sep=''))


# Use sapply to calculate the standard deviation for all variables 
stdev   <- sapply(data, sd) 
sd_data <- as.data.frame(cbind(names_num, stdev))


# Use sapply to display skewness 
skwn <- sapply(data, skewness) 
skwn_data <- as.data.frame(cbind(stdev, skwn))
# The further the distribution of the skew value from zero, the larger the skew to the left (negative skew value) 
# or right (positive skew value)


# Distribution plots
n <- ncol(data)
pdf_dir1 <- paste(out_dir, 'histograms.pdf', sep='')
pdf_dir2 <- paste(out_dir, 'boxplots.pdf', sep='')
 
pdf(file=pdf_dir1)
for (i in 1:n) {
  hist(data[,i])
  main <- colnames(data)[i]
}
dev.off() 

pdf(file=pdf_dir2)
for (i in 1:n) {
  boxplot(data[,i], main = colnames(data)[i])
}
dev.off() 


#K-means clustering

# Standardize the variables
data.scale <- data.frame(scale(data))


help("scale")


# K-means clustering
# Recall that K-means is an algorithm that partitions a data set into K 
# clusters by minimizing the sum of squared distance in each cluster
set.seed(999)
km_fit <- kmeans(data.scale, 3) 


# Clusters
km_fit$cluster
# example of output
#A list with components:
#  - cluster
# A vector of integers indicating the cluster to which each point is allocated.
#  - centers
# A matrix of cluster centres.
#  - within group sum of squares
# The within-cluster sum of square distances for each cluster.
#  - size
# The number of points in each cluster.

# Cluster sizes
km_fit$size


# Declare a function to plot the within group sum of squares (WGSS) vs the number of clusters. 
wgssplot <- function(data, nc, seed){
  wgss <- rep(0,nc)
  wgss[1] <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wgss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wgss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


# How to determine the number of clusters? We look at the percentage of explained variance as a function of the number of clusters.
# Plot the WGSS across different number of clusters
wgssplot(data.scale, nc=6, seed=999) 


# Is there a relationship between the clusters and the companies' probability of default?
data_clus <- as.data.frame(cbind(dataset, km_fit$cluster))
colnames(data_clus)[11] <- 'cluster'
clus_def <- table(data_clus$cluster, data_clus$Default)


def_table <- clus_def/km_fit$size
def_rates <- def_table[,2]
def_rates



# Hierarchical clustering. 
#Hierarchical methods use a distance matrix as an input for the clustering algorithm.

distance <- dist(data.scale, method = "euclidean") # Euclidean distance 


# Hierarchical clustering
hir_fit <- hclust(distance)


# Plot the dendrogram 
# The main graphical tool for looking at a hierarchical cluster solution is known as a dendogram. 
# This is a tree-like display that lists the objects which are clustered along the x-axis, 
# and the distance at which the cluster was formed along the y-axis.
plot(hir_fit) 


# Cut tree into 3 clusters
groups <- cutree(hir_fit, k=3)


# Draw dendogram with red borders around the 3 clusters
rect.hclust(hir_fit, k=3, border="red") 




# Case study - paper by  Paolo Giudici and Gloria Polinesi 

# In this paper the authors use the ETF_Italian dataset and cluster them using correlations 
# Import data on the return of 52 Italian ETF (Exchange traded funds) over a period of 486 days. 
# File name: "ETF_Italian"
ETF_Italian <- read_csv(paste(in_dir, 'ETF_Italian.csv', sep=''))

dataset <- ETF_Italian


# Correlation  
correlation <- cor(dataset)



# First step is calculating a distance matrix. 
dist <- 2-2*correlation
dist <- as.dist(dist)


# Next, we apply the hclust method. We use the default method of hclust - meaning that when a cluster is formed, 
# its distance to other objects is computed as the maximum distance between any object in the cluster and the other objects ("complete link")
H <- hclust(dist) 

# Now that we got a cluster solution, we need to examine the results.
plot(H,labels=colnames(dataset), main='Italian ETF')


# Draw borders on a number of classes you want
rect.hclust(H, k=6, border="red")



# Specify the method (single, complete, average etc)
# In complete-link (or complete linkage), 
# in each step we merge the two clusters with the smallest MAXIMUM pairwise distance.

# In single-link (or single linkage) hierarchical clustering, 
# in each step we merge the two clusters with the smallest MINIMUM pairwise distance.

# In average-link hierarchial clustering, we merge in each step the two clusters with the smallest distance 
# calculated as the AVERAGE distance between all pairs of data points, each of which
# comes from a different group.

H_single <- hclust(dist, method = "single")
H_average <- hclust(dist, method = "average")


plot(H_single,labels=colnames(dataset), main='hclust - single')
plot(H_average,labels=colnames(dataset), main='hclust - average')


# We can choose the number of clusters we are interested in. For example, we can look at the 6-cluster solution 
# using the cutree function.
groups6 <- cutree(H,6) # 6 clusters 
table(groups6)


# Scatter plot
plot(groups6)


# We can change the number of classes
groups3 <- cutree(H,3) # 3 clusters 
table(groups3)


# Scatter plot
plot(groups3)


####################################################################################################
##################################### NETWORK MODELS #############################################
####################################################################################################


# Clear the environment 
rm(list=ls())


# Install packages by removing the # and running the lines 
 install.packages("readxl")
 install.packages("qgraph")
 install.packages("igraph")


# Call libraries 
library(readxl)
library(qgraph)
library(igraph)



#obtain a network from a dataset

# Import dataset exchanges.xlsx

exchanges <- read_excel( "exchanges.xlsx")


# Create a copy of the dataset, called data, on which we will run all operations
data <- exchanges


# Have a preview of the first few observations per each variable
head(data)


# Get the column names
colnames(data)


# Calculate returns from prices
data2 <- as.data.frame(sapply(data[,-1], function(x) diff(log(x), lag=1)))
data2 <- as.data.frame(cbind(data[2:nrow(data),1],data2))


## Correlation Network

# First step: calculate correlations
cor_network <- cor_auto(data2[,-1])


# Second step: Plot

# The function qgraph can be used to plot correlations networks. 
# The argument "graph = " determines the type of graph to be made when a correlation or covariance matrix 
# is used as input. The choice of graph = "cor" plots a correlation network whereas graph = "pcor"
# plots a partial correlation network. 


# Simple correlation network
Graph_1 <- qgraph(cor_network, graph = "cor", layout = "spring", edge.width=0.5)
summary(Graph_1) # provides a summary of the network (number of edges)


# Partial correlation network
Graph_2 <- qgraph(cor_network, graph= "pcor", layout = "spring", edge.width=1)
summary(Graph_2)


# We can get more precise results if we eliminate links that are not statistically significant.
# The threshold argument can be used to do just that -- to remove edges that are not significant.
Graph_3 <- qgraph(cor_network, graph = "pcor", layout = "spring", edge.width=1, threshold = "sig",
                  sampleSize = nrow(data), alpha = 0.05)
summary(Graph_3)


# Investigate the centrality measures of the graphs 
centralities_Graph1 <- centrality(Graph_1)
centralities_Graph2 <- centrality(Graph_2)
centralities_Graph3 <- centrality(Graph_3)


# Plotting the centrality measures
centralityPlot(Graph_1, include =c("Strength", "Closeness"))


# Compare the two networks
centralityPlot(GGM = list(correlation = Graph_1, partial_correlation = Graph_2))




##########################
##### NEURAL NETWORKS ####
##########################

# Clear the environment 
rm(list=ls())


# Install packages by removing the # and running the lines 
install.packages("readxl")
install.packages("readr")
install.packages("ggplot2")                                  
install.packages("nnet")
install.packages("neuralnet")


# Call libraries 
library(readxl)
library(readr)
library(ggplot2)
library(nnet)
library(neuralnet)



# Application 1. Import data on the return of 52 Italian ETF (Exchange traded funds) 
# classified by their riskiness (6 classes), over a period of 31 days



# and run the following line
ETF_Italian_31days <- read_csv('ETF_Italian_31days.csv')


# Make a copy which we use for the analysis                                  
data <- ETF_Italian_31days


# Investigate the dataset
summary(data)


# Main variable of interest --> class 
table(data$class)


# In this case, the dependent variable is the "class" and we need to encode it.
# This means that, from 1 variable "class", we will get 6 that will take the values 1/0.
train <- cbind(data[, 2:32], class.ind(as.factor(data$class)))


# Set labels for the new created columns while keeping the same names for other columns
names(train) <- c(names(data)[2:32],"c1","c2","c3", "c4", "c5", "c6")


# Write down the formula that will be used to fit the neural network 
name <- names(train)
formula <- as.formula(paste("c1 + c2 + c3 + c4 + c5 + c6  ~", paste(name[!name %in% c("c1","c2","c3", "c4", "c5", "c6", "ETF_name", "class")], collapse = " + ")))


# Print to check the formula that we will apply in the following step
formula


# Let's fit a neural network on the full dataset
set.seed(134)
nn5 <- neuralnet(formula,
                 data = train,
                 hidden = c(5),  ## 5 neurons in the hidden layer
                 act.fct = "logistic",
                 err.fct = 'ce', ## entropy-based error measure
                 linear.output = FALSE,
                 lifesign = "minimal")


# Plot the neural network with 5 hidden units
plot(nn5)


# Let's change the number of neurons in the hidden layer (= 15)
set.seed(134)
nn <- neuralnet(formula,
                data = train,
                hidden = c(15),
                act.fct = "logistic",
                err.fct = 'ce',
                linear.output = FALSE,
                lifesign = "minimal")

# Plot the neural network with 15 hidden units
plot(nn)



##### Let's have a look at the accuracy on the training set (in-sample accuracy) #####

# Compute predictions
nn_pred <- compute(nn, train[, 1:31])


# Extract results
nn_pred1 <- nn_pred$net.result
head(nn_pred1)


# Accuracy (in-sample)
original_values <- max.col(train[, 32:37])
predicted <- max.col(nn_pred1)
mean(predicted == original_values)



# Initialise vector of results 
outs <- NULL


# Train-test split proportions
proportion <- 0.80


# 10-fold cross validation
k=10

# Crossvalidate
# Set seed for reproducibility purposes
set.seed(134)




mean(outs)



#Application 2 
rm(list=ls())

# Import the bitcoin prices dataset
exchanges <- read_excel('exchanges.xlsx')


# Make a copy which we use for the analysis and calculate returns                                
data <- as.data.frame(sapply(exchanges[,-1], function(x) diff(log(x), lag=1)))
data <- as.data.frame(cbind(exchanges[2:nrow(exchanges),1],data))


# We choose the return of Bitcoin on Coinbase as response variable
# We train the neural network on the 2016-2017 period and predict returns in 2018
train <- data[data$Date<='2017-12-31', 2:ncol(data)]


formula <- btc_coinbase ~ .


# Let's fit a neural network on the training dataset. 
set.seed(123)
nn <- neuralnet(formula,
                 data = train,
                 hidden = c(10), 
                 linear.output = TRUE,  # the default error option for linear functions is SSE (no need to specify it)
                 lifesign = "minimal")


# Plot the neural network 
plot(nn)


# Validate on the testing sample
test <- data[data$Date>'2017-12-31', 2:ncol(data)]

set.seed(123)
nn_pred <- compute(nn, test)
  
  
# Extract results
predicted<- nn_pred$net.result
  
  
# Accuracy (test set)
original_values <- test$btc_coinbase
rmse <- sqrt(mean((original_values-predicted)^2))


####################################################################################################
####################################### TREE MODELS ############################################
####################################################################################################

# Clear the environment
rm(list=ls())

# Install the following packages if not installed yet                         
 install.packages("readxl")
 install.packages("rpart")
 install.packages("partykit")
 install.packages("rattle")
 install.packages("rpart.plot")
 install.packages("caret")
 install.packages("ggplot2")
 install.packages("ROCR")
 install.packages("randomForest")
 install.packages("pROC")


# Call libraries
library(readxl) 
library(rpart) 
library(partykit) 
library(rattle) 
library(rpart.plot) 
library(caret) 
library(ggplot2)
library(ROCR) 
library(randomForest) 
 library(pROC)


# Import Bank Marketing dataset (public)
## [Moro et al., 2014] S. Moro, P. Cortez and P. Rita
## "A Data-Driven Approach to Predict the Success of Bank Telemarketing" (Decision Support Systems)
## Input variables:
        # bank client data:
# age (numeric)
# job : type of job (categorical: "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")
# marital : marital status (categorical: "divorced","married","single","unknown")
# education (categorical: "basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")
# housing: has housing loan? (categorical: "no","yes","unknown")
# loan: has personal loan? (categorical: "no","yes","unknown")
        # related with the last contact of the current campaign:
# contact: contact communication type (categorical: "cellular","telephone") 
# month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# day_of_week: last contact day of the week (categorical: "mon","tue","wed","thu","fri")
# duration: last contact duration, in seconds (numeric)
        # other attributes:
# campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; missing if the client was not contacted in previous campaigns)
# previous: number of contacts performed before this campaign and for this client (numeric)
# poutcome: outcome of the previous marketing campaign (categorical: "failure","nonexistent","success")

## Output variable (desired target):
# y - has the client subscribed a term deposit? (binary: "yes","no")


clients_data <- read_excel('C:bank_marketing.xlsx')
data <- clients_data


data$y <- as.factor(data$y)

for (i in 1:9) {
        data[i] <- lapply(data[i], as.factor)
}


# Count the cases in which the target variable is equal to "yes" (success percentage)
yes_perc <- nrow(data[data$y=='yes',])/nrow(data)*100

print(yes_perc)


# Stratified Sampling --> considering the target variable as a strata
set.seed(1000)
div <- createDataPartition(y = data$y, p = 0.7, list = F)

# Training Sample
data.train <- data[div,] # 70% here

# Test Sample
data.test <- data[-div,] # rest of the data (30%) goes here



# We use the CART decision tree algorithm
# The CART algorithm for classification trees minimizes the Gini impurity in each group

fit1 <- rpart(y ~ ., data=data.train, method = "class")


# Print tree detail
printcp(fit1)

# Plot the tree
plot(fit1, margin = 0.2, main="Tree: Recursive Partitioning")
text(fit1, cex=0.8) 

prp(fit1, type=2, extra=1,  main="Tree: Recursive Partitioning") # type=2 draws the split labels below the node labels
                                                                 # extra=1 displays the number of observations that fall in the node 

fancyRpartPlot(fit1)


# Make predictions on the test sample
data.test$fit1_score <- predict(fit1,type='prob',data.test)
fit1_pred <- prediction(data.test$fit1_score[,2],data.test$y)
fit1_perf <- performance(fit1_pred,"tpr","fpr")


# Model performance plot
plot(fit1_perf, lwd=2, colorize=TRUE, main="ROC Fit1: Recursive Partitioning")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3)

# AUROC, KS and GINI
# The KS statistic is the maximum difference between the cumulative percentage of "yes" (cumulative true positive rate)
# and the cumulative percentage of "no" (cumulative false positive rate)
# The Gini coefficient is measured in values between 0 and 1, where a score of 1 means that the model is 100% accurate
# in predicting the outcome, While a Gini score equal to 0 means that the model is entirely inaccurate (random model).
fit1_AUROC <- round(performance(fit1_pred, measure = "auc")@y.values[[1]]*100, 2)
fit1_KS <- round(max(attr(fit1_perf,'y.values')[[1]]-attr(fit1_perf,'x.values')[[1]])*100, 2)
fit1_Gini <- (2*fit1_AUROC - 100)
cat("AUROC: ",fit1_AUROC,"\tKS: ", fit1_KS, "\tGini:", fit1_Gini, "\n")



# Conditional inference tree --> 
# Both rpart and ctree recursively perform univariate splits 
# of the target variable based on values on the other variables in the dataset
# Differently from the CART, ctree uses a significance test procedure 
# in order to select variables instead of selecting the variables that minimize the Gini impurity.

fit2 <- ctree(y ~ ., data=data.train)

# Summary
fit2


# This is essentially a decision tree but with extra information in the terminal nodes.
plot(fit2, gp = gpar(fontsize = 6),     
     ip_args=list(abbreviate = FALSE, 
                  id = FALSE))

# Make predictions on the test sample
data.test$fit2_score <- predict(fit2,type='prob',data.test)
fit2_pred <- prediction(data.test$fit2_score[,2],data.test$y)
fit2_perf <- performance(fit2_pred,"tpr","fpr")

# Model performance plot
plot(fit2_perf, lwd=2, colorize=TRUE, main="ROC Fit2: Conditional Inference Tree")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3)

#  AUROC, KS and GINI
fit2_AUROC <- round(performance(fit2_pred, measure = "auc")@y.values[[1]]*100, 2)
fit2_KS <- round(max(attr(fit2_perf,'y.values')[[1]]-attr(fit2_perf,'x.values')[[1]])*100, 2)
fit2_Gini <- (2*fit2_AUROC - 100)
cat("AUROC: ",fit2_AUROC,"\tKS: ", fit2_KS, "\tGini:", fit2_Gini, "\n")




# Random Forest
set.seed(150)
fit3 <- randomForest(y ~ ., data = data.train, na.action=na.roughfix)


fit3_fitForest <- predict(fit3, newdata = data.test, type="prob")[,2]
fit3_fitForest.na <- as.data.frame(cbind(data.test$y, fit3_fitForest))
colnames(fit3_fitForest.na) <- c('y','pred')
fit3_fitForest.narm <- as.data.frame(na.omit(fit3_fitForest.na)) ##remove na
fit3_pred <- prediction(fit3_fitForest.narm$pred, fit3_fitForest.narm$y)
fit3_perf <- performance(fit3_pred, "tpr", "fpr")

#Plot variable importance
varImpPlot(fit3, main="Random Forest: Variable Importance")

# Model Performance plot
plot(fit3_perf,colorize=TRUE, lwd=2, main = "fit3 ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3)

# AUROC, KS and GINI
fit3_AUROC <- round(performance(fit3_pred, measure = "auc")@y.values[[1]]*100, 2)
fit3_KS <- round(max(attr(fit3_perf,'y.values')[[1]] - attr(fit3_perf,'x.values')[[1]])*100, 2)
fit3_Gini <- (2*fit3_AUROC - 100)
cat("AUROC: ",fit3_AUROC,"\tKS: ", fit3_KS, "\tGini:", fit3_Gini, "\n")


#Compare ROC Performance of the 3 models
plot(fit1_perf, col='blue', lty=1, main='ROCs: Model Performance Comparison') # Recursive partitioning
plot(fit2_perf, col='grey',lty=2, add=TRUE); # Conditional inference tree
plot(fit3_perf, col='red',lty=3, add=TRUE); # Random forest
lines(c(0,1),c(0,1),col = "green", lty = 4 ) # random line

legend(0.6,0.5,
       c('Fit1: Recursive partitioning (AUROC=79.85)','Fit2: Conditional inference tree (AUROC=91.86)','Fit3: Random forest (AUROC=79.3)', 'Random line'),
       col=c('blue','grey', 'red', 'green'),
       lwd=c(1,2,3), cex=0.7)


delong<-roc.test(fit1_AUROC,fit2_AUROC,method="delong") # DeLong test


