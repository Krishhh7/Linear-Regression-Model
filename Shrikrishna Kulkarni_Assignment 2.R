# Week 1A Assignment: SIMPLE LINEAR REGRESSION USING R

rm(list = ls()) # remove previously stored variables
library(caTools) #Importing the library for data manipulation

# Data set formation
data_f <- data.frame( years_exp=c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
                salary=c(39343.00, 46205.00, 37731.00, 43525.00, 39891.00, 56642.00, 
                         60150.00, 54445.00, 64445.00, 57189.00))

head(data_f)

#Checking the summary
summary(data_f)

#Scatter plot to see the relationship

plot(data_f$years_exp, salary, main = "Years of Experience VS Salary", xlab = "Years of Experience",
     ylab = "Salary")

#Checking for Outliers using Box Plot

boxplot(salary) # No tiny circle spotted in the box plot. No Outliers

#Splitting the data into Train and Test data
set.seed(123) # setting seed for reproducibility
splt_sample <- sample.split(salary, SplitRatio = 0.60) #Splitting the data into 60:40

train_set  <- subset(data_f, splt_sample == TRUE) #Assigning 60% of data as Training set
train_set
test_set  <- subset(data_f, splt_sample == FALSE) #Assigning 40% of the data as Test set
test_set


#Linear Regression Model:

LRM= lm(formula = salary ~ years_exp, data = train_set)

summary(LRM)

