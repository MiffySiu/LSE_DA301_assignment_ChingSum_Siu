

# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Assignment activity 6: Making recommendations to the business ---- 

############################################################################

# Scenario:
# Continuing from the previous assignment activity, Turtle Games wants your 
# help with predicting the sales of the video games for the next financial 
# year based on the available sales data from Europe and North America.

# Objective:
# Determine the optimal global sales (in millions) for the next financial 
# year for each of the video games.

############################################################################


# ---- 1. Import the data and check correlation ----

## Import the library
library(tidyverse)
library(readr)         # import data sets
library(dplyr)         # data wrangling
library(tidyr)         # data wrangling

# Import the data set by re-setting my working directory
# getwd()
# setwd(dir ="C:/Users/Miffy/Desktop/Course 3 Assignment3/Data Sets")
# games_sales_original <- read.csv(file="games_sales.csv", header = TRUE)

# Import the data set by choosing the file manually
games_sales_original <- read.csv(file.choose(), header = TRUE)

# Sense check the data set
head(games_sales_original)        # view first 6 rows
View(games_sales_original)        # Separate window in spreadsheet format

# Identify missing values in data set 
sum(is.na(games_sales_original)) 

# 2 rows with missing values is relatively insignificant
# Delete the two rows with missing values
games_sales <- na.omit(games_sales_original)

# Check the missing values again
sum(is.na(games_sales))

# Check the number of rows
as_tibble(games_sales)


# Return summary statistics of the data set
summary(games_sales)

# Subset the data with only the numeric columns, codes from:
# https://www.tutorialspoint.com/how-to-select-only-numeric-columns-from-an-r-data-frame
numeric_only <- unlist(lapply(games_sales, is.numeric))
games_sales_num <- games_sales[ , numeric_only]
View(games_sales_num)

# compute the correlation between all pairs of variables
cor(games_sales_num)


# ---- 2. Create a new regression model ----

# Create a regression model with an object called model1
# lm(y ~ x1 + x2, data=mydata)
model1 = lm(Global_Sales ~ NA_Sales + EU_Sales, data=games_sales_num)
summary(model1)   # Print the summary statistics


# Create a regression model with all variables
# model2 = lm(Global_Sales ~ NA_Sales + EU_Sales + Rank, data=games_sales_num)
model2 = lm(Global_Sales ~ ., data=games_sales_num)
summary(model2) 

model3 = lm(Global_Sales ~ NA_Sales, data=games_sales_num)
summary(model3) 

model4 = lm(Global_Sales ~ EU_Sales, data=games_sales_num)
summary(model4) 

# The R-squared value in model 1 is high enough (0.9648)
# By comparing the R-squared values, model 2 is the highest
# But it just add a little value in R-squared value and adjusted one
# Model 2 would be picked for prediction


# ---- 3. Prediction of sales ----

# Get the sum of sales in North America
sum_NA <- sum(games_sales_num$NA_Sales)
# Get the sum of sales in Europe
sum_EU <- sum(games_sales_num$EU_Sales)

# Create a new data frame to predict the sales
sales_predict = data.frame(NA_Sales = sum_NA, EU_Sales = sum_EU)
sales_predict

# Create a new object to test and specify the predict function
predictTest = predict(model1, newdata = sales_predict, interval = 'confidence')
predictTest

# The optimal global sales is 8340.804 in millions of units
# for the next financial year.