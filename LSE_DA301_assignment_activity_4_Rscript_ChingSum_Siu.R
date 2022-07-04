

# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Assignment activity 4: Visualise data to gather insights

############################################################################

# Scenario:
# Help with exploring the data to derive information on 
# certain customer groups, which in turn will provide them with 
# information to potentially improve their sales performance.

# Objective:
# 1. Determine the customer group that will most likely 
#    leave a review on the products they have purchased: 
#    Which age group submits the most reviews?
# 2. Determine the most expensive product purchased by
#    a particular group of customers: What is the most expensive 
#    Lego set purchased by customers who are at least 25 years old?

############################################################################


# ---- 1. Prepare the workstation ----

library(tidyverse)                    # Import the libraries

# getwd()                             # Check and re-set my working directory
# setwd(dir ="C:/Users/Miffy/Desktop/Course 3 Assignment3/Data Sets")
# lego <- read.csv(file="lego.csv",   # Import the data set
#                  header = TRUE)

# Import the data set
lego <- read.csv(file.choose(), header = TRUE)

head(lego)              # Check the data frame 
View(lego)              # View the whole data frame
as_tibble(lego)         # Check the data type
summary(lego)           # Get a overview of the data
sum(is.na(lego))        # Cheek for any missing values


# ---- 2. Visualisation ----

# Objective 1:  Which age group submits the most reviews? ----
# Create a histogram to show the number of reviews by ages
qplot(ages, num_reviews, data=lego, geom="col")

# Add the labels of axes and title, codes from the page:
# https://colauttilab.github.io/RCrashCourse/2_qplot.html
qplot(ages, num_reviews, data=lego, geom="col", 
      xlab="Age", ylab="Total number of reviews",
      main="The total number of reviews by age categories of Lego sets")


# [[[ Problem: Not clear with the scale of x-axis ]]]


# Alternative plot: a box plot
# Create a box plot to get a better picture [[ FAILED ONE ]]
qplot(ages, num_reviews, data=lego, geom="boxplot")     ### problem: data type

# Change the data type of "ages" from integer to character
# Force it to be categorical variable for comparison
lego2 <- mutate(lego, ages = as.character(ages))  

# Check the changes in data type
glimpse(lego2)

# Create a box plot to get a better picture [[ FAILED ONE ]]
qplot(ages, num_reviews, data=lego2, geom="boxplot")    ### problem: order

# Filter the ages below 10 to adjust order manually
lego2_age <- filter(lego, ages < 10)

# Assign 0 before these numbers
lego2_age$ages <- paste("0", lego2_age$ages, sep = "", collapse = NULL)

# Check the changes
head(lego2_age)

# Filter the ages need not to be changed as another data set
lego2_age2 <- filter(lego, ages >= 10)

# Combine the two data sets
lego2 <- rbind(lego2_age,lego2_age2)

# Check the changes
View(lego2)

# Create a box plot to show a clear distribution in number of reviews by ages
qplot(ages, num_reviews, data=lego2, geom="boxplot")

# Add the labels of axes and title
qplot(ages, num_reviews, data=lego2, geom="boxplot",
      xlab="Age", ylab="Number of reviews",
      main="The number of reviews by age categories of Lego sets")

### Answer to objective 1: Age 8


# Objective 2: What is the most expensive Lego set for ages >= 25 years old? ----

# Create a subset of data for ages >= 25
lego_age25 <- lego2[lego2$ages >= 25, ]

# Create a scatter plot with jitter to show the prices of Lego products 
# [[[ Problem: very messy ]]]
qplot(ages, list_price, data=lego_age25, geom="jitter",
      xlab="Age", ylab="Listed price",
      main="The prices of Lego sets with ages cater to 25-year-old or above")

# Create a box plot to show the ranges of prices by ages
qplot(ages, list_price, data=lego_age25, geom="boxplot",
      xlab="Age", ylab="Listed price",
      main="The prices of Lego sets with ages cater to 25-year-old or above")


### Answer to objective 2: Not easy to tell by the graph

# Confirm in numeric answer by an aggregate function
# Show the highest price of products at age 25 to 30
obj <- aggregate(list_price~ages, lego_age25, max)
obj <- arrange(obj, desc(list_price))
obj

### Answer to objective 2: Age 29 with the price 259.87

# Sort the Lego set with the price 259.87
# This Lego set is not popular, having only 6 reviews
obj2 <- filter(lego_age25, list_price == 259.87)
obj2

# The expensive Lego set with great number of reviews and aged 25 or above
# Assume great number of reviews represent popularity
# Sort the Lego set with reviews received over 50
lego_age25_popular <- filter(lego_age25, num_reviews > 50)
lego_age25_popular

# Sort the list by listed price
lego_age25_popular <- lego_age25_popular %>%
  arrange(desc(list_price))

# Check the result
head(lego_age25_popular)

### Additional information to objective 2:
### The relatively popular and expensive Lego set: Age 29 with the price 246.87

