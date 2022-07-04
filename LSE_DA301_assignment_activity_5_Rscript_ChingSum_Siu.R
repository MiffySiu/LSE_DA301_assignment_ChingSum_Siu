

# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Assignment activity 5: Clean, manipulate, and visualise the data ----

############################################################################

# Scenario:
# The Turtle Games sales team has provided you with the data on the number 
# of video games units (in millions) sold in stores across Europe and North 
# America. You will help them predict the global sales (in millions) for 
# the next financial year for each of the video games. However, before 
# making predictions, you need to help them prepare the data for analysis.

# Objective:
# Prepare the data and ensure that it is ready for analysis by:
#   - ensuring there are no missing values
#   - modifying the data using string manipulation
#   - visualising the data to identify and understand possible trends 

############################################################################

# ---- 1. Prepare the workstation ----

## Import the library
library(tidyverse)
library(readr)         # import data sets
library(dplyr)         # data wrangling
library(tidyr)         # data wrangling
library(skimr)         # descriptive statistics summaries
library(DataExplorer)  # report in HTML

# Import the data set by re-setting my working directory
# getwd()
# setwd(dir ="C:/Users/Miffy/Desktop/Course 3 Assignment3/Data Sets")
# games_sales_original <- read.csv(file="games_sales.csv", header = TRUE)

# Import the data set by choosing the file manually
games_sales_original <- read.csv(file.choose(), header = TRUE)

# ---- 2. Data Manipulation ----

# Sense check the data set
head(games_sales_original)        # view first 6 rows
View(games_sales_original)        # Separate window in spreadsheet format
dim(games_sales_original)         # Number of rows and columns
as_tibble(games_sales_original)   # Check the data type

# Identify missing values in data set 
sum(is.na(games_sales_original)) 

# 2 rows with missing values is relatively insignificant
# Delete the two rows with missing values
games_sales <- na.omit(games_sales_original)

# Check the missing values again
sum(is.na(games_sales))

# Check the number of rows
dim(games_sales)

# Converting the values under Genre to lower case
games_sales <- games_sales %>%
  mutate(Genre = str_to_lower(Genre))

# Check the changes of
head(games_sales)

# Merge the values for the variables Genre and Platform
games_sales <- games_sales %>%
  mutate(Genre_Platform =  str_c(Genre, "_", Platform))

# Check the result
head(games_sales)


# ---- 3. Statistical Analysis ----

# to get the gist of the data
summary(games_sales)

DataExplorer::create_report(games_sales)

# ---- 4. Visualisation of skewness  ----
# Plot histograms to show skewness: right-skewed

# 4.1 Skewness of NA sales ====
ggplot(games_sales, aes(x = NA_Sales)) +  
  geom_histogram(stat = "count") +
  labs(title="The distribution of North America Sales",
       x = " Sales") 

# 4.2 Skewness of EU sales ====
ggplot(games_sales, aes(x = EU_Sales)) +  
  geom_histogram(stat = "count") +
  labs(title="The distribution of Sales in Europe",
       x = " Sales")

# 4.3 Skewness of Global sales ====
ggplot(games_sales, aes(x = Global_Sales)) +  
  geom_histogram(stat = "count") +
  labs(title="The distribution of Sales in global",
       x = " Sales")



# ---- 5. Correlations of global sales and other variables ----
# Scatter plot with a best-fit line 

# 5.1 Relationship between Global_Sales and North America Sales ----
# [[[ Problem: plot with outlier ]]]
ggplot(games_sales, aes(x = Global_Sales, y = NA_Sales)) + 
  geom_point() + geom_smooth() +
  labs(title="The relationship between global sales and North America sales",
       x = "Global Sales",
       y = "North America Sales")

# 5.2 Relationship between Global_Sales and Europe Sales ----
ggplot(games_sales, aes(x = Global_Sales, y = EU_Sales)) + 
  geom_point() + geom_smooth() +
  labs(title="The relationship between global sales and Europe sales",
       x = "Global Sales",
       y = "Europe Sales")

# 5.3 Relationship between Global Sales and Ranking ----
# [[[ Problem: only the top rankings matter ]]]
ggplot(games_sales, aes(x = Rank , y = Global_Sales)) + 
  geom_point() + geom_smooth() +
  labs(title="The relationship between global sales and ranking of video games",
       x = "Ranking of video games",
       y = "Global Sales")

# 5.4 Relationship of sales between North America and Europe ----
ggplot(games_sales, aes(x = NA_Sales, y = EU_Sales)) + 
  geom_point() + geom_smooth() +
  labs(title="The relationship between sales in North America and Europe",
       x = "Sales in North America",
       y = "Sales in Europe")


# 5.5 Relationship between Year and Global_Sales ----
# [[[ Problem: No correlation ]]]
ggplot(games_sales, aes(x = Year , y = Global_Sales)) + 
  geom_point() + geom_smooth()





# ---- 6. Distribution of other variables ----

# 6.1 Skewness of the genre ====
ggplot(games_sales, aes(x = Genre)) +  
  geom_histogram(stat = "count") +
  labs(title = "The distribution of the genre of games", 
       x = "Genre of games",    
       y = "Number of games")


# 6.2 The distribution of platforms ====

# Bar plot  [[[ problem ]]]
# The distribution of platforms involves too many data
# Very messy if it includes all the platforms
ggplot(games_sales, aes(x = Platform)) + 
       geom_histogram(stat = "count") +
       labs(title = "The distribution of the platforms")

# Construction a frequency table of the column "Platform"
byplatform <- table(games_sales$Platform)

# Change the table into a data frame
byplatform <- data.frame(byplatform)
byplatform

# Add the column names
names(byplatform) <- c("Platform", "Total_number")
byplatform

# Sort by total in descending order
byplatform <- arrange(byplatform, desc(Total_number))

# View the frequency table
View(byplatform)

# Subset those with more products on the ranking list
plot_byplatform <- filter(byplatform, Total_number > 300)
plot_byplatform

# Plot the distribution of platforms
# Plot by frequency table instead of data list
# Sort the order of bars by the total number in descending order
# Codes from reference page:
# https://www.rpubs.com/dvdunne/reorder_ggplot_barchart_axis
ggplot(plot_byplatform, aes(x=reorder(Platform, -Total_number), y=Total_number)) +
  geom_bar(stat = "identity") +
  labs(title = "The most popular platforms of games",    # Title and subtitle
       subtitle = "Sorted by having over 300 games in the ranking of 15,000 games",
       x = "Platforms of games",                         # The title of x-axis   
       y = "Number of games") +                          # The title of y-axis
  theme_gray()                                           # Theme layer
  

# 6.3 The distribution of year ====
# Construction a frequency table of the column "Year"
byyear <- table(games_sales$Year)

# Change the table into a data frame
byyear <- data.frame(byyear)
byyear

# Add the column names
names(byyear) <- c("Year", "Total_number")
byyear

# Drop the row with N/A values by specifying the index
plot_byyear <- byyear[-40, ]
plot_byyear

# Plot the distribution of year
ggplot(plot_byyear, aes(x = Year, y = Total_number)) +  
  geom_histogram(stat = "identity") +
  labs(title = "Number of games first launched by year",  # Title and subtitle
       subtitle = "Among the games in the ranking of 15,000 games", 
       x = "Year",                                        # The title of x-axis   
       y = "Number of games") +                           # The title of y-axis
  theme_gray()                                            # Theme layer


# 6.4 Distribution of the publishers ====
# [[ Problem: too messy with too many elements ]]
ggplot(games_sales, aes(x = Publisher)) +  
  geom_histogram(stat = "count") 




# ---- 7. Games with zero sales in North America and Europe ----

# 7.1 Sales in North America ====
# Construction a frequency table of the column "NA_Sales"
byNA_Sales <- table(games_sales$NA_Sales)

# Change the table into a data frame
byNA_Sales <- data.frame(byNA_Sales)

# Add the column names
names(byNA_Sales ) <- c("NA_Sales", "Total_number")
byNA_Sales 

# Plot the chart [[[ Problem: too messy ]]]
pie(byNA_Sales$Total_number)

# Get a table with sales >0 only
# Filter the data with sales greater than 0, and get the sum of total
byNA_Sales_pos <- filter(byNA_Sales, NA_Sales != 0)
Total_number <- sum(byNA_Sales_pos$Total_number)
pos_NA <- data.frame(Total_number)    # Change the value to a data frame
NA_Sales <- ">0"                      # Set an object to form the data frame

# Combine the sum of total and the object as a subset
byNA_Sales_post <- data.frame(NA_Sales, Total_number)
byNA_Sales_post

# Filter the data with sales equals to 0
byNA_Sales_zero <- filter(byNA_Sales, NA_Sales == 0)
byNA_Sales_zero

# Combine the two subsets into a data frame (sales = 0 and sales > 0)
Plot_byNA_Sales <- rbind(byNA_Sales_post, byNA_Sales_zero)
Plot_byNA_Sales


# Plot the pie chart to show the portion of 0 in sales in North America
# https://bookdown.org/dli/rguide/pie-chart.html#ggplot2-pie-chart
ggplot(data = Plot_byNA_Sales, aes(x = "", y = Total_number, fill = NA_Sales)) + 
  geom_bar(stat = "identity") + 
  labs(title = "The portion of games having zero sales in North America") +
  coord_polar("y")


# 7.2 Sales in Europe ====
# Construction a frequency table of the column "EU_Sales"
byEU_Sales <- table(games_sales$EU_Sales)

# Change the table into a data frame
byEU_Sales <- data.frame(byEU_Sales)

# Add the column names
names(byEU_Sales ) <- c("EU_Sales", "Total_number")
byEU_Sales 

# Plot the chart  [[[ Problem: very messy ]]]
pie(byEU_Sales$Total_number)

# Get a table with only >0 and 0
# Filter the data with sales greater than 0, and get the sum of total
byEU_Sales_pos <- filter(byEU_Sales, EU_Sales != 0)
Total_number <- sum(byEU_Sales_pos$Total_number)
pos_EU <- data.frame(Total_number)    # Change the value to a data frame
EU_Sales <- ">0"                      # Set an object to form the data frame

# Combine the sum of total and the object as a subset
byEU_Sales_post <- data.frame(EU_Sales, Total_number)
byEU_Sales_post

# Filter the data with sales equals to 0
byEU_Sales_zero <- filter(byEU_Sales, EU_Sales == 0)
byEU_Sales_zero

# Combine the two subsets (sales equals 0 and sales greater than 0)
Plot_byEU_Sales <- rbind(byEU_Sales_post, byEU_Sales_zero)
Plot_byEU_Sales


# Plot the pie chart to show the portion of 0 in sales in Europe
ggplot(data = Plot_byEU_Sales, aes(x = "", y = Total_number, fill = EU_Sales)) + 
  geom_bar(stat = "identity") + 
  labs(title = "The portion of games having zero sales in Europe") +
  coord_polar("y")


