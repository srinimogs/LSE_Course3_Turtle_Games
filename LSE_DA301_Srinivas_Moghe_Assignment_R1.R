## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(moments)

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header=TRUE)

 # Print and explore the data frame.
head(turtle_sales)
summary(turtle_sales)
dim(turtle_sales)
str(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales1 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(turtle_sales1)

# View the descriptive statistics.
summary(turtle_sales1)
dim(turtle_sales1)
str(turtle_sales1)
glimpse(turtle_sales1)

# convert datatype from integer to character or factor
turtle_sales1 <- mutate(turtle_sales1, Product=as.factor(Product))

# check to see if the data type has changed
glimpse(turtle_sales1)
str(turtle_sales1)
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Platform, EU_Sales, data=turtle_sales1, main='European Sales', xlab='Platform', ylab='European Union Sales',
      geom=c('point', 'jitter'))
qplot(Platform, NA_Sales, data=turtle_sales1, main='Nationwide Sales', 
             xlab='Platform', ylab='National Sales', geom=c('point', 'jitter'))
 qplot(Platform, Global_Sales, data=turtle_sales1, main='Wordlwide Sales', 
             xlab='Platform', ylab='World Sales',geom=c('point', 'jitter'))

## 2b) Histograms
# Create histograms.
qplot(Platform, data=turtle_sales1)

## 2c) Boxplots
# Create boxplots.
qplot(Platform, NA_Sales, data=turtle_sales1, geom='boxplot')
qplot(Platform, EU_Sales, data=turtle_sales1, geom='boxplot')
qplot(Platform, Global_Sales, data=turtle_sales1, geom='boxplot')

# Plot a bar plot to get insights
qplot(Platform, data=turtle_sales1, geom='bar')


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# from  the plots created it clearly shows that sales of Nintendo devices have been 
# the highest whilst devices made by Microsoft namely XBox and Play station have lagged behind
# with lesser than 20 million units sold. This shows that Turtle Games need to develop more games for the Nintendo platforms 





###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
# week 4 the dataset created was "turtle_sales1
View(turtle_sales1)
glimpse(turtle_sales1)
# Check output: Determine the min, max, and mean values.
# check for the maximum and minimum values of all sales
# to give an idea of the range of sales figures
# to get the values for National sales
min(turtle_sales1$NA_Sales)
max(turtle_sales1$NA_Sales)
mean(turtle_sales1$NA_Sales)
# to get values for sales in European Union
min(turtle_sales1$EU_Sales)
max(turtle_sales1$EU_Sales)
mean(turtle_sales1$EU_Sales)
# to get values for sales throughout the world
min(turtle_sales1$Global_Sales)
max(turtle_sales1$Global_Sales)
mean(turtle_sales1$Global_Sales)

# View the descriptive statistics.
summary(turtle_sales1)


###############################################################################

# 2. Determine the impact on sales per product_id.
# to use the group_by function import "dplyr" package from tidyverse

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product(platform).
# using the group_by function and sum to see the total per product(Platform)
# create a new dataframe to store the results. 
turtle_sales_prod <- turtle_sales1 %>% group_by(Platform) %>%
  summarise(sum_NA_Sales=sum(NA_Sales),
            sum_EU_Sales=sum(EU_Sales),
            sum_Global_Sales=sum(Global_Sales),
            .groups = 'drop') %>%
  as.data.frame()
 
# View the data frame.
View(turtle_sales_prod)

# Explore the data frame.
head(turtle_sales_prod)
dim(turtle_sales_prod)
View(turtle_sales_prod)
str(turtle_sales_prod)
summary(turtle_sales_prod)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# create a scatterplot for each sales column
# create a plot for National Sales
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_NA_Sales)) + 
  geom_point(color='red', size=2)
# create a plot for sales within European Union
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_EU_Sales)) + 
  geom_point(color='red', size=2)
# create a plot for sales world wide
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_Global_Sales)) + 
  geom_point(color='red', size=2)

# Create histograms.
# a histogram is created for the various products on sale
ggplot(turtle_sales1, aes(x=Platform)) + 
  geom_histogram(stat='count', fill='blue')

# Create boxplots for each of the sales columns 
# this is in order to understadn which graphs can be used
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_NA_Sales)) +
  geom_boxplot()
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_EU_Sales)) +
  geom_boxplot()
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_Global_Sales)) +
  geom_boxplot()

# as can be seen form the graphs created that scatterplots are the best
# to give insights in to the sales of various products
# through Turtle Games 

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(turtle_sales1$NA_Sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test on each of the sales columns
# this is to check for normal distribution
# the first sales column to be tested is National Sales
shapiro.test(turtle_sales_prod$sum_NA_Sales)
# the p-value is less that 0.05 and clearly shows data
# is not normally distributed

# the second column to be tested is European Sales
shapiro.test(turtle_sales_prod$sum_EU_Sales)
# the result shows that the p-value being less thatn 0.05 
# the data is not normally dis tributed

# the third column to be tested is World Wide Sales
shapiro.test(turtle_sales_prod$sum_Global_Sales)
# the result shows the data is not normally distributed
# as p-value is less than 0.05

## 3c) Determine Skewness and Kurtosis
# Skewness Test for all Sales Columns
skewness(turtle_sales_prod$sum_NA_Sales)
skewness(turtle_sales_prod$sum_EU_Sales)
skewness(turtle_sales_prod$sum_Global_Sales)
# the skewness tests reveal the figures to be positive and that means 
# the data is skewed to the right and hence is not normally distributed

#Kurtosis Test for all Sales Columns
kurtosis(turtle_sales_prod$sum_NA_Sales)
kurtosis(turtle_sales_prod$sum_EU_Sales)
kurtosis(turtle_sales_prod$sum_Global_Sales)
# the results are above 3 and that means the dataset has heavier tails
# than normal distribution. values were 4.5, 3.8, 3.5. 

## 3d) Determine correlation
# Determine correlation.
# the Shapiro Test has been carried out for all the sales columns
# and it is clear the data is not normally distributed
# now to check is any correlation exists between the sales
# columns and how strong it is.
cor(turtle_sales_prod$sum_NA_Sales, turtle_sales_prod$sum_EU_Sales)
# the above coorelation result is between 0 amd 1 with
# the value being closer to 1 and is 0.87. This suggests a positive
# correlation where change in one variable causes other variable 
# to change in the same direction
cor(turtle_sales_prod$sum_NA_Sales, turtle_sales_prod$sum_Global_Sales)
# the above equation yielded a result of 0.96 which is nearly 1 and 
# clearly shows a strong correlation which means any change in
# worldwide Sales causes a change in National sales.
cor(turtle_sales_prod$sum_EU_Sales, turtle_sales_prod$sum_Global_Sales)
# the result for this equation is also close to 1 being 0.96
# shows a strong correlation.

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# in response to the above question the scatterplot is the best plot
# to refer to as it clearly shows the popularity of the various game
# consoles being offered by Turtle Games and gives a clear understanding 
# of where the focus of the company should be in terms of development 
# of games with respect to the type of console and its popularity
# The other plots do not give any clear indication hence can be disregarded
# the analysis of the data shows the impact on National sales from 
# Global and Pan european sales which must be considered by 
# the company and its marketing and development teams. 
###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




