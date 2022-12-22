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
install.packages("psych")
library(psych)
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
# to get the values for North American sales
min(turtle_sales1$NA_Sales)
max(turtle_sales1$NA_Sales)
mean(turtle_sales1$NA_Sales)
# for North American Sales min=0, max=34.02, mean=2.515
# to get values for sales in European Union
min(turtle_sales1$EU_Sales)
max(turtle_sales1$EU_Sales)
mean(turtle_sales1$EU_Sales)
# for European Sales min=0, max=23.80, mean=1.643
# to get values for sales throughout the world
min(turtle_sales1$Global_Sales)
max(turtle_sales1$Global_Sales)
mean(turtle_sales1$Global_Sales)
# for Global Sales min=0.01, max=67.85, mean=5.334

# View the descriptive statistics.
summary(turtle_sales1)


###############################################################################

# 2. Determine the impact on sales per product_id.
# to use the group_by function import "dplyr" package from tidyverse

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product(platform).
# using the group_by function and sum to see the total per product(Platform)
# create a new data frame to store the results. 
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
# create a plot for North American Sales
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_NA_Sales)) + 
  geom_point(color='red', size=2)
# create a plot for sales within European Union
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_EU_Sales)) + 
  geom_point(color='blue', size=2)
# create a plot for sales world wide
ggplot(turtle_sales_prod, aes(x=Platform, y=sum_Global_Sales)) + 
  geom_point(color='blue', size=2)

# Create histograms.
# a histogram is created for the various products on sale
ggplot(turtle_sales1, aes(x=Platform)) + 
  geom_histogram(stat='count', fill='blue')

# Create boxplots for each of the sales columns 
# this is in order to understand which graphs can be used
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
qqline(turtle_sales$NA_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test on each of the sales columns
# this is to check for normal distribution
# the first sales column to be tested is North American Sales
shapiro.test(turtle_sales_prod$sum_NA_Sales)
# the p-value is less that 0.05 and clearly shows data
# is not normally distributed

# the second column to be tested is European Sales
shapiro.test(turtle_sales_prod$sum_EU_Sales)
# the result shows that the p-value being less thatn 0.05 
# the data is not normally distributed

# the third column to be tested is World Wide Sales
shapiro.test(turtle_sales_prod$sum_Global_Sales)
# the result shows the data is not normally distributed
# as p-value is less than 0.05

## 3c) Determine Skewness and Kurtosis
# Skewness Test for all Sales Columns
skewness(turtle_sales_prod$sum_NA_Sales)
skewness(turtle_sales_prod$sum_EU_Sales)
skewness(turtle_sales_prod$sum_Global_Sales)
# the skewness tests reveal all three figures to be positive and that means 
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
# worldwide Sales causes a change in North American sales.
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
# the analysis of the data shows the impact on North American sales from 
# Global and Pan european sales which must be considered by 
# the company and its marketing and development teams. 
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
View(turtle_sales_prod)

# Determine a summary of the data frame.
summary(turtle_sales_prod)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(turtle_sales$NA_Sales, turtle_sales$EU_Sales)
# the above equation yielded a result of 0.70 which shows 
# a good correlation between North American and European Sales
cor(turtle_sales$NA_Sales, turtle_sales$Global_Sales)
# the above calculation shows a even better correlation of 0.93
# between North American and Global Sales

# create a plot to understand the data - this is before 
# improving the linearity of the dataset between North American Sales
# and European Sales 
plot(turtle_sales$NA_Sales, turtle_sales$EU_Sales)

# improve linearity by using the sqrt function
sqrtEUSales <- sqrt(turtle_sales$EU_Sales)

# plot a graph to see the results after improvement of
# linearity of the data
plot(turtle_sales$NA_Sales, sqrtEUSales)

# create a plot to see the dataset before improvement of linearity
# between North American and Global Sales
plot(turtle_sales$NA_Sales, turtle_sales$Global_Sales)

# improve linearity by using the sqrt function
sqrtGlobal <- sqrt(turtle_sales$Global_Sales)

# Now create a plot to see the difference after linearity
plot(turtle_sales$NA_Sales, sqrtGlobal)

# Create a linear regression model on the original data which 
# is "Turtle_Sales" dataset provided by the company.
# create a regression model for 2 columns North American Sales
# and European Sales.
turtleLegMod <- lm(turtle_sales$EU_Sales~turtle_sales$NA_Sales)

# view the summary of the regression model
summary(turtleLegMod)
# the R squared value of this model is 0.4978 and lower hence 
# not a good model

# Basic visualisation.
qqnorm(residuals(turtleLegMod))
qqline(residuals(turtleLegMod), col='red')

# create another model for North American Sales and European Sales
# use the par function to set the size of the plot
par(mar=c(0.5, 0.5, 0.5, 0.5))

turtleLegMod2 <- lm(sqrtEUSales~turtle_sales$NA_Sales)
summary(turtleLegMod2)

# basic visualisation
qqnorm(residuals(turtleLegMod2))
qqline(residuals(turtleLegMod2), col='red')

# compare the two plots 
plot(turtle_sales$NA_Sales, turtle_sales$EU_Sales)
abline(coefficients(turtleLegMod), col='red')

plot(turtle_sales$NA_Sales, sqrtEUSales)
abline(coefficients(turtleLegMod2), col='blue')
# from the above 2 plots for European Sales it can be seen that 
# plot "turtleLegMod2" is better


## create a regression model for 2 columns North American Sales
# and Global Sales.
turtleLegGlobal <- lm(turtle_sales$Global_Sales~turtle_sales$NA_Sales)

# view the summary of the regression model
summary(turtleLegGlobal)
# the R squared value of this model is 0.87 which tends towards 1 and is good.

## 2b) Create a plot of the simple linear regression model created above.
qqnorm(residuals(turtleLegGlobal))
qqline(residuals(turtleLegGlobal), col='red')

turtleLegGlobal1 <- lm(sqrtGlobal~turtle_sales$NA_Sales)
summary(turtleLegGlobal1)

# create a plot for the simple linear regression model
qqnorm(residuals(turtleLegGlobal1))
qqline(residuals(turtleLegGlobal1), col='blue')

# compare the two models
plot(turtle_sales$NA_Sales, turtle_sales$Global_Sales)
abline(coefficients(turtleLegGlobal), col='red')

plot(turtle_sales$NA_Sales, sqrtGlobal)
abline(coefficients(turtleLegGlobal1), col='red')
# from the above plot it can be seen that "turtleLegGlobal" model is better

# the p-values of both models were the same hence there is a correlation 
# between all 3 variables but better between North American Sales and 
# Global Sales, as is demonstrated by the R squared values.
# Hence from the the 2 R squared values it is clear that turtleLegMod1 
# is a better model that fits and can be used for further analysis. This model
# has the 2 variables which contains North American Sales figures and 
# Global Sales figures which give a better indication of the direction 
# of the games popularity. 
 

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
dim(turtle_sales)

# remove the non numbneric columns for analysis
turtle_sales1 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# view the data frame to check if columns removed
view(turtle_sales1)

# remove the Product and the Platform variables as they are not needed
# for regression analysis
turtle_sales2 <- select(turtle_sales1, -Product, -Platform)

# check and see if removel of variables/columns has been done
view(turtle_sales2)
# determine the value of the correlation between the variables
# and see the degree of correlation
cor(turtle_sales2)

# install and import the "psych" package to visualise the 
# correlation between the variables/columns
# use par function to reduce he size of the plot to fit the window
par(mar=c(0.3, 0.3, 0.3, 0.3))

# create a correlation plot to check if positive ir negative
corPlot(turtle_sales2)
# the blue colours in the plot clearly show a strong positive correlation
# between all the columns/variables, with different shades
# of blue, correlation is high.

# Multiple linear regression model.
# create models based on dependent variable as North American Sales
# and non-dependent or Y variables as European Sales and 
# Global Sales

# first model is with North American Sales and European Sales
turtle_mlr <- lm(NA_Sales~EU_Sales, data=turtle_sales2)
summary(turtle_mlr)

# the results of this model shows a weak correlation with 
# Multiple R squared value of 0.4978 and Adjusted R squared value of ~0.4963

# next add another variable to the model to see the results
turtle_mlr1 <- lm(NA_Sales~EU_Sales+Global_Sales, data=turtle_sales2)
summary(turtle_mlr1)
# the summary shows that the multiple R squared value is 0.9316
# and the Adjusted R squared value is 0.9312


# now remove the European sales and try only with Global Sales
turtle_mlr2 <- lm(NA_Sales~Global_Sales, data=turtle_sales2)
summary(turtle_mlr2)
# the summary shows that after taking out the European variable the value drops to
# mutiple R squared value = 0.8741 and Adjusted R squared value= 0.8738
# As can be seen from above the model "turtle_mlr2" is the strongest
# of the three created so far. 
# the model shows a strong correlation between North American Sales 
# and Global Sales and proves that trends across the world influence 
# the american market, whilst the European market are more in isolation
# and are in themselves influenced by the Global market. 

###############################################################################

# 4. Predictions based on given values
# create a multi linear model for the dataset turtle_sales_prod 
model <- lm(sum_Global_Sales~sum_NA_Sales+sum_EU_Sales, 
            data=turtle_sales_prod)
# check the summary for the coefficients estimate values
summary(model)
# create a new data frame for the specific values
new <-data.frame(sum_NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08), 
                 sum_EU_Sales=c(23.80, 1.56, 0.65, 0.97, 0.52))
# use the predict function to see the value for Global Sales
predict(model, newdata = new)
# the prediction of Global sales for the above sales figures 
# are 75.13, 10.88, 8.23, 8.25, 28.47

# the above figures are correct as verified 
# by the following formula
# intercept + estimate(sum_NA_Sales) * sum_NA_Sales value
# + estimate(sum_EU_Sales) * sum_EU_Sales value
# for example: 4.45 + 1.05 * 34.02 + 1.46 * 23.80 = 75.13

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The multi linear model created as above works well and it can accurately
# predict the Global Sales with regards to given values of North American 
# Sales and European Sales. This will help the marketing team at Turtle Games
# to understand the sales trend when they have figures for one market segment.
# this will help them to plan their campaigns effectively to maximise
# sales across the world.  


###############################################################################
###############################################################################




