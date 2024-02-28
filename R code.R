---
Title:   "MA334_2312128.R"
Author:  "Pathakoti Rahul"
Date:    "2024-01-05"
Output:  html_document
---
  


if(!is.null(dev.list())) dev.off()  # This code is to clear out the past 
rm(list = ls())
cat("\014")


#Necessary Library to be installed  
library(DescTools)
library(dplyr)
library(tidyr)
library(moments)  
library(reshape2) 
par(mfrow=c(1, 1))



# This code allows to read the csv file and choose the dataset of tabular format in anywhere from our device 

Proj_data_all_11 = read.csv(file.choose())




# Variable definition for all dataset

# 1) "Location":(character) Location with code which can be refereed through easting and northing                

# 2) "Bees":(numeric) "Bird":(numeric) "Bryophytes":(numeric) "Butterflies":(numeric) "Carabids":(numeric) "Hoverflies":(numeric)        "Isopods":(numeric)  "Ladybirds":(numeric) "Macromoths":(numeric) "Grasshoppers_._Crickets":(numeric) "Vascular_plants":(numeric)    The data of above 11 variables are species of Taxonomic groups.   

# 13)"Easting": (integer)  Vertical lines on the map are called "eastings," and they increase in value as you travel east. (Eastings    along the corridor)         

# 14)"Northing":(integer)  horizontal lines are called northings as they increase in value as you travel north on the map.(Northings    up the stairs)        

# 15)"dominantLandClass": (character) Summary of land classes taken from 2007 ITE land classification (Bunce et al.      2007). These   45 land classes were used as the basis for the analysis in order to assign specific environmental         zones which are indicative   of areas under similar abiotic conditions      

# 16)"ecologicalStatus":(numeric) "ecological status" refers to the overall health and condition of ecosystems. It is assessed using    biodiversity indicators, considering a broad range of species across various taxonomic groups.       

# 17)"period": (character)  Distinct time periods:Y70:-(1970–1990) and Y00:-(2000–2013)



# MY ALLOCATED SET OF 5 TAXINOMIC GROUPS. 

eco_selected_names = c("Butterflies","Hoverflies","Ladybirds","Grasshoppers_._Crickets","Vascular_plants")



#  Creating a BD5 group and assigning my 5 variables

## ("Butterflies","Hoverflies","Ladybirds","Grasshoppers_._Crickets","Vascular_plants")

BD5 = data.frame(Butterflies = 
                   Proj_data_all_11$Butterflies,Hoverflies = 
                   Proj_data_all_11$Hoverflies,Ladybirds = 
                   Proj_data_all_11$Ladybirds,Grasshoppers_._Crickets = 
                   Proj_data_all_11$Grasshoppers_._Crickets,Vascular_plants= 
                   Proj_data_all_11$Vascular_plants)

# shows tabular dataset of BD5 
View(BD5)       


# it will calculate the mean for the selected Bio Diversity measure  
mean_selected = rowMeans(
  Proj_data_all_11[,eco_selected_names])

# it will create a new data frame(Proj_data) with selected columns with new variable (eco_status_5) for mean values
Proj_data = Proj_data_all_11%>%select("Location",eco_selected_names,
                                      "Easting","Northing","dominantLandClass",      
                                      "ecologicalStatus","period")%>%  # Including other variables      
  mutate(eco_status_5=mean_selected) 


#  shows coloumn name's of dataset in Proj_data  
names(Proj_data)  

# it converts the variable from character to factor 
Proj_data$period <- as.factor(Proj_data$period) 

# it converts the variable from character to factor
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass) 

# Shows the data-type of period variable after converting it
class(Proj_data$period)

# Shows the data-type of dominantLandClass after converting it
class(Proj_data$dominantLandClass)


#shows tabular dataset of Proj_data
View(Proj_data)

#shows tabular dataset of Proj_data_all_11
View(Proj_data_all_11)


# Necessary library to install for usage of code
library(e1071)
library(dplyr)

# a) Present a table which provides the following summary statistics for each of the five
# variables in your BD5 group. This table must provide the 7 statistics:
# The six statistics which are commonly found using the summary() command, namely Min, 1st Quarter, Median, Mean, 3rd Quarter, Max

# This creates a table of univariate stats  

# this will create an empty data frame named 'table'
table <- data.frame()

# Looping with columns 2 to 6 in 'Proj_data'
for (i in c(2:6)) {
  # it will bind a new row to 'table' with summary statistics for the current column
  table <- rbind(table,
                 c(names(Proj_data)[i],
                   round(min(Proj_data[, i], na.rm = TRUE), digits = 2),
                   round(quantile(Proj_data[, i], 0.25, na.rm = TRUE), digits = 2),
                   round(median(Proj_data[, i], na.rm = TRUE), digits = 2),
                   round(mean(Proj_data[, i], na.rm = TRUE), digits = 2),
                   round(quantile(Proj_data[, i], 0.75, na.rm = TRUE), digits = 2),
                   round(max(Proj_data[, i], na.rm = TRUE), digits = 2)
                 ))
}

# this will assign a new column names to the resulting 'table' data frame
colnames(table) <- c("My_group", "min", "Quantile_first", "Median", "Mean", "Quantile_third", "Max")

# this will arrange the rows of the 'table' data frame based on specified columns, gives output for summary statistics which i have mentioned 
table %>% arrange(My_group, min, Quantile_first, Median, Mean, Quantile_third, Max)



# b) In addition, add a column to this table which provides a new statistic, that is the 20% Winsorized mean for all the variables in the my group.  

# this code is to create a function to calculate Winsorized mean
winsorized_mean <- function(x, trim = 0.2) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(mean(x, na.rm = TRUE))
}

# i am Initializing an empty data frame
table <- data.frame()

# Looping through numeric columns (assuming they are from 2 to 6)
for (i in c(2:6)) {
  col_name <- names(Proj_data)[i]
  
  # it will Calculate summary statistics for all given information with rounding off to 2 digits
  summary_stats <- c(
    round(min(Proj_data[, col_name], na.rm = TRUE), digits = 2),
    round(quantile(Proj_data[, col_name], 0.25, na.rm = TRUE), digits = 2),
    round(median(Proj_data[, col_name], na.rm = TRUE), digits = 2),
    round(mean(Proj_data[, col_name], na.rm = TRUE), digits = 2),
    round(quantile(Proj_data[, col_name], 0.75, na.rm = TRUE), digits = 2),
    round(max(Proj_data[, col_name], na.rm = TRUE), digits = 2),
    round(winsorized_mean(Proj_data[, col_name], trim = 0.2), digits = 2)  # Add Winsorized mean
  )
  
  # this will Combine all statistics
  row_data <- c(col_name, summary_stats)
  
  # it will append the row to the table
  table <- rbind(table, row_data)
}

# it will add column names
colnames(table) <- c("My_group", "Min", "Quantile_first", "Median", "Mean", "Quantile_third", "Max", "Winsorized_Mean")

# Print the table consisting of "My_group", "Min", "Quantile_first", "Median", "Mean", "Quantile_third", "Max", "Winsorized_Mean"
print(table)


#2) Estimate the correlations between all pairs of variables in 5 variables and put them into a table which 
# consists #of 5 rows and 5 columns where each row/column represents a BD5 variable.
# Calculate the correlation matrix


# Using this cor() function to calculate the correlations of BD5 variables 
cor_matrix = cor(BD5)

# This will create a table with 5 rows and 5 columns
table_data = cor_matrix[1:5, 1:5]

# Print the resulting table
print(table_data)

# shows the correlation table of my 5 variables 
library(corrplot)

# shows the corrplot in graphical way
corrplot(cor(table_data),method = "number",number.cex = .80,tl.cex = .80,tl.col = "Black")


# 3) I will perform the Boxplot for only one variable in BD5(Vascular_plants)

# This will define box_color
box_color <- "grey"

# It will create a boxplot
boxplot(BD5$Vascular_plants, main = "Boxplot for Vascularplants", ylab = "Variable Values", col = box_color)

# It will calculate boxplot statistics
stats <- boxplot.stats(BD5$Vascular_plants)

# Opens a new plot
par(new = TRUE)

# This will add points for mean and median
points(rep(1, length(mean(BD5$Vascular_plants))), mean(BD5$Vascular_plants), col = "blue", pch = 30)
points(rep(1, length(median(BD5$Vascular_plants))), median(BD5$Vascular_plants), col = box_color, pch = 30)

# This will add lines for quartiles(Q3,Q1), median, min, and max
abline(h = stats$stats[c(1, 2, 3, 4, 5)], col = c("red", "orange", box_color, "orange", "red"), lty = 2)

# It will add text labels for statistical values
text(1.2, stats$stats[1], paste("Min:", round(stats$stats[1], 2)), pos = 4, col = "red")
text(1.2, stats$stats[2], paste("Q1:", round(stats$stats[2], 2)), pos = 4, col = "orange")
text(1.2, stats$stats[3], paste("Median:", round(stats$stats[3], 2)), pos = 4, col = box_color)
text(1.2, stats$stats[4], paste("Q3:", round(stats$stats[4], 2)), pos = 4, col = "orange")
text(1.2, stats$stats[5], paste("Max:", round(stats$stats[5], 2)), pos = 4, col = "red")



###Hypothesis tests

#Perform two distinct types of hypothesis test aside from the linear regression results.You may choose any tests within the #scope of the module using the given data set.You should precisely report the p values and also an interpretation of the #results.


# This will set up the plotting area
par(mfrow=c(1, 1))

# This will create a quantile-quantile plot to compare the two samples
qqplot(Proj_data$eco_status_5, Proj_data$ecologicalStatus)

# It will add a reference line to the QQ plot
abline(0, 1, col="red")

# this calculates empirical cumulative distribution functions (ECDF) for both samples.
BD5_cdf <- ecdf(Proj_data$eco_status_5)
BD11_cdf <- ecdf(Proj_data$ecologicalStatus)

# this Plot give the cumulative distribution functions on the same graph
plot(BD11_cdf, col="red")
lines(BD5_cdf, col="green")

# it willPerform the Kolmogorov-Smirnov (KS) test to compare the distributions
ks.test(Proj_data$eco_status_5, Proj_data$ecologicalStatus)



# The Given p-value
p_value <- 0.0001922
significance_level <- 0.05

# To compare p-value to significance level for KS-test 
if (p_value < significance_level) {
  cat("Reject the null hypothesis. There is evidence of a significant difference between the distributions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude a significant difference between the distributions.\n")
}


# This will Select specific columns and pivot the data wider:

# I am spliting the data of period for comparsion of Histogram 
names(Proj_data)
Proj_data_split <- Proj_data %>% 
  select(Location, period, eco_status_5) %>%
  pivot_wider(names_from = period, values_from = eco_status_5) %>%
  mutate(BD5_change = Y00 - Y70)

# shows the tabular dataframe
View(Proj_data_split)


# this will Creates a histogram of the BD5_change variable:
hist(Proj_data_split$BD5_change)

# it will Extract the 'BD5_change' variable:
BD5_change <- Proj_data_split %>% pull(BD5_change)

# this Perform a t-test on the 'BD5_change' variable with the null hypothesis H0: μ = 0:
t.test(BD5_change, mu = 0)


# The Given p-value BD5 
p_value <- 1.868e-13
significance_level <- 0.05


# Using simple if else loop for knowing the relation of significance level with respect to P- value 
# Compare p-value to significance level
if (p_value < significance_level) {
  cat("Reject the null hypothesis. There is evidence of a significant difference between the distributions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude a significant difference between the distributions.\n")
}


# I am Repeating the analysis for the 'BD11_change' variable:
names(Proj_data)
Proj_data_all_11_split <- Proj_data %>% 
  select(Location, period, ecologicalStatus) %>%
  pivot_wider(names_from = period, values_from = ecologicalStatus) %>%
  mutate(BD11_change = Y00 - Y70)


# It will create a histogram of the BD11_change variable:
hist(Proj_data_all_11_split$BD11_change)

# It will extract the 'BD11_change' variable:
BD11_change <- Proj_data_all_11_split %>% pull(BD11_change)


# It will perform a t-test on the 'BD11_change' variable with the null hypothesis H0: μ = 0:
t.test(BD11_change, mu = 0)


# The Given p-value BD11 
p_value <- 2.2e-16
significance_level <- 0.05

# Using simple if else loop for knowing the relation of significance level with respect to P- value 
# Compare p-value to significance level
if (p_value < significance_level) {
  cat("Reject the null hypothesis. There is evidence of a significant difference between the distributions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude a significant difference between the distributions.\n")
}




#       Contingency table/comparing categorical variables


# This will select columns 'Location' and 'BD11_change' from Proj_data_all_11_split:
Eco_change_BD11 <- Proj_data_all_11_split %>% select(Location, BD11_change)

# This will select columns 'Location' and 'BD5_change' from Proj_data_split:
Eco_change_BD5 <- Proj_data_split %>% select(Location, BD5_change)

# It will Inner join the two dataframes on the 'Location' column:
Both_eco_change <- inner_join(Eco_change_BD11, Eco_change_BD5, by = "Location")

# It will add columns 'BD11up' and 'BD5up' to Both_eco_change:
Both_eco_change <- Both_eco_change %>%
  mutate(BD11up = ifelse(BD11_change > 0, 1, 0)) %>%
  mutate(BD5up = ifelse(BD5_change > 0, 1, 0))

# Shows the tabular dataframe
View(Both_eco_change)

# It will shows the distribution of 'BD11up':
table(Both_eco_change$BD11up)

# It will shows the distribution of 'BD5up':
table(Both_eco_change$BD5up)


library(lmtest)


# This will Create a contingency table to interpret the joint distribution
Table_up_down <- table(Both_eco_change$BD11up, Both_eco_change$BD5up)

# It will Rename the columns and rows for clarity
colnames(Table_up_down) <- c("down", "up")
rownames(Table_up_down) <- c("down", "up")

# It will Display the contingency table
Table_up_down

# It will Perform a log-likelihood ratio test (chi-squared test)
chisq.test(Table_up_down)

# This will Display a summary, which also provides the chi-squared test results (similar p-value)
summary(Table_up_down)


# It will Load necessary libraries
install.packages(c("epiR", "pROC"))
library(epiR)
library(pROC)
require(epiR)
require(pROC)


#Creating a Contingency table
Table_up_down <- table(Both_eco_change$BD11up, Both_eco_change$BD5up)


# Confusion matrix values TN-True Negative, FP-False positive, FN-False Negative,TP-True Positive
TN <- 1254
FP <- 384
FN <- 248
TP <- 754

# it will Calculate Odds Ratio manually
odds_ratio <- (TP * TN) / (FP * FN)

# This will Print results
print(paste("Odds Ratio:", odds_ratio))

# This will Calculate sensitivity
sensitivity <- TP / (TP + FN)

# This will Calculate specificity
specificity <- TN / (TN + FP)

# This will Calculate Youden's Index
youden_index <- sensitivity + specificity - 1

# Print results
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
print(paste("Youden's Index:", youden_index))


# ROC Curve

roc_curve <- roc(Both_eco_change$BD11up, Both_eco_change$BD5up)
auc <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, cex.lab = 1.2, cex.main = 1.4)
text(0.7, 0.2, paste("AUC =", round(auc, 2)), col = "blue", cex = 1.2)



# Simple Linear Regression 

# This will Load required libraries
library(ggplot2)
library(dplyr)

# This will Display column names of the data
names(Proj_data_all_11)
names(Proj_data)  # I choose Bird as it's not one of the five variables  

# Simple linear regression part of the specified assignment

#It will Create a scatter plot
plot(Proj_data_all_11$Bird ~ Proj_data$eco_status_5)

#This will add a red line with intercept 0 and slope 1 (identity line)
abline(0, 1, col = "red")

# Fit a linear regression model
lin_mod <- lm(Proj_data_all_11$Bird ~ Proj_data$eco_status_5)

# This will add the regression line in green
abline(lin_mod, col = "green")

# It Shows the summary statistics of the linear model
summary(lin_mod)

# Diagnostic plots

# The Plot shows residuals against fitted values
plot(jitter(fitted(lin_mod)), residuals(lin_mod), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "blue")

# Quantile-quantile plot
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod), col = "red")



# multiple linear regression of BD1 on all five  

# Multiple linear regression of BD1 on all five variables

# It will Build the model
lmMod <- lm(Proj_data_all_11$Bird ~ .,
            data = Proj_data[c(eco_selected_names)], y = TRUE)

# It Display model summary
summary(lmMod)

# It will Calculate correlation between fitted values and actual values
cor(lmMod$fitted.values, lmMod$y)

# Displays Plot the actual vs. predicted values
plot(Proj_data_all_11$Bird ~ lmMod$fitted.values)
abline(0, 1, col = "red")

# It will Calculate residuals for the test data
mis_fit_to_Data <- Proj_data_all_11$Bird - lmMod$fitted.values

# Displays Plot residuals against predicted values
plot(mis_fit_to_Data ~ lmMod$fitted.values)
abline(0, 0, col = "red")

# Quantile-quantile plot for residuals
qqnorm(mis_fit_to_Data)
qqline(mis_fit_to_Data, col = "red")

# Display AIC value
AIC(lmMod)

# Display detailed model summary
summary(lmMod)


#2  

#Test-1: (lmMod_reduced)
# In this 5 Variables "Butterflies","Hoverflies","Ladybirds","Grasshoppers_._Crickets","vascular_plants",
#I am removing vascular_plants and comparing the AIC value with lmMod. 
lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Butterflies","Hoverflies","Ladybirds","Grasshoppers_._Crickets")],y=TRUE)

summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria


#2 Test -2
# In this test i am removing Grasshoppers_._Crickets
lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Butterflies","Hoverflies","Ladybirds","Vascular_plants")],y=TRUE)

names(BD5)
summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria

#2 Test -3
# In this test i am removing Ladybirds
lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Butterflies","Hoverflies","Grasshoppers_._Crickets","Vascular_plants")],y=TRUE)

names(BD5)
summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria

#2 Test -4

# In this test i am removing Hoverflies.

lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Butterflies","Ladybirds","Grasshoppers_._Crickets","Vascular_plants")],y=TRUE)

names(BD5)
summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria

#2 Test -5

# In this test i am removing Butterflies.

lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Hoverflies","Ladybirds","Grasshoppers_._Crickets","Vascular_plants")],y=TRUE)

names(BD5)
summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria



# now I am introducing an interaction
# Test-1      Interaction with Ladybirds and Butterflies 
lmMod_interaction <- lm(Proj_data_all_11$Bird~
                          Butterflies+Hoverflies+Ladybirds+Grasshoppers_._Crickets+Vascular_plants
                        +Ladybirds*Butterflies,   
                        data=Proj_data,y=TRUE)

summary(lmMod_interaction )
AIC(lmMod,lmMod_reduced,lmMod_interaction) # model with interataction prefered 
cor(lmMod_interaction$fitted.values,lmMod_interaction$y) 



# Test-2           Interaction with Ladybirds and Hoverflies
lmMod_interaction <- lm(Proj_data_all_11$Bird~
                          Butterflies+Hoverflies+Ladybirds+Grasshoppers_._Crickets+Vascular_plants
                        +Ladybirds*Hoverflies,   
                        data=Proj_data,y=TRUE)

summary(lmMod_interaction )
AIC(lmMod,lmMod_reduced,lmMod_interaction) # model with interataction prefered 
cor(lmMod_interaction$fitted.values,lmMod_interaction$y)  


# Test-3          Interaction with Ladybirds and Grasshoppers_._Crickets
lmMod_interaction <- lm(Proj_data_all_11$Bird~
                          Butterflies+Hoverflies+Ladybirds+Grasshoppers_._Crickets+Vascular_plants
                        +Ladybirds*Grasshoppers_._Crickets,   
                        data=Proj_data,y=TRUE)

summary(lmMod_interaction )
AIC(lmMod,lmMod_reduced,lmMod_interaction) # model with interataction prefered 
cor(lmMod_interaction$fitted.values,lmMod_interaction$y) 

# Test-4        Interaction with Ladybirds and vascular plants
lmMod_interaction <- lm(Proj_data_all_11$Bird~
                          Butterflies+Hoverflies+Ladybirds+Grasshoppers_._Crickets+Vascular_plants
                        +Ladybirds*Vascular_plants,   
                        data=Proj_data,y=TRUE)

summary(lmMod_interaction )
AIC(lmMod,lmMod_reduced,lmMod_interaction) # model with interataction prefered 
cor(lmMod_interaction$fitted.values,lmMod_interaction$y)  


# now i am using one period as the training set and one as the test set.
table(Proj_data$period)

nrow(Proj_data)

Proj_data_Y70 <- Proj_data_all_11%>%filter(period=="Y70") # training set
Proj_data_Y00 <- Proj_data_all_11%>%filter(period=="Y00") # test set

nrow(Proj_data_Y00);nrow(Proj_data_Y00)

lmMod_70 <- lm(Proj_data_Y70$Bird~.,
               data=Proj_data_Y70[c(eco_selected_names)],y=TRUE)

qqnorm(lmMod_70$residuals);qqline(lmMod_70$residuals,col="red")

plot(lmMod_70$residuals~lmMod_70$fitted.values) # look for unwanted pattern in residuals

abline(0,0,col="red")
Predict_00 <- predict(lmMod_70,Proj_data_Y00)
plot(Predict_00~Proj_data_Y00$Bird)
abline(0,1,col="red")
mean((Proj_data_Y70$Bird-lmMod_70$fitted.values)^2)  # MSE on train data set 
mean((Proj_data_Y00$Bird-Predict_00)^2)  # MSE on test data is higher



#5) Open Analysis

# Data Overview
summary(eco_selected_names)
# Select taxonomic groups
eco_selected_names <- c("Butterflies","Hoverflies","Ladybirds","Grasshoppers_._Crickets","Vascular_plants")

# Calculate biodiversity measure as a mean of the selected groups
mean_selected <- rowMeans(Proj_data_all_11[, eco_selected_names])
Proj_data <- Proj_data_all_11 %>%
  select("Location", eco_selected_names, "Easting", "Northing", "dominantLandClass", "ecologicalStatus", "period") %>%
  mutate(eco_status_5 = mean_selected)

# Convert categorical variables to factors
Proj_data$period <- as.factor(Proj_data$period)
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass)


# This creates a table of univariate stats  
table <- data.frame()
for(i in c(2:6)){
  table <- rbind(table,
                 c(names(Proj_data)[i],
                   round(mean(Proj_data[,i],na.rm = TRUE),digits = 2),
                   round(sd(Proj_data[,i],na.rm = TRUE),digits = 2),
                   round(skewness(Proj_data[,i],na.rm = TRUE),digits = 2)
                 ))}
colnames(table) <- c("taxi_group","mean","sd","skewness")
table%>%arrange(sd,skewness)  


# this will extend data exploration with correlations between continuous variables
names(Proj_data)
cont_vars <- Proj_data%>%select(c(2:8)) # includes easting and northing 
names(cont_vars)
cormat <- round(x = cor(cont_vars,use="pairwise.complete.obs"), digits = 2)

# melt the correlation matrix
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(value)
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(Var1,value)

# box plot comparisons for the two periods for BD5  
eco_status_5 <- Proj_data%>%pull(eco_status_5)
eco_period <- Proj_data%>%pull(period)

# Output for boxplot  
plot(eco_status_5~eco_period)              

# Spatial Analysis
spatial_analysis <- Proj_data %>%
  ggplot(aes(x = Easting, y = Northing, color = eco_status_5)) +
  geom_point() +
  labs(title = "Spatial Analysis of BD5", x = "Eastings", y = "Northings", color = "BD5") 

# Land Classification Analysis
land_class_analysis <- Proj_data %>%
  ggplot(aes(x = dominantLandClass, y = eco_status_5, fill = period)) +
  geom_boxplot() +
  labs(title = "BD5 Across Land Classifications", x = "Land Classification", y = "BD5")


# Results
print(land_class_analysis)
print(spatial_analysis)


# Comparsion of Northing Vs Proj_data 

# Northing vs Proj_data

plot(Proj_data_all_11$Northing~Proj_data$eco_status_5)
abline(0,1,col="red")
lin_mod <- lm(Proj_data_all_11$Northing~Proj_data$eco_status_5)
abline(lin_mod,col="green")
summary(lin_mod)

# some diagnostics 
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")



# Comparsion of Easting Vs Proj_data

# Easting vs Proj_data

plot(Proj_data_all_11$Easting~Proj_data$eco_status_5)
abline(0,1,col="red")
lin_mod <- lm(Proj_data_all_11$Easting~Proj_data$eco_status_5)
abline(lin_mod,col="green")
summary(lin_mod)

# some diagnostics 
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")

