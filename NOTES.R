# NOTES CHEM 498 
# Sabine Plummer
# Fall 2020

# VARIABLES
# - letters, number, dots or underlines characters
# - Case sensitive
# **reserved words (TRUE, FALSE)**

# Assignment statement: number or expression
a <- 2
a
b <- "blue"
b

# Calculator 
x <- 2+3
x

# Objects: pieces of memory with values
x <- 5
# Statements: creates and processes objects (scripts)
mean(x)
# Packages: collection of script functions
library (readr)

# Types of data in R
# - Numerical data; numbers (42, real, integer)
# - Integer data; integer (whole numbers)
# - Character data; string ("Oompaloompa")
# - Factor data; categorical ("candy", "candy canes", "candy corn", "syrup")
# - Ordinal data; ordered classifications (educational level)
# - Binary data; 1 0
# - Missing values (NA)

#Vectors: column of a table
w <- as.numeric(c("-.1", "2.7", "B"))
w
# as.numeric forces vector to be numeric

# Creating column vectors
r <- rbind("-.1", "2.7", "B")
r
# Creating row vectors
c <- cbind("-.1", "2.7", "B")
c
# Creating matrices
M <- matrix(
  data = cbind(2,4,3,1,5,7), # data as a row vector
  nrow = 2, # number of rows
  ncol = 3, # number of columns
  byrow = T #fill matrix by rows
)
M

# Call specific row and column using M[r,c]
M[2,3]
# Whole rows
M[2,]
# Whole columns
M[,3]
# Multiple columns/rows at once
M[,c(1,3)]

# Name rows/columns
dimnames(M) = list(
  c("row1", "row2"),
  c("col1", "col2", "col3")
)
M
# Call by name
M["row2", "col3"]

# DATAFRAMES
# A table allowing for all data types (1 type/column)

# Importing from csv
data_table <- read.csv("path to file .csv")
# use na.strings = c(" ", "<LOD", "  ", "NA") to clean data
# Subsetting
data_subset <- subset(data_table, "parameter" & "parameter")

# Note on operators
# ! = not
# & = and
# | = or
# == = equals
# != = not equal
# <, >, <=, >= 

# Loops
# For
for (variable in sequence) { # sequence ex. c(1:5)
  "expression
  expression"
}
# While
while (condition) { # condition ex. b < 50
  "expression
  expression"
}

# If 
if (test_expression) { # test expression ex. x > 0
  "statement"
}

# Merging data sets
# Horizontal
merged_dataset <- merge("dataset1", "dataset2", by = "common vector")
# Vertical
merged_dataset <- rbind("dataset1", "dataset2")

# Statistic functions
summary()
sum ()
mean()
median()
max()
min()
sd()
quantile()
str()

# Syntax
# na.rm = TRUE, NA data is ignored

# CURATING DATA
# Post-import
# - remove file headers
# - flag/convert missing data

# PLOTS
# Arguments
# x - x coordinates of points on plot
# y - y coordinates of points on plot
# type = "..." (p for points, l for lines)
# title = "..." (title of plot)
# xlab = "..." (title of x axis)
# ylab = "..." (title of y axis)

# ggplot2

# DATA CURATION AND MISSING DATA

# Making vectors with loops
# initialize
x <- c(0,1)
# length() returns number of rows
while (length(x) < 10) {
  # Determine position of last element
  position <- length(x)
  # Calcuate sum of last and last but one element; [] addresses data 
  new <- x[position] + x[position-1] #
  # Add new value to the vector
  x <- c(x,new) }
print(x)

# Coercing data
x <- as.numeric(c("-.1", "2.7", "B"))
# B will be recoded as NA

# Variable type change example
v <- factor(c("2", "3", "5", "7", "11"))
str(v)
print(v)
v <- as.character(v)  
str(v)
print(v)
v <- as.numeric(v)  
str(v)
print(v)
v <- as.integer(v)  
str(v)
print(v)
# has to go through character to get to interger/numerical from factor

# Matrix to data frame
as.data.frame()

# Dates
# Use POSIXct date/time (lubridate package)
as.POSIXct()

# Missing data 
is.na(x)
is.null(x)
Inf # infinite 
NaN # not a number

# Working with NA
na.rm = T #remove missing data
na.omit 
# Amelia II, Mice, and mitools packages for imputations

# PLOTTING

# Arguments
# x - x coordinates of points on plot
# y - y coordinates of points on plot
# type = "..." (p for points, l for lines)
# title = "..." (title of plot)
# xlab = "..." (title of x axis)
# ylab = "..." (title of y axis)

data(mtcars)
plot(mtcars$cyl, mtcars$mpg) 
boxplot(mtcars$cyl, mtcars$mpg)

# ggplot2

# Code layers structure of plot elements

library(ggplot2)
ggplot()
+ geom_point(data = quakes, aes(x = lat,y = long, colour = stations)

# Learn to use both!


# CENSORED DATA

# Left censored data:
# measurement below the field limit of detection (LOD) or quantification (LOQ)

# True value is between zero and censor limit (CL)
# Single lvevl sencoring: one LOD for single analysis method
# Multiple level censoring: multiple LODs for analysed data (multiple methods, pooled data)

# Right censored data:
# measurement above ULOQ

# ISSUE? Deleting data removes information from data set
# Deleting = UPWARDS bias for means and medians
# Setting to zero = DOWNWARDS bias for means and medians

# --> impacts correlations 

# Basic Strategies for Censored Data

# removing all data < LOD
# setting all data < LOD to zero
# setting all data < LOD to 1/2 LOD
# substituting random values between zero and LOD

# --> ONLY GOOD FOR SMALL NO. < LOD in large data sets

# Survival Analysis / Reliability Analysis (often for right censored data)

# - Maximum Likelihood estimation (MLE)
# - Kaplan-Meier (KM) methods --> non-parametric
# - Regression on Order Statistics (ROS) --> semi-parametric

# % censored (p)    sample size (n)     Method
#   p < 15                     substitution (but also no)
#  0 < p < 50           < 50       Kaplan-Meier (KM)
#  0 < p < 50           < 50          Robust ROS
#  0 < p < 50           > 50         MLE-Lognormal

# NADA package

censummary() # Details about censored observations
mean()
median()
hist() # to see the frequency of the data in each bin; plot mean and median into histogram

cenfit() # Kaplan-Meier (KM)

# Considered the (historic) “standard” for censored methods
# For small sample sizes; p between 0% and 50%
# Estimator: “Survival curve”, an empirical cumulative distribution function (ECDF) accounting for the censored observations at each concentration
# Estimates mean (biased upward!) and standard deviation; estimated percentiles correctly (but only above the largest censoring limit!)

cenmle() # MLE, lognormal distribution

# Based on a lognormal distribution
# mean(), sd(), quantile()
# Estimates all quantiles (also below the censoring limit; distribution is known! Use the probability plot to check!)
# Performs better than K-M; multiple LODs possible
# For small sample sizes, the parametric method will give uncertain results (distribution not well defined!). Use ROS or K-M instead.

cenros() # ROS

# Based on a lognormal distribution; robust against skewed data.
# Multiple detection limits supported
# Performs a regression on data assuming lognormal quantiles
# Regression line predicts unknown observations below LOD. Don’t use these as individual observations - only to compute summary statistics
# No limit on the percentage of censored data (up to 80%!)
# No confidence intervals provided (review the std error instead;
 
                                
# MULTIVARIATE DATA ANALYSIS

# consider several characteristics (coded as variable) simultaneously

# variance is used for signal and noise
# Noise is anything that doesn't contribute to classification or concentration predictions

# Working with multivariate data

# - Exploratory data analysis
# - Classification for identifying new or existing classes
# - Regression analysis

# Structure of multivariate data

# - linear
# - non-linear
# - inseparable

# Predictive model development 

# - Original data (ex. FTIR data)
# - Pre-treatment/reduction (ex. averaging, PCA)
# - Classification (ex. KNN, LDA, PCA)
# - Classification model
# - Validation 

# Model Training

# - test model performance based on test samples (large data)
# - test model performance using cross validation (small data)
# - avoid over/under fitting: slightly lower-quality fit is usually more robust that perfect fit

# - training set: sample of data used to fit model
# - validation set: sample of data used to provide unbiased evaluation of a model fit on the training dataset while tuning model parameters
# - test set: sample of data used to provide unbiased evaluation of a final model fit on the training dataset

# - small data set? cross-validation: bootstrapping


# PRINCIPAL COMPONENT ANALYSIS

# unsupervised methods 

# - No additional input provided for the classes (e.g., PCA)
# - You “ask” the algorithm to develop a model without knowing what the “correct” answer is
# - Variance is explained by measurement data only (e.g., a spectrum) and not by supplying the expected results (e.g., concentration) to build the model

# supervised models

# - Input provided for existing classes (e.g., contaminated & clean) in the training set (e.g., KNN)
# - You “ask” the algorithm to develop a model that fits the training data and then test model performance on a validation/test data set
# - Reference data are not provided for validation and testing

# Multivariate data analysis methods (Chemistry)

# Principal Component Analysis (PCA): data reduction, classification, unsupervised
# K-nearest neighbor (KNN): classification, supervised
# Principal Component Regression (PCR): quantitative, prediction, unsupervised
# Decision Trees (incl. random forests, DT/RF): classification, supervised
# Artificial Neural Networks (ANN): classification and quantitative prediction, unsupervised

# PCA

# - standard multivariate method
# - unsupervised classification
# - used for feature selection, data reduction, de-correlation of variables
# - choose as many PC as necessary to include variance responsible for classification

prcomp()

# Model assessment - Resampling Methods

# - Repeatedly drawing samples from a training set and refitting a model of interest on each sample
# - Obtain additional information about the fitted model (e.g., errors, robustness...)
# - For large data sets, split into training and test sets
# - Cross-validation Use for small data sets, where you want to use as many samples as possible for training
# - Cross-validation avoids overlapping training and test sets

# Training set Validation 

# - Randomly divide the available data set into two subsets: Training set and a validation set (sometimes called “hold-out set”)
# - Create the model using the training set
# - Use the fitted model is used to predict the responses for the test set
# - Validation set error (di↵erences between observed and predicted) is calculated

# k-fold cross-validation


# - Data is split into k subsets of equal size
# - Choose k between 5 and 10, while ensuring to have enough training data
# - Each subset in turn is used for testing and the remainder for training

# Full Cross-Validation

# - Leave-one-out cross-validation
# - Repeated testing with multiple test samples provides a realistic error estimate
# - Always the same result – no variation due to randomness of training and validation set selection
# - Time consuming calculation, if sample size is large

# PRedictive Error Sum of Squares (PRESS)

# - Use input from full cross-validation (squared di↵erences between measured and predicted)
# - Calculate your model by step-wise variable selection
# - Find minimum number of components for minimum PRESS


# KNN CLASSIFICATION AND MODEL QUALITY

# --> more resampling methods

# Noise addition

# - test for noise immunity (generalization of a model)
# - test should not adapt itself to the noise in the system
# - Test model stability by adding noise to test/unknown data set

# Confusion matrix

# - describes performance of classification model
# - tests data for which true values are known

# Extended confusion matrix

# - True pos / true neg / false pos / false neg
# - High Kappa score = big difference between accuracy and null error rate

# Distance measures (bewteen 2 points in n-dimensional space)

# - p = 1 is equivalent to the Manhattan distance
# - p = 2 is equivalent to the Euclidean distance

# Mahalanobis distance (don't need to scale or decorrelate)


# K-nearest neighbours, KNN

# - Used to classify objects based on closest training examples
# - Train model with reference values supplied (supervised learning)
# - Supply unknowns and calculate distance to nearest neighbours
# - Vote on class to assign it to unknown
# - Among the simplest of all Data Mining Algorithms

# - Determines class of unknowns based based computed distance in training records

# Tunning KNN model

# Stir pile til it starts looking right...

# - Test different distance measures & other weighting parameters
# - k too small: Sensitive to noise points; k too large: May include points from other classes
# - Choose an odd value for k, to eliminate ties:
        # k = 1: Square class
        # k = 3: Triangle class 
        # k = 7: Square class

# Advantages

# - Simple technique that is easily implemented; building model is inexpensive
# - Extremely flexible classification scheme; does not involve preprocessing (well...)
# - Well suited for multi-modal classes (classes of multiple forms)
# - It is always half as good as the best methods; good for baselining results

# Disadvantages

# - Classifying unknown records is computationally expensive;
# - Requires distance computation of k-nearest neighbours
# - Especially when the size of the training set grows
# - Accuracy can be severely degraded by the presence of noisy or irrelevant features



# PCR is just a MLR with PCA
# PLS is supervised PCA for MLR









