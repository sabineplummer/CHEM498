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
# - Factor data; categorical ("candy", candy canes", "candy corn", "syrup")
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






