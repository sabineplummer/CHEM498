# CHEM498 Assignment (GK) 1 

# Write your own t-test

# Sabine Plummer (40087050)
# 17/09/2020

# Packages
library(readr)

# F-test
F_Test <- function (x, y) { #variables 
  result <- (sd(x))^2/(sd(y))^2 #formula
  print (result)
}

# Spool 
S_Pool <- function (x, y) { #variables
  result <- sqrt((((sd(x))^2)*(length(x)-1)+((sd(y))^2)*(length(y)-1))/(length(x)+length(y)-2)) #formula
  print (result)
}

# T-test with equal variance
T_Test_Evar <- function (x, y, s) { #variables
  result <- (((abs(mean(x)-mean(y)))/s)*(sqrt((length(x)*length(y))/(length(x)+length(y))))) #formula
  print (result)
}

# T-test with unequal variance
T_Test_Uvar <- function (x, y) { #variables
  result <- (abs(mean(x)-mean(y)))/(sqrt((((sd(x))^2)/length(x))+(((sd(y))^2)/length(y)))) #formula
  print (result)
}

# Appropriate T-test runs based on F-test result

# Randomly generated data
x <- rnorm(20)
y <- rnorm(20)

t.test(x,y)
T_Test_Uvar(x,y)


use file.choose() to find data