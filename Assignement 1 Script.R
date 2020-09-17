# CHEM498 Assignment (GK) 1 

# Write your own t-test

# Sabine Plummer (40087050)
# 17/09/2020

# Packages
library(readr)

# F-test
F_Test <- function (x, y) { #variables 
  if (sd(x) > sd(y)) {
    return (sd(x))^2/(sd(y))^2
  } 
  if (sd(y) > sd(x)) {
    return (sd(y))^2/(sd(x))^2
  }
}

  
# Spool 
S_Pool <- function (x, y) { #variables
  output <- sqrt((((sd(x))^2)*(length(x)-1)+((sd(y))^2)*(length(y)-1))/(length(x)+length(y)-2)) #formula
}

# T-test with equal variance
T_Test_Evar <- function (x, y) { #variables
  output <- (((abs(mean(x)-mean(y)))/S_Pool(x,y))*(sqrt((length(x)*length(y))/(length(x)+length(y))))) #formula
  return (output)
}

# T-test with unequal variance
T_Test_Uvar <- function (x, y) { #variables
  output <- (abs(mean(x)-mean(y)))/(sqrt((((sd(x))^2)/length(x))+(((sd(y))^2)/length(y)))) #formula
  return (output)
}

# Appropriate T-test runs based on F-test result
T_Test <- function (x,y) { #variables
  if (F_Test(x,y) <= qf(0.95, df1=(length(x)-1), df2=(length(y)-1))) {
   return (T_Test_Evar(x,y))
    } 
  if (F_Test(x,y) > qf(0.95, df1=(length(x)-1), df2=(length(y)-1))) {
   return (T_Test_Uvar(x,y))
    }
}

# Randomly generated data
set.seed(20)
x <- rnorm(20)
y <- rnorm(20)

use file.choose() to find data