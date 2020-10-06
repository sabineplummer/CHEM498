# CHEM498 Assignment (GK) 1 

# Write your own t-test

# Sabine Plummer (40087050)
# 17/09/2020

# Packages
library(readr)

# F-test
F_Test <- function (x, y) { #variables 
  if (sd(x) > sd(y)) {
    return ((sd(x))^2/(sd(y))^2) #determines which should be the nominator to return F calc >=1
  } 
  if (sd(y) > sd(x)) {
    return ((sd(y))^2/(sd(x))^2)
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
  if (F_Test(x,y) <= qf(0.95, df1=(length(x)-1), df2=(length(y)-1))) { #determines if Fcalc <= Ftable
   return (T_Test_Evar(x,y))
    } 
  if (F_Test(x,y) > qf(0.95, df1=(length(x)-1), df2=(length(y)-1))) { #determines if Fcalc > Ftable
   return (T_Test_Uvar(x,y))
    }
}

###

# Randomly generated data equal variance
set.seed(20)
a <- rnorm(20)
b <- rnorm(20)

  # F-tests
  f1 <- F_Test(a,b)
  f2 <- var.test(b,a) #manual determination of greater sd as numerator

  # Testing for equality
  round(f1, 5) == round(f2$statistic, 5)

  # T-tests
  t1 <- T_Test(a,b)
  t2 <- t.test(a,b)

  # Testing for equality
  round(t1, 5) == round(abs(t2$statistic), 5) #t.test gives negative t value

# Randomly generated data unequal variance
set.seed(20)
c <- rnorm(20, sd = 1)
d <- rnorm(20, sd = 5)

  # F-tests
  f3 <- F_Test(c,d)
  f4 <- var.test(d,c) #manual determination of greater sd as numerator

  # Testing for equality
  round(f3, 5) == round(f4$statistic, 5)

  # T-tests
  t3 <- T_Test(c,d)
  t4 <- t.test(c,d)

  # Testing for equality
  round(t3, 5) == round(abs(t4$statistic), 5) #t.test gives negative t value


# Government of Canada Data

Data2019 <- read.csv("en_climate_daily_QC_7025251_2019_P1D.csv", stringsAsFactors = FALSE) #importing 2019
str(Data2019)
Data2020 <- read.csv("en_climate_daily_QC_7025251_2020_P1D.csv", stringsAsFactors = FALSE) #importing 2020
str(Data2020)

# Subsetting dataframes (selecting May)

Data2019_subset <- subset(Data2019, Month == 5)
Data2020_subset <- subset(Data2020, Month == 5)

  # Testing daily means

  # F-tests
  f1 <- F_Test(Data2019_subset$Mean.Temp...C.,Data2020_subset$Mean.Temp...C.)
  f2 <- var.test(Data2020_subset$Mean.Temp...C.,Data2019_subset$Mean.Temp...C.) #manual determination of greater sd as numerator

  # Testing for equality
  round(f1, 5) == round(f2$statistic, 5)

  # T-tests
  t1 <- T_Test(Data2019_subset$Mean.Temp...C.,Data2020_subset$Mean.Temp...C.)
  t2 <- t.test(Data2019_subset$Mean.Temp...C.,Data2020_subset$Mean.Temp...C.)

  # Testing for equality
  round(t1, 5) == round(abs(t2$statistic), 5) #t.test gives negative t value

  # Tcalc vs Ttable
  if (T_Test(Data2019_subset$Mean.Temp...C.,Data2020_subset$Mean.Temp...C.) 
      > qt(0.95, df=(length(Data2019_subset$Mean.Temp...C.)+(length(Data2020_subset$Mean.Temp...C.))-2))) {
    print("Difference is significant")
  } else {
    print("Difference is not significant")
  }

# :)
  