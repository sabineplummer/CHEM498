# CHEM498 Assignment (GK) 2 

# Prepare Data for Analysis

# Sabine Plummer (40087050)
# 06/10/2020

# Packages
library(readr)
library(dplyr)
library(naniar)


# Importing Data from CSV files

# Data set 1
ENclimate11 <- read.csv("dirty_en_climate_hourly_QC_702S006_11-2019_P1H.csv", stringsAsFactors = FALSE) #importing Environment Canada data
str(ENclimate11) #look at column data types post import

# Data set 2
ENclimate12 <- read.csv("dirty_en_climate_hourly_QC_702S006_12-2019_P1H.csv", stringsAsFactors = FALSE) #importing Environment Canada data
str(ENclimate12) #look at column data types post import

# Data set 3
RSQA <- read.csv("dirty_RSQA,  station99, 1 nov 2019-13 apr 2020.csv", stringsAsFactors = FALSE) #importing RSQA data
str(RSQA) #look at column data types post import


# Arranging data types

# ENclimate11

ENclimate11$Date.Time = as.POSIXct(ENclimate11$Date.Time)
ENclimate11$Temp...C. = as.numeric(ENclimate11$Temp...C.)
ENclimate11$Dew.Point.Temp...C. = as.numeric(ENclimate11$Dew.Point.Temp...C.)
str(ENclimate11)

# ENclimate12

ENclimate12$Date.Time = as.POSIXct(ENclimate12$Date.Time)
ENclimate12$Temp...C. = as.numeric(ENclimate12$Temp...C.)
ENclimate12$Dew.Point.Temp...C. = as.numeric(ENclimate12$Dew.Point.Temp...C.)
ENclimate12$Stn.Press..kPa. = as.numeric(ENclimate12$Stn.Press..kPa.)
str(ENclimate12)

# RSQA

RSQA$Date...Time = as.POSIXct(RSQA$Date...Time, format = "%d-%m-%Y %H:%M")
RSQA$PM2.5..ug.m3. = as.numeric(RSQA$PM2.5..ug.m3.)
RSQA$PM10..ug.m3. = as.numeric(RSQA$PM10..ug.m3.)
RSQA$O3..ppb. = as.numeric(RSQA$O3..ppb.)
RSQA$NO..ppb. = as.numeric(RSQA$NO..ppb.)
RSQA$NO2..ppb. = as.numeric(RSQA$NO2..ppb.)
RSQA$SO2..ppb. = as.numeric(RSQA$SO2..ppb.)
str(RSQA)


# Merging data sets

merged_data1 <- rbind(ENclimate11, ENclimate12)
str(merged_data1)

merged_data2 <- merge(merged_data1, RSQA, by.x = "Date.Time", by.y = "Date...Time", all.x = FALSE, all.y = TRUE)
str(merged_data2)


# Making vector of missing data variables

M <- c("NoData", 999, "999", -999, "-999", -1999, "-1999", "", " ", "<Samp", "InVld", "Down", "Calib", "Zero") 

# Replacing all missing values with NA

merged_data2_NA <- merged_data2 %>% replace_with_na_all(condition = ~.x %in% M) #replaces any missing data term in the dataframe with NA
str(merged_data2_NA)



