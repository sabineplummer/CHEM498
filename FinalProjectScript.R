################################################################################
#
# Sabine Plummer
# 40087050
# December 16th 2020 
# CHEM498 Final
#
# Classification Model for Solar Flares Using Decision Trees
#
################################################################################

# Load packages
library("rpart")       # To build the decision tree model
library("rpart.plot")  # To visualise decision tree results
library("caTools")     # Split data into training and test sets

# Load the data
flare <- read.csv('flare.data.2.csv')

# Factor variables for categorization 
flare$CLASS <- as.factor(flare$CLASS) # class
flare$SIZE <- as.factor(flare$SIZE) # largest spot size
flare$DIST <- as.factor(flare$DIST) # spot distribution
flare$ACT <- as.factor(flare$ACT) # activity
flare$EVO <- as.factor(flare$EVO) # evolution
flare$PRE <- as.factor(flare$PRE) # previous activity
flare$HISTCOM <- as.factor(flare$HISTCOM) # historically-complex
flare$RECCOM <- as.factor(flare$RECCOM) # recently historically complex
flare$AREA <- as.factor(flare$AREA) # area
flare$AREAL <- as.factor(flare$AREAL) # area of largest spot
flare$C <- as.factor(flare$C) # C class
flare$M <- as.factor(flare$M) # M class
flare$X <- as.factor(flare$X) # X class

# Check data classes and for outliers
str(flare)
summary(flare) # noticed some NA and missing values

any(is.na(flare)) # returned TRUE
any(is.null(flare)) # returned FALSE

flare[!complete.cases(flare),] # row 1067 doesn't contain anything
flare <- flare[-c(1067),]

str(flare)
summary(flare) # looks good!

# split data
set.seed(222)

split <- sample.split(flare$C, SplitRatio = 0.7)
train <- subset(flare, split == T)
test  <- subset(flare, split == F)

# Build a tree model with all variables using the training data
tree.all <- rpart(C ~ CLASS + SIZE + DIST + ACT + EVO + PRE + HISTCOM + RECCOM + AREA + AREAL, data = train) # Use data = train for validation
prp(tree.all)
rpart.plot(tree.all)
rpart.rules(tree.all)

# Predict the test samples and check results
tree.pred <- predict(tree.all, newdata = test)

# Visualise cross validation result
printcp(tree.all)
plotcp(tree.all, minline = T, col = "red")

# Produces 2 plots --
# Plot (1) r2 (from CV) versus number of splits.
# Plot (2) Relative error from CV +/- 1 stddev from CV vs # splits.
rsq.rpart(tree.all)

