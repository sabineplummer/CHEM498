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
library("caret")

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

nrow(train) # check split
nrow(test)

# Build a tree model with all variables using the training data
tree.c <- rpart(C ~ ., data = train, method = "class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 )) # Use data = train for validation

prp(tree.c)
rpart.plot(tree.c)
rpart.rules(tree.c)

# Visualize cross validation result
printcp(tree.c)
plotcp(tree.c, minline = T, col = "red")

# Produces 2 plots --
# Plot (1) r2 (from CV) versus number of splits.
# Plot (2) Relative error from CV +/- 1 stddev from CV vs # splits.
rsq.rpart(tree.c)

# Post-Pruning
printcp(tree.c)
bestcp <- tree.c$cptable[which.min(tree.c$cptable[,"xerror"]),"CP"] #choosing best cp

# Prune the tree using the best cp.
pruned <- prune(tree.c, cp = bestcp)

# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

# Confusion Matrix (training data)
conf.matrix <- table(train$C, predict(pruned,type = "class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Test samples 
tree.pred <- predict(pruned, newdata = test)
summary(tree.pred)

# Confusion Matrix (test data)
conf.matrix.2 <- table(test$C, predict(pruned, test, type = "class"))
rownames(conf.matrix.2) <- paste("Actual", rownames(conf.matrix.2), sep = ":")
colnames(conf.matrix.2) <- paste("Pred", colnames(conf.matrix.2), sep = ":")
print(conf.matrix.2)

# Error Rate
error.pred <- predict(pruned, test, type="class")
conf.matrix.test <- table(test$C, error.pred)
error.rate = round(mean(error.pred != test$C),2)

# Accuracy
accuracy <- 1 - error.rate

count <- length(error.pred)
count

# print out
paste("error rate", error.rate)
paste("accuracy:", accuracy)


confusionMatrix(error.pred, test$C)




# Sources: 
# https://rpubs.com/maulikpatel/229337
# https://www.listendata.com/2015/04/decision-tree-in-r.html
