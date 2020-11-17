################################################################################
#
# Sabine Plummer (40087050)
# Assignment 3 (GK)
# 17/11/2020
#
################################################################################


##### Wine classification using KNN with model validation #####

library(caret)

# Download the data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
download.file(url = url, destfile = "wine.csv")
wine_df <- read.csv("wine.csv", sep = ";")

# Check data structure to make sure import was correctly done
# All data must be numerical
str(wine_df)

# Ensure reproducible results of the following calculation by using set.seed()
# The number you choose does not matter
set.seed(3033)

# Split the data in to training and test set 
# Target variable is V1, split is 70:30, result returned as matrix, not list)
partitioned.data <- createDataPartition(y = wine_df$quality, p = 0.8, list = F)

training <- wine_df[partitioned.data,]
testing  <- wine_df[-partitioned.data,]

str(training)
str(testing)

# Check dimensions of training and test sets
dim(training)
dim(testing)

# Check for NAs; not permitted; otherwise use complete.cases() or na.omit()
anyNA(wine_df)

# Summarise of data set for each variable; determine need for preprocessing
summary(wine_df)

# Convert V12, the target variable to factors
training[["quality"]] = factor(training[["quality"]])
testing[["quality"]]  = factor(testing[["quality"]])

# Train the model
# Choose validation option
trctrl <- trainControl(method = "LOOCV")

# Train the model
# Preprocessing as part of train
set.seed(3333)
knn_fit <- train(quality ~., data = training, method = "knn", trControl = trctrl,
                 preProcess = c("center", "scale"), tuneLength = 5)

# Show model output
knn_fit

# Plot k values used against model accuracy (from cross-validation)
plot(knn_fit)
# Equivalent with variables explicitly mentioned
plot(knn_fit$results$k, knn_fit$results$Accuracy, type = "l")

# Use data on test set (unknowns)
test.prediction <- predict(knn_fit, newdata = testing)

# Plots prediction result
test.prediction

# Plot the confusion matrix for the model
confusionMatrix(test.prediction, testing$quality)
