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
partitioned.data <- createDataPartition(y = wine_df$quality, p = 0.7, list = F)

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
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # change to LOOCV to run full validation

# Train the model
# Preprocessing as part of train
set.seed(3333)
knn_fit <- train(quality ~., data = training, method = "knn", trControl = trctrl,
                 preProcess = c("center", "scale"), tuneLength = 5) # change between 10 and 5 depending on KNN model running

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


##### Wine classification using SVM with model validation #####

# Linear 

# tuning with different C values
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)

# train model
svm_fit <- train(quality ~., data = training, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 5)

# Show model output
svm_fit

# Plot k values used against model accuracy (from cross-validation)
plot(svm_fit)

# Equivalent with variables explicitly mentioned
plot(svm_fit$results$C, svm_fit$results$Accuracy, type = "l")

# Use data on test set (unknowns)
test.prediction.svm <- predict(svm_fit, newdata = testing)

# Plots prediction result
test.prediction.svm

# Plot the confusion matrix for the model
confusionMatrix(test.prediction.svm, testing$quality)


# Non-Linear

# tuning with different C and sigma values
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
set.seed(3232)

# train model
svm_Radial <- train(quality ~., data = training, method = "svmRadial",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid_radial,
                      tuneLength = 5)

# Show model output
svm_Radial

# Plot k values used against model accuracy (from cross-validation)
plot(svm_Radial)

# Equivalent with variables explicitly mentioned
plot(svm_Radial$results$C, svm_Radial$results$Accuracy, type = "l")

# Use data on test set (unknowns)
test.prediction.svm.radial <- predict(svm_Radial, newdata = testing)

# Plots prediction result
test.prediction.svm.radial

# Plot the confusion matrix for the model
confusionMatrix(test.prediction.svm.radial, testing$quality)

# KNN: https://dataaspirant.com/knn-implementation-r-using-caret-package/
# SVM: https://dataaspirant.com/support-vector-machine-classifier-implementation-r-caret-package/

