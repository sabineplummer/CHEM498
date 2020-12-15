################################################################################
#
# Example code for CHEM498, Decsision Trees using R (Boston housing data)
#
# Author: Gregor Kos (greg)
# Last update: 10 Nov 2020
#
# Example code for exercises
# Predict air pollution using housing prices and all the variables available
# Reduce tree size to avoid overfitting while maintaining model quality
# Modified from https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# Data published in https://doi.org/10.1016/0095-0696(78)90006-2
#
################################################################################

## Install and load packages
# install.packages("rpart")
# install.packages("rpart.plot"")
library("rpart")       # To build the decision tree model
library("rpart.plot")  # To visualise decision tree results
library("caTools")     # Split data into training and test sets

# For mapping
library("ggplot2")
library("ggmap")
library("mapproj")

# Load the data
boston <- read.csv('boston.csv')
str(boston)

## A map would be nice; taken from https://rpubs.com/Mturgal/164857
BostonLL <-c(-71.30, 42.00, -70.80, 42.40)
map      <- get_map(location = BostonLL, zoom = 11)

# Define the map
mapPoints <- ggmap(map) +
  geom_point(aes(x = LON, y = LAT,color = NOX),
             data = boston, alpha = 0.7,size=3) +
  scale_color_gradient(low = "#9ebcda", high = "#8856a7") +
  labs(title = "Polluted Areas in Boston",
       y = "Latitude", x = "Longtitude", color = "NOx conc (pp 10 million)" )

# Plot the map
mapPoints

# Plot observations on a geographical grid
plot(boston$LON, boston$LAT, xlab  = "Longitude", ylab = "Latitude")

# Find basic stats of Boston air pollution
summary(boston$NOX)

# Plot air pollution (NO_x concentrations); above average concentrations of NOx
points(boston$LON[boston$NOX >= 0.54], boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20)

# How about the prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)

# Plot housing prices on grid
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "blue", pch = 20, cex = 1)

# Add the pollution datra
points(boston$LON[boston$NOX >= 0.54], boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20, cex = .5)

## Let's try a linear regression -- to show that this is not a linear problem
lm.nox <- lm(NOX ~ LAT + LON, data = boston)
summary(lm.nox)

# Visualize regression output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$NOX >= 0.54], boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20, cex = 1)

# List the fitted values
# lm.nox$fitted.values

# Linear regression really does not work well here
points(boston$LON[lm.nox$fitted.values >= 0.54], boston$LAT[lm.nox$fitted.values >= 0.54], col = "blue", pch = 20, cex = 0.5)

## Let's try a tree-based model
tree.nox <- rpart(NOX ~ LAT + LON, data = boston)
# tree.nox <- rpart(NOX ~ LAT + LON + MEDV, data = boston)
# tree.nox <- rpart(NOX ~ LAT + LON + MEDV + TAX, data = boston)

# Plot the tree using prp command defined in rpart.plot package
# For more plotting options, see <http://www.milbo.org/rpart-plot/prp.pdf>
prp(tree.nox)

plot(boston$LON, boston$LAT)

points(boston$LON[boston$NOX >= 0.54],boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20, cex = 1)

fittedvalues = predict(tree.nox)
points(boston$LON[fittedvalues > 0.54],boston$LAT[fittedvalues >= 0.54], col = "blue", pch = 20, cex = 0.5)

# Avoid overfitting
# Same result with smaller tree (minbucket size reduced)
# Minbucket size: Min number of observations in terminal node (i.e., a pruning method)
tree.nox2 <- rpart(NOX ~ LAT + LON, data = boston, minbucket = 50)
prp(tree.nox2)

plot(boston$LON, boston$LAT)

points(boston$LON[boston$NOX >= 0.54],boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20, cex = 1)

fittedvalues <- predict(tree.nox2)
points(boston$LON[fittedvalues > 0.54],boston$LAT[fittedvalues >= 0.54], col = "blue", pch = 20, cex = 0.5)

## Predict air pollution using all the variables we have available
# Split the data; set.seed() ensures same split/same samples (reproducible)
set.seed(123)

split <- sample.split(boston$NOX, SplitRatio = 0.7)
train <- subset(boston, split == T)
test  <- subset(boston, split == F)

# Build a tree model with all variables using the training data
tree.all <- rpart(NOX ~ LAT + LON + CRIM + ZN + INDUS + CHAS + MEDV + RM + AGE + DIS + RAD + TAX + PTRATIO, data = boston) # Use data = train for validation
prp(tree.all)

# Predict the test samples and check results
tree.pred <- predict(tree.all, newdata = test)

# Calcualte SSE; squared differences between each observation and its estimate
tree.sse <- sum((tree.pred - test$NOX)^2)
tree.sse

# Visualise cross validation result
plotcp(tree.all, minline = T, col = "red")

# Produces 2 plots --
# Plot (1) r2 (from CV) versus number of splits.
# Plot (2) Relative error from CV +/- 1 stddev from CV vs # splits.
rsq.rpart(tree.all)

plot(boston$LON, boston$LAT)

points(boston$LON[boston$NOX >= 0.54], boston$LAT[boston$NOX >= 0.54], col = "red", pch = 20, cex = 1)

fittedvalues <- predict(tree.all)
points(boston$LON[fittedvalues >= 0.54],boston$LAT[fittedvalues >= 0.54], col = "blue", pch = 20, cex = 0.5)
