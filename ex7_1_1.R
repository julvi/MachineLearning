# exercise 7.1.1
rm(list=ls())
source('setup.R')
library(FNN) # if the package FNN is not already installed, install it using install.packages("FNN")

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth4.mat'))
X <- dat$X
N <- dat$N
M <- dat$M
y <- as.factor(dat$y) # make sure that the response is interpreted as a categorical variable
C <- dat$C

X_train <- dat$X.train
N_train <- dat$N.train
M_train <- dat$M.train
y_train <- as.factor(dat$y.train) # make sure that the response is interpreted as a categorical variable
C_train <- dat$C.train

X_test <- dat$X.test
N_test <- dat$N.test
M_test <- dat$M.test
y_test <- as.factor(dat$y.test) # make sure that the response is interpreted as a categorical variable
C_test <- dat$C.test

classNames <- as.vector(unlist(dat$classNames))
attributeNames <- as.vector(unlist(dat$attributeNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)


## Make a scatterplot of the data
Color = c('blue', 'red', 'green', 'magenta')
minmaxx <- c(min(X_train[,1]), max(X_train[,1]))
plot(minmaxx, minmaxx, type='n', main='Synthetic data', ylab='', xlab='')
for(c in 1:C){
    points(X_train[y_train==c-1,1], X_train[y_train==c-1,2], pch=20, col= Color[c]);
  }
points(X_test[,1], X_test[,2], pch=4);

## K-nearest neighbors
K = 5; # Number of neighbors

# Use knnclassify to find the K nearest neighbors
X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames
X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
y_test_est <- knn(X_traindf, X_testdf, cl=y_train, k = K, prob = FALSE, algorithm="kd_tree")

# Plot estimated classes for test data
dev.new()
minmaxx <- c(min(X_test[,1]), max(X_test[,1]))
plot(minmaxx, minmaxx, main='Synthetic data', ylab='', xlab='', type="n")
for(c in 1:C){
    points(X_test[y_test_est==c-1,1], X_test[y_test_est==c-1,2], pch=21, col=Color[c]);
  }


# Plot confusion matrix

tr <- classNames[as.numeric(y_test)]
pr <- classNames[as.numeric(y_test_est)]

dev.off()
confmatplot(tr, pr)