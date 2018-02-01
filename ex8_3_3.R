# exercise 8.3.3

rm(list=ls())
source("setup.R")
graphics.off()
library(nnet) # install.packages('nnet')

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth1.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

X_train <- dat$X.train
N_train <- dat$N.train
y_train<- dat$y.train

X_test <- dat$X.test
N_test <- dat$N.test
y_test <- dat$y.test
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)
X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames

# Parameters for neural network classifier
NHiddenUnits = 2;  # Number of hidden units
## Fit multiclass neural network to training set
y_trainfact <- factor(y_train)
(fmla <- as.formula(paste("y_trainfact ~ ", paste(attributeNames, collapse= "+"))))
model <- nnet(formula=fmla, data=X_traindf, size=NHiddenUnits)
    
## Compute results on test data
# Get the predicted output for the test data
Y_test_est <- predict(object=model, newdata=X_testdf, type='raw')

# Compute the class index by finding the class with highest probability from the neural
# network
y_test_est = max_idx(Y_test_est);
# Subtract one to have y_test_est between 0 and C-1
y_test_est = y_test_est-1;

# Compute error rate
ErrorRate = sum(y_test!=y_test_est)/N_test;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## Plot results
# Display trained network
plot(model)

# Display decision boundaries
predictionFunction <- function(Xgriddf, model){
Y_test_est <- predict(object=model, newdata=Xgriddf, type='raw')
y_test_est = max_idx(Y_test_est);
y_test_est
}

dbplot(X_testdf, attributeNames, predictionFunction, y=y_test, contourLevels=0.5, contourCols='white', model=model)
