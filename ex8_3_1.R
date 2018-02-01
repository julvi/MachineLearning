# exercise 8.3.1

rm(list=ls())
source("setup.R")
library(neuralnet) #install.packages("neuralnet")

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

# Parameters for neural network classifier
NHiddenUnits = 2;  # Number of hidden units

## Fit one-against-rest models
# Allocate variable for trained networks
net = vector('list', C)

    X_traindf <- data.frame(X_train)
    colnames(X_traindf) <- attributeNames
    X_testdf <- data.frame(X_test)
    colnames(X_testdf) <- attributeNames
(fmla <- as.formula(paste("y_trainc ~ ", paste(attributeNames, collapse= "+"))))
# Fit neural network to training set
for(c in 1:C){
  y_trainc <- y_train==(c-1)
    net[[c]] = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='logistic', linear.output=FALSE, err.fct='sse');
  }

## Compute results on test data
# Allocate variable for test results
Y_test_est = array(rep(NA, times=N_test*C), dim=c(N_test, C))

# For each one-against-rest classifier
for(c in 1:C){
    # Get the predicted output for the test data
     computeres <- compute(net[[c]], X_testdf)
     Y_test_est[,c] <- unlist(computeres$net.result)
  }

# Compute the class index by finding the maximum output from the neural
# network
y_ <- apply(Y_test_est, 1, max)
y_test_est <- apply(Y_test_est, 1, which.max)
# Subtract one to have y_test_est between 0 and C-1
y_test_est = y_test_est-1;

# Compute error rate
ErrorRate = sum(y_test!=y_test_est)/N_test;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''))

## Plot results
# Display decision boundaries
predictionFunction <- function(Xgriddf, model){
     computeres <- compute(model, Xgriddf)
     unlist(computeres$net.result)>0.5
}


par(mfrow=c(1, C))
for(c in 1:C){
  yc <- y==(c-1)
  dbplot(X, attributeNames, predictionFunction, y=yc, contourLevels=0.5, model=net[[c]], contourCols='white', cols=c('green', 'blue'))
  }

predictionFunction <- function(Xgriddf, models,C){
  Y_est <- array(rep(NA, times=dim(Xgriddf)[1]*C), dim=c(dim(Xgriddf)[1], C))
for(c in 1:C){
    # Get the predicted output for the test data
     computeres <- compute(net[[c]], Xgriddf)
     Y_est[,c] <- unlist(computeres$net.result)
  }
     max_idx(Y_est)-1
   }

dev.new()
dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0:3, models=net, C=C)
