# exercise 7.3.6
rm(list=ls())
graphics.off() # close all open graphics windows
source("setup.R")
library(neuralnet) #install.packages("neuralnet")
library(cvTools)

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N <- as.numeric(dat$N)
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- as.numeric(dat$M)
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# predict alcohol content
y=X[,11];
X <- X[,-11]
attributeNames <- attributeNames[-11]

# K-fold crossvalidation
K = 10;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 2;  # Number of hidden units
NTrain = 1; # Number of re-trains of neural network

# Variable for classification error
Error = rep(NA, times=K)
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
for(k in 1:K){ # For each crossvalidation fold
        print(paste('Crossvalidation fold ', k, '/', K, sep=''))

    # Extract training and test set
    X_train <- X[CV$which!=k, ];
    y_train <- y[CV$which!=k];
    X_test <- X[CV$which==k, ];
    y_test <- y[CV$which==k];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)
        
    X_traindf <- data.frame(X_train)
    colnames(X_traindf) <- attributeNames
    X_testdf <- data.frame(X_test)
    colnames(X_testdf) <- attributeNames
        
    # Fit neural network to training set
    MSEBest = Inf;
    for(t in 1:NTrain){
        netwrk = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='tanh', linear.output=TRUE, err.fct='sse');
        mse <- sum((unlist(netwrk$net.result)-y_train)^2)

        if(mse<MSEBest){
          bestnet <- netwrk
          MSEBest <- mse
        }
      }
            # Predict model on test data
        
        computeres <- compute(bestnet, X_testdf)
    y_test_est = unlist(computeres$net.result)
    
    # Compute error rate
    Error[k] = sum((y_test-y_test_est)^2); # Count the number of errors

      }
    

# Print the error rate
print(paste('Mean Sum of Squares Error (MSSE): ', sum(Error)/sum(CV$TestSize), sep=''));

# Display the trained network (given for last cross-validation fold)
plot(bestnet);
