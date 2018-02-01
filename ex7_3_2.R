# exercise 7.3.2
rm(list=ls())
source("setup.R")
graphics.off()
#install.packages("neuralnet")
library(neuralnet) 
library(cvTools)

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'xor.mat'))
X <- dat$X
N <- as.numeric(dat$N)
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- as.numeric(dat$M)
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# K-fold crossvalidation
K = 10;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 1;  # Number of hidden units
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
        netwrk = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='logistic', linear.output=FALSE, err.fct='sse');
        classification <- unlist(netwrk$net.result)>0.5
        mse <- sum((classification-y_train)^2)

        if(mse<MSEBest){
          bestnet <- netwrk
          MSEBest <- mse
        }
      }
    
    # Predict model on test data
        
        computeres <- compute(bestnet, X_testdf)
        classification <- computeres$net.result>0.5
    y_test_est = classification>.5;    
    
    # Compute error rate
    Error[k] = sum(y_test!=y_test_est); # Count the number of errors
      }

# Print the error rate
print(paste('Error rate: ', sum(Error)/sum(CV$TestSize)*100, '%', sep=''));

# Display the trained network (given for last cross-validation fold)
plot(netwrk)

# Display the decision boundary (given for last cross-validation fold) 
  predictionFunction <- function(X_traindf, netwrk){
compres <- compute(netwrk, X_traindf)
probs <- matrix(compres$net.result, nrow=sqrt(dim(X_traindf)[1]), byrow=FALSE) #>0.5
probs
  }
  dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0.5, contourCols='white', netwrk=bestnet)

