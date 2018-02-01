# exercise 7.1.2
rm(list=ls())
source('setup.R')
library(FNN)
library(cvTools)

# Load data
source("Scripts/ex4_1_1.R")

# Leave-one-out crossvalidation
CV <- cvFolds(N, K=N);
K = N

# K-nearest neighbors parameters
L = 40; # Maximum number of neighbors

# Variable for classification error
Error = array(rep(NA, times=K*L), dim=c(K,L))

for(k in 1:K){ # For each crossvalidation fold
        print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))

    # Extract training and test set
    X_train <- X[CV$which!=k, ];
    y_train <- y[CV$which!=k];
    X_test <- X[CV$which==k, ];
    y_test <- y[CV$which==k];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)

X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames
X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
        
    for(l in 1:L){ # For each number of neighbors
        
        # Use knnclassify to find the l nearest neighbors
y_test_est <- knn(X_traindf, X_testdf, cl=y_train, k = l, prob = FALSE, algorithm="kd_tree")
        
        # Compute number of classification errors
        Error[k,l] = sum(y_test!=y_test_est); # Count the number of errors
      }
  }

## Plot the classification error rate
plot(colSums(Error)/sum(CV$TestSize)*100, main='Error rate', xlab='Number of neighbors', ylab='Classification error rate (%)', pch=20, type='l');
