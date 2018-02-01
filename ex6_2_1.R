# exercise 6.2.1
rm(list=ls())
source("setup.R")
# Load data
library(cvTools)
library(R.matlab)
dat <- readMat(file.path('Data', 'body.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y

# Linear regression criterion function

# This function takes as input a training and a test set.
#  1. It fits a linear model on the training set using lm.
#  2. It estimates the output of the test set using predict.
#  3. It computes the sum of squared errors.
funLinreg <- function(X_train, y_train, X_test, y_test){
  Xr <- data.frame(X_train)
Xtest <- data.frame(X_test)
  if(dim(as.matrix(X_train))[2]!=0){
xnam <- paste("X", 1:dim(as.matrix(X_train))[2], sep="")
colnames(Xr) <- xnam
colnames(Xtest) <- xnam
(fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse= "+"))))
}else{
xnam <- 1
(fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse= "+"))))
}
mod = lm(fmla, data=Xr)
preds <- predict(mod, newdata = Xtest)
sum((y_test-preds)^2)  
}

## Crossvalidation
# Create crossvalidation partition for evaluation
K = 5;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialize variables
Features <- matrix(rep(NA, times=K*M), nrow=K)
Error_train <- matrix(rep(NA, times=K), nrow=K)
Error_test <- matrix(rep(NA, times=K), nrow=K)
Error_train_fs <- matrix(rep(NA, times=K), nrow=K)
Error_test_fs <- matrix(rep(NA, times=K), nrow=K)

# For each crossvalidation fold
for(k in 1:K){
paste('Crossvalidation fold ', k, '/', K, sep='')
    
    # Extract the training and test set
    X_train <- X[CV$which!=k, ];
    y_train <- y[CV$which!=k];
    X_test <- X[CV$which==k, ];
    y_test <- y[CV$which==k];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)

    # Use 10-fold crossvalidation for sequential feature selection
fsres <- forwardSelection(funLinreg, X_train, y_train, nfeats=10)
    
    # Save the selected features
    Features[k,] = fsres$binaryFeatsIncluded    
    # Compute squared error without feature subset selection
    Error_train[k] = funLinreg(X_train, y_train, X_train, y_train);
    Error_test[k] = funLinreg(X_train, y_train, X_test, y_test);
    # Compute squared error with feature subset selection
    Error_train_fs[k] = funLinreg(X_train[,F], y_train, X_train[,F], y_train);
    Error_test_fs[k] = funLinreg(X_train[,F], y_train, X_test[,F], y_test);
    
    # Show variable selection history
#    mfig(sprintf('(%d) Feature selection',k));
    I = length(fsres$costs) # Number of iterations
par(mfrow=c(1,2))
    # Plot error criterion
    plot(fsres$costs, xlab='Iteration', ylab='Squared error (crossvalidation)', main='Value of error criterion');
    # Plot feature selection sequence
    bmplot(attributeNames, 1:I, fsres$binaryFeatsIncludedMatrix);
}
    

# Display results
print('Linear regression without feature selection:')
print(paste('- Training error:', sum(Error_train)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test)/sum(CV$TestSize)));

print('Linear regression with sequential feature selection:');
print(paste('- Training error:', sum(Error_train_fs)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test_fs)/sum(CV$TestSize)));

# Show the selected features
dev.off()
bmplot(attributeNames, 1:K, Features, xlab='Crossvalidation fold', ylab='', main='Attributes selected');
