# exercise 8.2.1
rm(list=ls())
graphics.off()
source("setup.R")

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth6.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

## Fit model using bootstrap aggregation (bagging)

# Number of rounds of bagging
L = 100;

# Variable for model parameters
#make a empty list
w_est = vector('list', L)

# Weights for selecting samples in each bootstrap
weights = rep(1, times=N)/N;
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
# For each round of bagging

for(l in 1:L){
    # Choose data objects by random sampling with replacement 
    i = discreternd(weights, N);

    # Extract training set
    X_train = X[i, ];
    y_train = y[i];
    X_traindf <- data.frame(X_train)
    colnames(X_traindf) <- attributeNames

    # Fit logistic regression model to training data and save result
    w_est[[l]] = glm(fmla, family=binomial(link="logit"), data=X_traindf);
  }
Xdf <- data.frame(X)
   # Xdf <- data.frame(X[,])
    colnames(X) <- attributeNames
# Evaluate the logistic regression on the training data
plist <- lapply(w_est, FUN=function(model, newd, typ) predict(model, newdata=newd, type=typ), newd=Xdf, typ='response')
p <- matrix(unlist(plist), nrow=N, byrow=FALSE)

# Estimated value of class labels (using 0.5 as threshold) by majority voting
y_est = rowSums(p>.5)>L/2; 

# Compute error rate
ErrorRate = sum(y!=y_est)/N;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## Plot decision boundary
  predictionFunction <- function(Xgriddf, model, typ){
plist <- lapply(w_est, FUN=function(model, newd, typ) predict(model, newdata=newd, type=typ), newd=Xgriddf, typ=typ)
p <- matrix(unlist(plist), nrow=dim(Xgriddf)[1], byrow=FALSE)
rowMeans(p)
  }

  dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0.5, contourCols='white', model=w_est, typ='response')

