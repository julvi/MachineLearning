# exercise 8.2.2
rm(list=ls())
source("setup.R")
graphics.off()
# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth5.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

## Fit model using boosting (AdaBoost)

# Number of rounds of boosting
L = 100;

# Allocate variable for model parameters
w_est = vector('list', L)

# Allocate variable for model importance weights
alpha = rep(NA, times=L)

# Weights for selecting samples in each round of boosting
weights = rep(1, times=N)/N

## Boosting (AdaBoost)
# For each round of boosting
l = 1;
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
while(l<=L){
    # Choose data objects by random sampling with replacement 
    i = discreternd(weights, N);

    # Extract training set
    X_train = X[i, ];
    y_train = y[i];
    X_traindf <- data.frame(X_train)
    colnames(X_traindf) <- attributeNames
    Xdf <- data.frame(X)
    colnames(Xdf) <- attributeNames

    # Fit logistic regression model to training data and save result
    w_est[[l]] = suppressWarnings(glm(fmla, family=binomial(link="logit"), data=X_traindf));
    
    # Make predictions on the whole data set
    p = predict(w_est[[l]], newdata=Xdf, type='response')
    y_est = p>0.5;
    
    # Compute error rate
    ErrorRate = sum(weights*(y!=y_est))/N;
    
    # Restart if error rate is above 50%
    # From Tan et al. p. 289: "In addition, if any intermediate rounds
    # produce an error rate higher than 50%, the weights are reverted back
    # to their original uniform values (...) and the resampling procedure
    # is repeated.
    if(ErrorRate > .5){
        # Reset weights and do a new round
        weights = ones(N,1)/N;
      }else{
        # Compute model importance weight
        # Tan et al. p. 288, equation below equation 5.68
        alpha[l] = .5*log((1-ErrorRate)/ErrorRate);

        # Update weights
        # Tan et al. p. 288, equation 5.69
        weights[y==y_est] = weights[y==y_est]*exp(-alpha[l]);
        weights[y!=y_est] = weights[y!=y_est]*exp(alpha[l]);
        weights = weights/sum(weights);
        
        # Next round
        l = l+1;
      }
}

# Normalize the importance weights
alpha = alpha/sum(alpha);

# Evaluate the logistic regression on the training data
plist <- lapply(w_est, FUN=function(model, newd, typ) predict(model, newdata=newd, type=typ), newd=Xdf, typ='response')
p <- matrix(unlist(plist), nrow=N, byrow=FALSE) # N is number of observations

# From Tan el al. p. 288: "Instead of using a majority voting scheme, the
# prediction made by each classifier (...) is weighted according to
# (alpha).

y_est <- rowSums(t(apply(p>0.5, 1, function(x, weights) x*weights, weights=alpha)))>0.5

# Compute error rate
ErrorRate = sum(y!=y_est)/N;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## Plot decision boundary
predictionFunction <- function(Xgriddf, model, typ, alpha){
plist <- lapply(w_est, FUN=function(model, newd, typ) predict(model, newdata=newd, type=typ), newd=Xgriddf, typ='response')
p <- matrix(unlist(plist), nrow=dim(Xgriddf)[1], byrow=FALSE)
y_est <- rowSums(t(apply(p>0.5, 1, function(x, weights) x*weights, weights=alpha)))
y_est
  }

dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0.5, contourCols='white', model=w_est, typ='response', alpha=alpha)
