# exercise 8.3.2

rm(list=ls())
source("setup.R")

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

## Fit one-against-rest models
# Fit logistic regression model to training set
w_est <- vector('list', C)
(fmla <- as.formula(paste("y_trainc ~ ", paste(attributeNames, collapse= "+"))))
for(c in 1:C){
  y_trainc <- y_train==(c-1)
    w_est[[c]] = suppressWarnings(glm(fmla, family=binomial(link="logit"), data=X_traindf));
  }
    
## Compute results on test data
# For each one-against-rest classifier
plist <- lapply(w_est, FUN=function(model, newd, typ) predict(model, newdata=newd, type=typ), newd=X_testdf, typ='response')
Y_test_est <- matrix(unlist(plist), nrow=N_test, byrow=FALSE)

# Compute the class index by finding the maximum output from the logistic
# regression models
y_test_est=max_idx(Y_test_est);
# Subtract one to have y_test_est between 0 and C-1
y_test_est = y_test_est-1;

# Compute error rate
ErrorRate = sum(y_test!=y_test_est)/N_test;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## Plot results
# Display decision boundaries
predictionFunction <- function(Xdf, model){
     predict(model, newdata=Xdf, type='response')
}
par(mfrow=c(1,C))
for(c in 1:C){
  y_testc <- y_test==(c-1)
    dbplot(X_testdf, attributeNames, predictionFunction, y=y_testc, contourLevels=0.5, contourCols='white', model=w_est[[c]])
  }

dev.new()

predictionFunction <- function(Xgriddf, models){
plist <- lapply(X=models, FUN=function(model, newd) predict(model, newdata=newd, type='response'), newd=Xgriddf)
Y_test_est <- matrix(unlist(plist), nrow=dim(Xgriddf)[1], byrow=FALSE)
y_test_est=max_idx(Y_test_est)
y_test_est <- y_test_est-1
}

dbplot(X_testdf, attributeNames, predictionFunction, y=y_test, contourLevels=0.5, contourCols='white', models=w_est)
