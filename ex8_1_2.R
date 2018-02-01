# exercise 8.1.2
rm(list=ls())
source("setup.R")
library(cvTools)

# Load data

library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# Remove all attributes except "Alcohol"
i = 11; # Attribute indices to keep
attributeNames <- attributeNames[i]

## Crossvalidation
# Create crossvalidation partition for evaluation
# using stratification and 50 pct. split between training and test 
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=2)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Extract the training and test sets, retaining only the attributes that are to be kept
    X_train <- X[CV$which!=1, ];
    y_train <- y[CV$which!=1];
    X_test <- X[CV$which==2, ];
    y_test <- y[CV$which==2];
    CV$TrainSize[1] <- length(y_train)
    CV$TestSize[2] <- length(y_test)

X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames

## Fit model
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))

# Fit logistic regression model to predict the type of wine
w_est = glm(fmla, family=binomial(link="logit"), data=X_traindf);

# Evaluate the logistic regression on the test data
p = predict(w_est, newdata=X_testdf, type="response")

## Plot receiver operating characteristic (ROC) curve
rocplot(p, y_test);

# We'll plot a confusion matrix as well
y_test_est = p>.5;
dev.new()
confmatplot(classNames[y_test+1], classNames[y_test_est+1]);
#graphics.off() # close all figures
