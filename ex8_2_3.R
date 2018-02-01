# exercise 8.2.3
rm(list=ls())
source("setup.R")
graphics.off()
library(MASS)
#install.packages("randomForest")
library(randomForest) 
 
# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth7.mat'))
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

# Fit classification trees using the TreeBagger function
classAssignments <- factor(classNames[y+1])
(fmla <- as.formula(paste("classAssignments ~ ", paste(attributeNames, collapse= "+"))))

Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames
B = randomForest(fmla, data=Xdf, ntree=L)

# Make predictions for the whole data set. The predict function outputs a
# vector of factor levels, so y_est can be computed by comparing the returned
# factor levels to the level of the second class. This
# will give a 0 for "Class 1" and a 1 for "Class 2".
y_est = predict(B, Xdf) == factor(classNames)[2];

# Compute error rate
ErrorRate = sum(y!=y_est)/N;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## Plot decision boundary
  predictionFunction <- function(Xgriddf, model, classNames){
    y_est = predict(model, Xgriddf) == factor(classNames)[2];
    y_est
  }
  dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0.5, contourCols='white', model=B, classNames=classNames)
