
####################################################################################
#Naive Bayes
####################################################################################
#Use script ex7_2_4

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----
#Packages
#----
library(cvTools)
source("naiveBayes.R")
source("predict.naiveBayes.R")

#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")


#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

classlabels <- dat[,5] #If only four classes as a test use 1:97
X <- dat[,-c(1,4,5,6,7,8)] 
#NOTE: removing so that there is no categorical variables.
y <- as.numeric(as.factor(classlabels))
y <- y-1
attributeNames <- colnames(X)

# Info about the data
N = 126;
M = 10;
C = 8;


K <- 10;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()


# Parameters for naive Bayes classifier
#Distribution <- c('normal', 'mvmn', 'normal', 'mvmn')
Distribution <- 'normal'
Prior <- 'empirical'

# Variable for classification error
Error <- rep(NA, times=K)

for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', K, sep=''));
  
  # Extract training and test set
  X_train <- X[CV$which!=k, ];
  y_train <- y[CV$which!=k]
  X_test <- X[CV$which==k, ];
  y_test <- y[CV$which==k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  # Fit naive Bayes classifier to training set
  
  mymod <- naiveBayes(X_train, y_train, distribution=Distribution, prior=Prior)
  # Predict model on test data    
  predictRes <- predict.naiveBayes(Xtest=X_test, mod=mymod)
  y_test_est <- predictRes$predictedClass
  
  # Compute error rate
  err <- y_test!=y_test_est
  err[is.na(err)] <- 1 # make test cases that could not be predicted count as errors
  Error[k] <- sum(err) # Count the number of errors
}

# Print the error rate
print(paste('Error rate: ', sum(Error)/sum(CV$TestSize)*100), sep='');
#Error rate: 37,30