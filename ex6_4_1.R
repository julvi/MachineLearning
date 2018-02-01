# exercise 6.4.1
rm(list=ls())
library(rpart)
library(cvTools)
source("setup.R")
graphics.off()
# Load data
library(R.matlab)
dat <- readMat(file.path('Data','wine2.mat'))
X <- dat$X
N <- as.numeric(dat$N)
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- as.numeric(dat$M)
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

## Crossvalidation

# Create 10-fold crossvalidation partition for evaluation
K = 10;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialize variables
Error_LogReg = rep(NA, times=K)
Error_DecTree = rep(NA, times=K)

# For each crossvalidation fold
for(k in 1:K){
    print(paste('Crossvalidation fold ', k, '/', K, sep=''));
    
    # Extract the training and test set
    X_train <- X[CV$which!=k, ];
    y_train <- y[CV$which!=k];
    X_test <- X[CV$which==k, ];
    y_test <- y[CV$which==k];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)
    Xdatframe_train <- data.frame(X_train)
colnames(Xdatframe_train) <- attributeNames
    Xdatframe_test <- data.frame(X_test)
colnames(Xdatframe_test) <- attributeNames
    

# construct formula to fit automatically to avoid typing in each variable name
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))

    # Logistic regression 
    w_est = glm(fmla, family=binomial(link="logit"), data=Xdatframe_train);
    y_est = predict.glm(w_est, newdata=Xdatframe_test, type="response")
    Error_LogReg[k] = sum(y_test!=(y_est>.5));

    # Decision tree
mytree <- rpart(fmla, data=Xdatframe_train,control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")
    Error_DecTree[k] = sum(classNames[y_test+1] != classNames[predict(mytree, newdat=Xdatframe_test, type="vector")]);
    
  }

# Determine if classifiers are significantly different
# mfig('Error rates');
errors <- data.frame(cbind(Error_LogReg/CV$TestSize, Error_DecTree/CV$TestSize)*100)
colnames(errors) <- c('Logistic regression', 'Decision tree')
boxplot(errors, ylab="Error rate ()%")

testresult <- t.test(Error_LogReg, Error_DecTree)
if(testresult$p.value < 0.05){
    print('Classifiers are significantly different');
}else{
    print('Classifiers are NOT significantly different');
}
