##################################################################################################
#Analyze our data as a regression problem using linear regression.
##################################################################################################
#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----
#Packages
#----
source('forwardSelection.R')
source('~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/is.scalar.R')
source('~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/bmplot.R')
library(rpart)
library(cvTools)
library(R.matlab)


#----------------------
#Load the data into R
#----------------------
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /IntroductionToMachineLearning")
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#Extract class labels of observations
classlabels <- dat[-77,5] #77 is the outlier

#Remove the fith column because it is with the class labels and remove the first because it is the ID
X <- dat[-77,-c(1,5)] 
#take out host genome size
dim(X)

#Extract attributes, i.e. names of attributes
attributeNames <- colnames(X)

#Extract the class names present in data
classNames <- unique(classlabels)

#Extract numeric class assignments
y <- as.numeric(as.factor(classlabels))
y <- y-1

# Number data objects, attributes, and classes
N = 125;
M = 14;
C = 8;

Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames

#convert the columns into factors
Xdatframe[,attributeNames] <- lapply(Xdatframe[,attributeNames] , factor)

# check that Xdatframe represents data as categorical variables
summary(Xdatframe)

#The y value is the phage GC content. Normalize
yR = X[, 2]
yR = scale(yR)

#Do not include the Phage GC content and predicted_host and phage_family (the last two are categorical
#variables). Normalize the data
XR = data.frame(X[, -c(2,6,3)])
XRs = scale(XR)
XR = data.frame(XRs)
#colMeans(XR1) controle the normalization
#sd(XR1[,4])

RattributeNames <- colnames(XR)
###----------------------------------------------------------------------------------
##Fit a full model
###----------------------------------------------------------------------------------

(fmla <- as.formula(paste("yR ~ ", paste(RattributeNames, collapse= "+"))))
w_est = lm(fmla, data=XR)


#Make a scatterplot of the predicted values versus the true values
y_est = w_est$fitted.values
plot(yR, y_est, xlab='Phage GC content (true)', ylab='Phage GC content (estimated)', main='Phage GC content', pch=20)

# Make a histogram of the residual error
hist(yR-y_est, breaks=41, main="Residual error")
# Make a scatter plot of the residual errors. They should be randomly distributed around zero - ok.
plot(yR-y_est)

#Now instead of using all the attributes, select some with forward regression. Use scriot 6_2_1.
# fsres <- forwardSelection(funLinreg, XR, yR, nfeats=10) TEST

#--
#The function
#--
dev.off()
#Changes after defining the dependent value
N <- 125
M <- 11

funLinreg <- function(X_train, y_train, X_test, y_test){
  
  Xr <- data.frame(X_train)
  Xtest <- data.frame(X_test)
  if(dim(as.matrix(X_train))[2]!=0){
    xnam <- paste("X", 1:dim(as.matrix(X_train))[2], sep="")
    colnames(Xr) <- xnam
    colnames(Xtest) <- xnam
    (fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse= "+"))))
    #print(fmla)
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
K = 10;
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

for(k in 1:K){
  print(paste('Crossvalidation fold ', k, '/', K, sep=''))
  
  # Extract the training and test set
  X_train <- XR[CV$which!=k, ];
  y_train <- yR[CV$which!=k];
  X_test <- XR[CV$which==k, ];
  y_test <- yR[CV$which==k];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold crossvalidation for sequential feature selection
  #fsres <- forwardSelection(funLinreg, X_train, y_train, nfeats=10)
  fsres <- forwardSelection(funLinreg, X_train, y_train, minCostImprovement=0.005, stoppingCrit='minCostImprovement', cvK=10) 
  
  # Save the selected features
  Features[k,] = fsres$binaryFeatsIncluded    
  
  # Compute squared error with feature subset selection
  Error_train_fs[k] = funLinreg(X_train[,F], y_train, X_train[,F], y_train)
  Error_test_fs[k] = funLinreg(X_train[,F], y_train, X_test[,F], y_test)
  
  # Show variable selection history
  I = length(fsres$costs) # Number of iterations
  par(mfrow=c(1,1))
  
  # Plot error criterion
  plot(fsres$costs, xlab='Iteration', ylab='Squared error (crossvalidation)', main='Value of error criterion')
  
  # Plot feature selection sequence
  bmplot(RattributeNames, 1:I, fsres$binaryFeatsIncludedMatrix)
  
}

#Print results for the 10 fold cross validation
print('Linear regression with sequential feature selection:');
print(paste('- Training error:', sum(Error_train_fs)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test_fs)/sum(CV$TestSize)));

#Show selected features
par(mfrow=c(1,1))
par(mar=c(4.1, 11, 4.1, 2.1))
bmplot(RattributeNames, 1:K, Features, xlab='Crossvalidation fold', ylab='', main='Attributes selected')


#Save errors for test
Error_LinReg = Error_test_fs

#----
#Plot of the residual error
#----
#The model we get from the cross validation
w_estT = lm(yR ~ host_genome_size + host_GCcontent + frac_q, data=XR)

#Make a scatterplot of the predicted values versus the true values
y_estT = w_estT$fitted.values
plot(yR, y_estT, xlab='Phage GC content (true)', ylab='Phage GC content (estimated)', main='Phage GC content', pch=20)

# Make a histogram of the residual error
hist(yR-y_estT, breaks=41, main="Residual error", ylab="Residual error")
# Make a scatter plot of the residual errors. They should be randomly distributed around zero
plot(yR-y_estT, ylab="Residual error",xlab="Observation" )
#NOTE: I think there are some (relatively) very high neative values, indicating that there is something our model
#is not capturing

#Plot residual error versus attribute
#NOTE: I am not sure what kind of plot they are thinking about, when they write it like this in 
#the project description


#What are the eﬀects of the selected attributes in terms of predicting the data.
#Look at the coefficients and interpret
w_estT$coefficients
#The intercept is zero because we have stanardized. 
#frac_d has a high influence
#coverage has a high influence
# host_GCcontent has a high influence

