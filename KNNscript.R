####################################################################################
# Fit KNN
####################################################################################
#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----
#Packages
#----
#install.packages("FNN")
library(FNN)
#install.packages("cvTools")
library(cvTools)
source('categoric2numeric.R')

#----------------------
#Load the data into R
#----------------------
#Set working directory
#setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#Extract class labels of observations
classlabels <- dat[,5]
XC <- dat[,-c(1,4,5,6,7,8)] 
#NOTE: removing so that there is no categorical variables.

#Remove the fith column because it is with the class labels
#Remove the first because it is the ID
# 4 is the phage family - categorical
# 8 is the predicted host
#Remove host GC content because it is related to the host and also host genome size (6 og 7)

#Make the 2 categorical attributes into something that we can use
#First column 4
col4 = categoric2numeric(dat[,4])
XC[11:19]<-col4[[1]]
familynames <- col4[[2]]
colnames(XC)[11:19] <- familynames

#Then column 8
col7 = categoric2numeric(dat[,8])
XC[20:28]<-col7[[1]]
familynames7 <- col7[[2]]
colnames(XC)[20:28] <- familynames7
#Do we need to divide by the number of columns when we are not standardizing?

yC <- as.numeric(as.factor(classlabels))
yC <- yC-1
attributeNamesC <- colnames(XC)

#define the size - NOTE: we start with a test size
Nk = 126;
Mk = 28;
Ck = 8;


# Leave-one-out crossvalidation
CV <- cvFolds(Nk, K=Nk);
K = Nk

# K-nearest neighbors parameters
L = 40; # Maximum number of neighbors

# Variable for classification error
Error = array(rep(NA, times=K*L), dim=c(K,L))

for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_train <- XC[CV$which!=k, ];
  y_train <- yC[CV$which!=k];
  X_test <- XC[CV$which==k, ];
  y_test <- yC[CV$which==k];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNamesC
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNamesC
  
  for(l in 1:L){ # For each number of neighbors
    
    # Use knnclassify to find the l nearest neighbors
    y_test_est <- knn(X_traindf, X_testdf, cl=y_train, k = l, prob = FALSE, algorithm="kd_tree")
    
    # Compute number of classification errors
    Error[k,l] = sum(y_test!=y_test_est); # Count the number of errors
  }
}


## Plot the classification error rate
plot(colSums(Error)/sum(CV$TestSize)*100, main='Error rate', xlab='Number of neighbors', ylab='Classification error rate (%)', pch=20, type='l');
#Buy using this we would choose around 3 as the optimal number og neighbors - I think

#NOTE: What more do we need to do with KNN, what plots ect should we show?