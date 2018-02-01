####################################################################################
#ANN
####################################################################################
#Use script ex7_3_2

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----
#Packages
#----
#install.packages("neuralnet")

library(neuralnet) 
library(cvTools)


source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/dbplot.R")
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools")
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R")

#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#> names(dat)
#[1] "Phage_ID"                 "phage_genome_size"        "phage_Gccontent"          "phage_family"            
#[5] "annotated_host"           "host_genome_size"         "host_GCcontent"           "predicted_host"          
#[9] "frac_d"                   "frac_q"                   "Score"                    "Expected"                
#[13] "z"                        "coverage"                 "unique_kmers_in_template" "unique_kmers_in_query" 

classlabels <- dat[,5] #all classes
X <- dat[,-c(1,4,5,6,7,8)] 
#NOTE: removing attributes related to the host and categorical variables.TO DO: apply one out of K coding

#Extract numeric class assignments
y <- as.numeric(as.factor(classlabels))
y <- y-1
attributeNames <- colnames(X)
# substitute spaces with dots to make handling of columns in data matrix easier
#attributeNames <- gsub(' ', '.', attributeNames)

dim(X)
#[1] 126 10
# Information about the data
N = 126;
M = 10;
length(unique(y))
C = 8;

# 10-fold crossvalidation
K = 10;
set.seed(1234) # for reproducibility
#cvFolds splits n observations into K groups to be used for (repeated K-fold cross-validation)
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 2;  # Number of hidden units
NTrain = 2; # Number of re-trains of neural network

# Variable for classification error
Error = rep(NA, times=K)

(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))

for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', K, sep=''))
  
  # Extract training and test set
  #As training set take all observations that are not the k group
  X_train <- X[CV$which!=k, ];
  y_train <- y[CV$which!=k];
  #As test set take observations that are the k group
  X_test <- X[CV$which==k, ];
  y_test <- y[CV$which==k];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  
  # Fit neural network to training set
  MSEBest = Inf;
  for(t in 1:NTrain){
    #sse: sum of squared errors
    netwrk = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='logistic', linear.output=FALSE, err.fct='sse');
    classification <- unlist(netwrk$net.result)>0.5
    #mean squared error
    mse <- sum((classification-y_train)^2)
    #check if the mean square error is less then the previous one
    if(mse<MSEBest){
      bestnet <- netwrk
      MSEBest <- mse
    }
  }
  
  # Predict model on test data
  #compute computes the outputs of all neurons for specific arbitrary covariate vectors
  computeres <- compute(bestnet, X_testdf)
  classification <- computeres$net.result>0.5
  y_test_est = classification>.5;    
  
  # Compute error rate
  Error[k] = sum(y_test!=y_test_est); # Count the number of errors
}

# Print the error rate
print(paste('Error rate: ', sum(Error)/sum(CV$TestSize)*100, '%', sep=''));
#Error rate: 88.66 - 4 hidden units 2 trials
#Error rate: 87.62 - 2 hidden units 2 trials
#Error rate: 91.2698412698413% 2 hidden units 2 trials

# Display the trained network (given for last cross-validation fold)
plot(netwrk)

# Display the decision boundary (given for last cross-validation fold) 
predictionFunction <- function(X_traindf, netwrk){
  compres <- compute(netwrk, X_traindf)
  probs <- matrix(compres$net.result, nrow=sqrt(dim(X_traindf)[1]), byrow=FALSE) #>0.5
  probs
}
dbplot(X, attributeNames, predictionFunction, y=y, contourLevels=0.5, contourCols='white', netwrk=bestnet)

#NOTE: I am not sure how ANN predict which of the eight classes it is.
#try changing the retrains to 2.
#Also find a method to predict the number og hidden units to use - CV?
