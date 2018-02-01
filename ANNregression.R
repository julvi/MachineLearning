####################################################################################
#ANN regression
####################################################################################
#Use script ex7_3_6 as help.

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
#source("/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2/02450ToolboxR")

#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)
#names(dat)
#[1] "Phage_ID"                 "phage_genome_size"        "phage_Gccontent"          "phage_family"            
#[5] "annotated_host"           "host_genome_size"         "host_GCcontent"           "predicted_host"          
#[9] "frac_d"                   "frac_q"                   "Score"                    "Expected"                
#[13] "z"

classlabels <- dat[,5] 
X <- dat[,-c(1,4,5,8)] #I think in this case we can keep the information about the host since we are predicting information
#about the phage


#NOTE: removing so that there is no categorical variables.TO DO: apply one out of K coding
attributeNames <- colnames(X)

# substitute spaces with dots to make handling of columns in data matrix easier
#attributeNames <- gsub(' ', '.', attributeNames)


names(X)
#[1] "phage_genome_size"        "phage_Gccontent"          "host_genome_size"         "host_GCcontent"          
#[5] "frac_d"                   "frac_q"                   "Score"                    "Expected"                
#[9] "z"                        "coverage"                 "unique_kmers_in_template" "unique_kmers_in_query"     


# predict phage_GCcontent
y=X[,2]
#scaling and centering a matrix-like object
y = scale(y)
X <- X[,-2]
XRs = scale(X)
X = data.frame(XRs)
attributeNames <- attributeNames[-2]

# Information about the data
dim(X)
N = 126;
M = 11;
C = 8;


# K-fold crossvalidation
K = 10;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 1;  # Number of hidden units
NTrain = 1; # Number of re-trains of neural network

# Variable for classification error
Error = rep(NA, times=K)
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', K, sep=''))
  
  # Extract training and test set
  X_train <- X[CV$which!=k, ];
  y_train <- y[CV$which!=k];
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
    netwrk = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='tanh', linear.output=TRUE, err.fct='sse');
    mse <- sum((unlist(netwrk$net.result)-y_train)^2)
    
    if(mse<MSEBest){
      bestnet <- netwrk
      MSEBest <- mse
    }
  }
  # Predict model on test data
  
  computeres <- compute(bestnet, X_testdf)
  y_test_est = unlist(computeres$net.result)
  
  # Compute error rate
  Error[k] = sum((y_test-y_test_est)^2); # Count the number of errors
  
}


# Print the error rate
print(paste('Mean Sum of Squares Error (MSSE): ', sum(Error)/sum(CV$TestSize), sep=''));
#MSSE: 135.6 for 3 runs and 1 hidden layers
#MSSE: 147.6 for 3 runs and 2 hidden layers
#MSSE: 136.3 for 3 runs and 3 hidden layers
#MSSE: 135.7 for 3 runs and 4 hidden layers
#MSSE: 147.6 for 3 runs and 5 hidden layers
#(MSSE): 0.917183871930417 for 1 run and 1 hidden layer
#NOTE: Why does the function "overload" when the values are standardized?

# Display the trained network (given for last cross-validation fold)
plot(bestnet);
