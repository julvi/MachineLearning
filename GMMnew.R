####################################################################################
#K-means and hierarchical clustering
####################################################################################
#Use script 10_1_1 and 10_1_5

#Clear workspace ect:
rm(list=ls())
#graphics.off() # close all open graphics windows

# Load packages
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R")
source("setup.R")
library(mixtools) # install.packages("mixtools") # package that can be used to fit a gaussian mixture model. This package does allow random starts of the algorithm.
library(mclust)
library(cvTools)

#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
#setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#remove phage ID, categorical attributes and attributes related to the host
X <- dat[,-c(1, 4, 5, 6, 7, 8)] 

#--------------------
#One out-of-k-coding
#--------------------
#First column 4
col4 = categoric2numeric(dat[,4])
X[11:19]<-col4[[1]]
familynames <- col4[[2]]
colnames(X)[11:19] <- familynames

#Then column 8
col7 = categoric2numeric(dat[,8])
X[20:28]<-col7[[1]]
predictedHost <- col7[[2]]
colnames(X)[20:28] <- predictedHost

#Remove outlier
X=X[-77,]
classlabels <- dat[-77,5] 
X <- data.frame(X)

y <- as.numeric(as.factor(classlabels))
y <- y-1

#standardize
X <- scale(X)
dattmp<-X[,11:28]*(1/sqrt(9))
X[,11:28] <- dattmp
attributeNamesC <- colnames(X)

#Define size
N <- 125
M <- 28
C <-8

# Range of K's to try
KRange = 2:10;
T = length(KRange);

# Allocate variables
#BIC = rep(NA, times=T)
#AIC = rep(NA, times=T)
CVE = rep(0, times=T)

# Variables for cluster validity measures
Entropy = rep(NA, times=T)
Purity = rep(NA, times=T)
Rand = rep(NA, times=T)
Jaccard = rep(NA, times=T)

# Create crossvalidation partition for evaluation
set.seed(1234) # for reproducibility
NumTestSets <- 10
CV <- cvFolds(N, K=NumTestSets)
CV$NumTestSets <- NumTestSets
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# For each model order (number of clusters)
for(t in 1:T){
  # Get the current K
  K <- KRange[t];
  
  # Display information
  print(paste('Fitting model for K =', K));
  
  model <- Mclust(data=X, G=K) # if using the package mclust to fit the model
  
  i = model$classification 
  
  # Compute cluster validities
  res <- clusterval(y, i);
  
  Entropy[t] <- res$Entropy
  Purity[t] <- res$Purity
  Rand[t] <- res$Rand
  Jaccard[t] <- res$Jaccard
  
  # Get BIC and AIC
  #BIC[t] = BIC(model);
  #AIC[t] = AIC(model);
  
  # For each crossvalidation fold
  for(k in 1:CV$NumTestSets){
    # Extract the training and test set
    X_train <- X[CV$which!=k, ];
    X_test <- X[CV$which==k, ];
    
    # Fit model to training set
    model <- Mclust(data=X_train, G=K) # if using the package mclust to fit the model
    
    # Evaluation crossvalidation error
    res <- gmmposterior(model, X_test);
    NLOGL <- res$ll
    CVE[t] = CVE[t]+NLOGL;
  }
}
#par(mar=c(4,4,4,4))
## Plot results
#cols <- c('blue', 'darkgreen', 'red')
#miny <- min(c(BIC, AIC, 2*CVE))
miny <- min(2*CVE)
#maxy <- max(c(BIC, AIC, 2*CVE))
maxy <- max(2*CVE)
#plot(c(KRange[1], KRange[length(KRange)]), c(miny, maxy), main='GMM: Number of clusters', xlab='K')
plot(c(KRange[1], KRange[length(KRange)]), c(miny, maxy), ylab = "CV Error", main='GMM: Number of clusters', xlab='K')
#points(KRange, BIC, col=cols[1]);
#points(KRange, AIC, col=cols[2]);
#points(KRange, 2*CVE, col=cols[3]);
points(KRange, 2*CVE, col='red')
#legend('topleft', legend=c('BIC', 'AIC', 'Crossvalidation'), fill=cols);
legend('topright', legend='Crossvalidation', fill='red');


## Plot cluster validities
cols <- c('blue', 'green', 'red', 'lightblue')
maxy <- max(c(-Entropy, Purity, Rand, Jaccard))
miny <- min(c(-Entropy, Purity, Rand, Jaccard))
plot(c(2,T+1), c(miny, maxy), type='n', main='Cluster validity', xlab='Number of clusters', ylab='')

lines(KRange, -Entropy, col=cols[1]);
lines(KRange, Purity, col=cols[2]);
lines(KRange, Rand, col=cols[3]);
lines(KRange, Jaccard, col=cols[4]);

legend('bottomright', legend=c('Negative Entropy', 'Purity', 'Rand', 'Jaccard'), fill=cols, cex = 0.9)
#par(xpd=TRUE)
#par(mar=c(3,3,3,3))
#legend(4,2, legend=c('Negative Entropy', 'Purity', 'Rand', 'Jaccard'), fill=cols, cex = 0.5)

