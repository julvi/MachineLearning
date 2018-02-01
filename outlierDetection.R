#---------------------------------------------------------------
#Outlier detection
#---------------------------------------------------------------
#use script 11_2_2 as inspiration

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows

#----------------------
#Load the data into R
#----------------------
#Set working directory
#setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
setwd("/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning/Project3")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#Library and sources
source("gausKernelDensity.R")
#install.packages("FNN")
library(FNN) 

#remove phage ID, categorical attributes and attributes related to the host
X<- dat[,-c(1,4,5,6,7,8)] 
X <- scale(X) #Standardize
#X <- X[-77,] The outlier, which is not removed. Try remobing it and see which one is then found
X <-data.frame(X) 

attributeNames <- colnames(X)

#---------------------------------------------
#Gaussian kernel
#---------------------------------------------

# Estimate optimal kernel density width by leave-one-out cross-validation
widths=2^(-10:10)
logP <- rep(NA, times=length(widths))
for(w in 1:length(widths)){
  res = gausKernelDensity(as.matrix(X), widths[w]);
  f <- res$density
  log_f <- res$log_density
  logP[w]=sum(log_f);
}

val <- max(logP)
ind <- which.max(logP)
width=widths[ind]
print(paste('Optimal estimated width is', width))

#Estimate density for each observation not including the observation
# itself in the density estimate
res = gausKernelDensity(as.matrix(X), width)
f <- res$density

#Sort the densities
sortres <- sort(f, index.return=TRUE)
y <- sortres$x
i <- sortres$ix

#Display the index of the lowest density data object
print(i[1])
#Perfect, since it is the same that we found in the first report

# Plot density estimate outlier scores
barplot(y[1:20], main='Method: Gaussian kernel', ylab="Outlier score", xlab="20 observations with lowest score", col="darkblue")


#---------------------------------------------
#K-nearest neighbor density estimator 
#---------------------------------------------
#Use script 11_3_1 as help

# Number of neighbors
K = 5;

# Find the k nearest neighbors
res <- get.knnx(data=X, query=X, k=K+1)
idx <- res$nn.index
D <- res$nn.dist

# Compute the density
density = 1/(rowSums(D[,2:dim(D)[2]])/K)

# Sort the densities
sortres <- sort(density, index.return=TRUE)
y <- sortres$x
i <- sortres$ix

#Display the index of the lowest density data object
print(i[1])

# Plot outlier scores
barplot(y[1:20], main="Method: KNN density", ylab="Outlier score", xlab="20 observations with lowest score", col="blue")


#---------------------------------------------
#K-nearest neigbor average relative density
#---------------------------------------------
#Use script 11_3_1 as help

# Compute the average relative density
avg_rel_density=density/(rowSums(matrix(density[idx[,2:dim(idx)[2]]], nrow=dim(idx)[1]))/K)

# Sort the densities
sortres <- sort(avg_rel_density, index.return=TRUE)
y <- sortres$x
i <- sortres$ix

#Display the index of the lowest density data object
print(i[1])

# Plot outlier scores
barplot(y[1:20], main='Method: KNN average relative density', ylab="Outlier score", xlab="20 observations with lowest score", col="red")
#Here it is more clear to see the outlier

#---------------------------------------------
#Distance to the K’th nearest neighbor
#---------------------------------------------
#Use script 11.2.4 as help

#Neighbors to use
K = 5

#Find the k nearest neighbors
res <- get.knnx(data=X, query=X, k=K+1)
i <- res$nn.index
D <- res$nn.dist

#Outlier score
f = D[,K+1]

#Sort the outlier scores

# Sort the densities
sortres <- sort(f, index.return=TRUE, decreasing=TRUE)
y <- sortres$x
i <- sortres$ix

#Display the index of the lowest density data object
print(i[1])

# Plot kernel density estimate outlier scores
barplot(y[1:20], main='Method: Distance to the 5’th nearest neighbor', xlab="20 observations with lowest score", ylab="Outlier score", col="green")




