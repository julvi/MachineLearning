####################################################################################
#K-means
####################################################################################
#Use script 9_1_5

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#Library and sources
source("categoric2numeric.R")
source("clusterval.R")


#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("/Users/juliavi/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project3')
#source("/Users/juliavi/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/setup.R")
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")
#source("clusterplot.R")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#remove phage ID, categorical attributes and attributes related to the host
X <- dat[,-c(1,4,5,6,7,8)] 

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

## K-means clustering

# Maximum number of clusters
K = 10;

# Run k-means
res <- kmeans(X, K);
i <- res$cluster
Xc <- res$centers

##########-----------------------------------------------
# Test the results depending on the number of clusters
##########-----------------------------------------------

# Allocate variables
Entropy = rep(NA, times=K)
Purity = rep(NA, times=K)
Rand = rep(NA, times=K)
Jaccard = rep(NA, times=K)

for(k in 1:K){
  # Run k-means
  kmeansres = kmeans(X, k, iter.max=100);
  i <- kmeansres$cluster
  # Compute cluster validities
  res <- clusterval(y, i);
  
  Entropy[k] <- res$Entropy
  Purity[k] <- res$Purity
  Rand[k] <- res$Rand
  Jaccard[k] <- res$Jaccard
}

## Plot results
cols <- c('blue', 'green', 'red', 'lightblue')
maxy <- max(c(-Entropy, Purity, Rand, Jaccard))
miny <- min(c(-Entropy, Purity, Rand, Jaccard))
plot(c(1,K), c(miny, maxy), type='n', main='Cluster validity', xlab='Number of clusters', ylab='')
lines(1:K, -Entropy, col=cols[1]);
lines(1:K, Purity, col=cols[2]);
lines(1:K, Rand, col=cols[3]);
lines(1:K, Jaccard, col=cols[4]);
legend('bottomright', legend=c('Negative Entropy', 'Purity', 'Rand', 'Jaccard'), fill=cols)
#Note: move the legend








# ## Plot results
# 
# # Number of images to plot
# L = 5;
# 
# # Get some random image indices
# j = sample(x=1:N, size=L)
# 
# graphics.off()
# # Plot centroids
# n1 = ceiling(sqrt(K/2)); n2 = ceiling(K/n1);
# par(mfrow=c(n1, n2), mar=c(0,0,2,0), xaxt='n', yaxt='n')
# for(k in 1:K){
#   centroid <- Xc[k,]
#   dim(centroid) <- c(x, y, c)
#   plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Centroid of class", k))
#   image <- as.raster(centroid)
#   rasterImage(image, 1, 1, x, y, interpolate=FALSE)
# }
# 
# # Plot random images and corresponding centroids
# dev.new()
# par(mfrow=c(L,2), mar=c(0,0,2,0), xaxt='n', yaxt='n')
# for(l in 1:L){
#   centroid <- X[j[l],]
#   dim(centroid) <- c(x, y, c)
#   plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Example image from class", i[j[l]]))
#   image <- as.raster(centroid)
#   rasterImage(image, 1, 1, x, y, interpolate=FALSE)
#   
#   centroid <- Xc[i[j[l]],]
#   dim(centroid) <- c(x, y, c)
#   plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Centroid of class", i[j[l]]))
#   image <- as.raster(centroid)
#   rasterImage(image, 1, 1, x, y, interpolate=FALSE)
# }
