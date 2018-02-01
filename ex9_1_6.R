# exercise 9.1.5
rm(list=ls())
source("setup.R")
# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'digits.mat'))
#dat = readMat(’Data/digits.mat’’)
X <- dat$X
N <- dim(X)[1]
M <- dim(X)[2]

# Image resolution and number of colors
#x = 40; 
x = 16;
#y = 40;
y = 16;
c = 1;

## K-means clustering

# Maximum number of clusters
K = 10;

# Run k-means (This will take a while to run on a large data set)
res <- kmeans(X, K);
i <- res$cluster
Xc <- res$centers
    

## Plot results

# Number of images to plot
L = 5;

# Get some random image indices
j = sample(x=1:N, size=L)

graphics.off()
# Plot centroids
n1 = ceiling(sqrt(K/2))
n2 = ceiling(K/n1);
par(mfrow=c(n1, n2), mar=c(0,0,2,0), xaxt='n', yaxt='n')
for(k in 1:K){
  centroid <- Xc[k,]
  dim(centroid) <- c(x, y)
  plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Centroid of class", k))
  #image <- as.raster(centroid)
  image(as.matrix(centroid))
  }

# Plot random images and corresponding centroids
dev.new()
par(mfrow=c(L,2), mar=c(0,0,2,0), xaxt='n', yaxt='n')

for(l in 1:L){
  centroid <- X[j[l],]
  dim(centroid) <- c(x, y)
  plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Example image from class", i[j[l]]))
  #image <- as.raster(centroid)
  image(as.matrix(centroid))
  
  centroid <- Xc[i[j[l]],]
  dim(centroid) <- c(x, y)
  plot(c(1, x), c(1, y), type = "n", xlab="", ylab="", main=paste("Centroid of class", i[j[l]]))
  #image <- as.raster(centroid)
  image(as.matrix(centroid))
  }

