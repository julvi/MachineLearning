# exercise 9.1.1
rm(list=ls())
source("setup.R")

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth1.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)
Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames

## K-means clustering

# Number of clusters
K = 4;

# Run k-means
res = kmeans(Xdf, K);
i <- res$cluster
Xc <- res$centers

## Plot results
# Plot data
clusterplot(Xdf, y, i, Xc, main='K-means');
