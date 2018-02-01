# exercise 9.2.1
rm(list=ls())
graphics.off()
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

## Hierarchical clustering
# Maximum number of clusters
Maxclust = 4;

# Compute hierarchical clustering
#hc <- hclust(dist(Xdf), method="single")
hc <- hclust(dist(Xdf), method="average")
# Compute clustering by thresholding the dendrogram
i <- cutree(hc, k = Maxclust)

## Plot results

# Plot dendrogram
plclust(hc)


# Plot data
dev.new()
clusterplot(Xdf, y, i, main='Hierarchical');
