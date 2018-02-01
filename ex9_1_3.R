# exercise 9.1.3
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

# Maximum number of clusters
K = 10;

# Allocate variables
Entropy = rep(NA, times=K)
Purity = rep(NA, times=K)
Rand = rep(NA, times=K)
Jaccard = rep(NA, times=K)
for(k in 1:K){
    # Run k-means
    kmeansres = kmeans(Xdf, k, iter.max=100);
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
