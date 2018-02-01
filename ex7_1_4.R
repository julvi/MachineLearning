# exercise 7.1.4
rm(list=ls())
source('setup.R')
graphics.off()
library(FNN)

# Load data

library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

y = X[, 11];
Xr = X[, 1:10];

# K-nearest neighbors parameters
L = 40; # Maximum number of neighbors

res <- get.knn(X, k=L)
IDX <- res$nn.index

Error <- array(rep(NA, times=N*L), dim=c(N,L))
for(ll in 1:L){ # For each crossvalidation fold
    
    y_est= apply(matrix(as.vector(y)[IDX[,1:ll]], ncol=ll), 1, mean)
    Error[,ll] = (y-y_est)^2;

  }

## Plot the Regression least squares error
plot(colSums(Error)/N, main='LS Error', xlab='Number of neighbors', ylab='LS Error (%)', type='l');
