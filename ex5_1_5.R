# exercise 5.1.5

# Load the data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# Identify the outliers (see exercise 4.2.1)
idxOutlier = which(X[,2]>20 | X[,8]>10 | X[,11]>200)
# Remove outliers from the data set
X <- X[-idxOutlier,]
y <- y[-idxOutlier,]
N = N-length(idxOutlier);

# Remove attribute 12, Quality score
X <- X[,-12]
attributeNames <- attributeNames[-12]
M = dim(X)[2];

dim(X)
#[1] 6304   11