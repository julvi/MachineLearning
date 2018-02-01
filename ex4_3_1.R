# exercise 4.3.1

# Digits to include in analysis (to include all, n = 1:10);
n = c(0, 1);

# Number of principal components 
K = 4;

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'zipdata.mat'))
traindata <- dat$traindata

# Extract digits
X = traindata[,2:dim(traindata)[2]]
y = traindata[,1]
classNames = c('0','1','2','3','4','5','6','7','8','9','10')
classLabels = classNames[y+1];

# Remove digits that are not to be inspected
j = !is.na(match(y,n))
X <- X[which(j),]
classLabels = classLabels[j];
classNames = classNames[n+1];
y = y[which(j)]

# Subtract the mean from the data
Y = t(apply(X,1, '-', colMeans(X)));

# Obtain the PCA solution by calculating the SVD of Y
svdres = svd(Y);
names(svdres)
U <- svdres$u
V <- svdres$v
S <- diag(svdres$d)

# Compute the projection onto the principal components
Z = U%*%S;

# Make a new data set of PC1-PCK, overwriting the old X
X = Z[,1:K];
attributeNames <- c()
for(ik in 1:K){
  attributeNames <- c(attributeNames, paste("PC", ik))
}

N <- dim(X)[1];
M <- dim(X)[2]
C = length(classNames);

# close all open figures
graphics.off()
# Make some plots
source(file.path("Scripts", "ex4_1_2.R"));
dev.new()
source(file.path("Scripts", "ex4_1_3.R"));
dev.new()
source(file.path("Scripts", "ex4_1_4.R"));
dev.new()
source(file.path("Scripts", "ex4_1_5.R"));
dev.new()
source(file.path("Scripts", "ex4_1_6.R"));
dev.new()
source(file.path("Scripts", "ex4_1_7.R"));
