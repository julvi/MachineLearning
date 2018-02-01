# clear workspace
rm(list = ls(all = TRUE))
install.packages('class')
library(class)
install.packages('FNN')
library(FNN)
library(R.matlab)
# read in the data
dat <- readMat(file.path('Data', 'zipdata.mat'))

# extract the matrices testdata and traindata from dat
testdata <- dat$testdata
traindata <- dat$traindata

# the features, i.e. images of digits, are stored in the rows of traindata, except for the first column. The first column contains the class of the row, i.e. the digit identity
X = traindata[,2:dim(traindata)[2]]
y = traindata[,1] #contains the digit identity

Xtest = testdata[,2:dim(testdata)[2]]
ytest = testdata[,1]


digits_to_inspect = c(0, 1)

# find the observations that relate to the digits chosen in digits_to_inspect
#match(y,digits_to_inspect) is a vector with the same length of y. With 1 == 0 and 2 == 1 and the rest are NAs
inds = !is.na(match(y,digits_to_inspect))
indstest = !is.na(match(ytest,digits_to_inspect))

# extract the rows of X found above
#extract the rows of X that correspond to 0 or 1
X = X[inds,]
y=y[inds]
Xtest = Xtest[indstest,]
ytest=ytest[indstest]
# get the column means of X, subtract them from each row, and perform and SVD on the resulting matrix
means <- colMeans(X)
Xzeromean <- t(apply(X,1,'-',means))
Xtestzeromean <- t(apply(Xtest,1,'-',means))
svdres <- svd(Xzeromean)

# extract the matrices containing the left and right singular vectors, respectively
U=svdres$u; V=svdres$v;

K = 5 # number of principal components to use
pc_projections <- Xzeromean%*%V[,1:K]
pc_projectionstest <- Xtestzeromean%*%V[,1:K]

#perform the K-nearest neighbor classification
preds <- knn(pc_projections, pc_projectionstest, cl=y)

error_rate <- mean(preds!=ytest)
error_rate
