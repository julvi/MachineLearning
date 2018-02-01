# exercise 4.2.1
# clear the workspace
rm(list=ls())
# Load the data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine.mat'))
# view content of the Matlab data structure wine.mat
names(dat)
# extract variables
X <- dat$X
y <- dat$y
N <- dat$N
M <- dat$M
C <- dat$C
classNames <- dat$classNames
attributeNames <- dat$attributeNames
# assign attribute names as column names of the data matrix X
colnames(X) <- unlist(attributeNames)
# We start with a box plot of each attribute
par(mar=c(10,4,4,2)+0.1)
boxplot(X, main="Wine: Boxplot", las=2)

# From this it is clear that there are some outliers in the Alcohol
# attribute (10x10^14 is clearly not a proper value for alcohol content)
# However, it is impossible to see the distribution of the data, because
# the axis is dominated by these extreme outliers. To avoid this, we plot a
# box plot of standardized data (using the zscore function).

source("standardize.R")
par(mar=c(10,4,4,2)+0.1)
boxplot(standardize(X), main="Wine: Boxplot", las=2)


# This plot reveals that there are clearly some outliers in the Volatile
# acidity, Density, and Alcohol attributes, i.e. attribute number 2, 8,
# and 11. 

# Next, we plot histograms of all attributes.
par(mar=c(1,1,1,1))
yvals <- c()
for(m in 1:M)
{
	res <- hist(X[,m], plot=FALSE);
        yvals = c(yvals, res$counts)
}

par(mfrow=c(3,4))
for(m in 1:M)
{
	hist(X[,m], main=unlist(attributeNames[m]), ylim=c(min(yvals), max(yvals)));
}

# This confirms our belief about outliers in attributes 2, 8, and 11.
# To take a closer look at this, we next plot histograms of the 
# attributes we suspect contains outliers

m = c(2, 8, 11);
yvals <- c()
for(i in 1:3)
{
   res <- hist(X[,m[i]],breaks=51, plot=FALSE);
   yvals <- c(yvals, res$counts)
}

par(mfrow=c(1,3))
for(i in 1:3)
{
   hist(X[,m[i]],breaks=51, main=unlist(attributeNames[m[i]]), ylim=c(min(yvals), max(yvals)))
}

# The histograms show that there are a few very extreme values in these
# three attributes. To identify these values as outliers, we must use our
# knowledge about the data set and the attributes. Say we expect volatide
# acidity to be around 0-2 g/dm^3, density to be close to 1 g/cm^3, and
# alcohol percentage to be somewhere between 5-20 % vol. Then we can safely
# identify the following outliers, which are a factor of 10 greater than
# the largest we expect.

idxOutlier = X[,2]>20 | X[,8]>10 | X[,11]>200

# Finally we will remove these from the data set
X = X[-which(idxOutlier),]
y = y[-which(idxOutlier)]
N = N-sum(idxOutlier);

# Now, we can repeat the process to see if there are any more outliers
# present in the data. We take a look at a histogram of all attributes:


yvals <- c()
for(m in 1:M)
{
	res <- hist(X[,m], plot=FALSE);
        yvals <- c(yvals, res$counts)
}

par(mfrow=c(3,4))
for(m in 1:M)
{
	hist(X[,m], main=unlist(attributeNames[m]), ylim=c(min(yvals), max(yvals)));
}


# This reveals no further outliers, and we conclude that all outliers have
# been detected and removed.
