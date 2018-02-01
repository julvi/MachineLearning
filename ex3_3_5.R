# exercise 3.3.5
# load the package "gplots", which contains the function hist2d for making 2-dimensional histograms. If the package is not already installed on your computer, an error will result from the function call library(gplots). In that case, install the package using install.packages("gplots") and then run library(gplots) again. Same for the package MASS.
install.packages('gplots')
library(gplots)
library(MASS)
# Number of samples
N = 1000; 

# Mean
mu = c(13, 17);

# Standard deviation of x1
s1 = 2;

# Standard deviation of x2
s2 = 3;

# Correlation between x1 and x2
corr = 0.5;

# Covariance matrix
S = matrix(c(s1^2, corr*s1*s2, corr*s1*s2, s2^2), nrow=2, byrow=TRUE);

# Number of bins in histogram
NBins = 20;

# Generate samples from the Normal distribution
X = mvrnorm(N, mu=mu, Sigma=S);

# Plot scatter plot of data

xrange = mu[1]+S[1,1]*c(-3, 3);
yrange = mu[2]+S[2,2]*c(-3, 3);

par(mfrow=c(1,2))
plot(xrange, yrange, type="n", ylab="x2", xlab="x1", main="Scatter plot of data")
points(X[,1], X[,2]);
hist2d(x=X, nbins=NBins, col=gray(32:0/32), main="2-D Normal distribution", xlab="x1", ylab="x2"); # lighter colors correspond to lower values, i.e. lower counts of observations in bins. This may generate a warning,
#" Warning messages:
#1: In seq.default(from = min(x), to = max(x), length = nbins[1] + 1,  :
#  extra argument(s) ‘labels’ will be disregarded
#2: In seq.default(from = min(y), to = max(y), length = nbins[2] + 1,  :
#  extra argument(s) ‘labels’ will be disregarded".
# Just ignore this warning. The warning can appear even with the simple call hist2d(X).

