# exercise 3.3.3

# Number of samples
N = 100000; 

# Mean
mu = 17;       

# Standard deviation
s = 2;  

# Number of bins in histogram
NBins = 50;

# Generate samples from the Normal distribution
X = rnorm(N, mean=mu, sd=s);

# Plot a histogram
graphics.off() # close all open figures
res = hist(X, breaks=NBins, freq=FALSE);
#middle break
x = res$mids
x = seq(from=min(x), to=max(x), length.out=1000);
lines(x, dnorm(x, mean=mu, sd=s));


# Compute empirical mean and standard deviation
mu_ = mean(X);
s_ = sd(X);

mu_
s_
