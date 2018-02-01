# exercise 11.1.3
rm(list=ls())
source("setup.R")
library(mixtools)

source(file.path('Scripts', 'ex11_1_1.R'))
graphics.off()

# Number of components
K = 3;

# x-values to evaluate the GMM
x = seq(from=-10, to=10, length.out=100)

# Fit Gaussian mixture model
model <- normalmixEM(x=X, k=K)

res <- gmmposterior(model, matrix(x, ncol=1))

# Plot GMM estimate
plot(x, exp(res$logpdf), main='Gaussian mixture model', type='l');
