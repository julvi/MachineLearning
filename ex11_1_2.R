# exercise 11.1.2
rm(list=ls())
source("setup.R")
library(mixtools)

source(file.path('Scripts', 'ex11_1_1.R'))
graphics.off()
# x-values to evaluate the KDE
x = seq(from=-10, to=10, length.out=100)

# Compute kernel density estimate
f = wkde(x=as.vector(X), u=x, bw=1)

# Plot kernel density estimate
plot(x, f, main='Kernel density estimate', type='l');
