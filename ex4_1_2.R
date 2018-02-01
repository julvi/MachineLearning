# exercise 4.1.2
source('setup.R')
yvals = c()
#for each attribute make a plot
for(m in 1:M)
  {
	res <- hist(X[,m]);
        yvals = c(yvals, res$counts)
  }

# the argument ylim ensures that all histograms are plotted on the same y-axis
par(mfrow=c(2,2))
for(m in 1:M)
  {
	hist(X[,m], xlab=attributeNames[m], main="Histogram of attribute values", ylim=c(min(yvals), max(yvals)));
  }
