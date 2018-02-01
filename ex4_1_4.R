# exercise 4.1.4

yvals = c()
for(m in 0:(C-1))
  {
res <- boxplot(X[m==y,], plot=FALSE)
yvals = rbind(yvals, res$stats)
  }

par(mfrow=c(1,3))
for(m in 0:(C-1))
  {
boxplot(X[m==y,], main=paste("Boxplot for", classNames[m+1]), ylim=c(min(yvals), max(yvals)))
  }
