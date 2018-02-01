# exercise 4.1.7


Xstandardized <- standardize(X)
# sums of columns are now practically zero (difference from zero is due to machine error), and variance of columns are now one.
apply(Xstandardized,2, sum)
apply(Xstandardized,2,sd) 

image(t(Xstandardized[N:1,]), col=gray(0:32/32), xaxt="n", yaxt="n", ylab="Observation number", xlab="Attribute")
axis(1, at=seq(from=0, to=1, length.out=4),labels=attributeNames)
axis(2, at=seq(from=0, to=1, length.out=N), labels=1:N)

