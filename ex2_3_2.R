####################
# exercise 2.3.2
####################
# choose which digits to consider in the analysis
digits_to_inspect = c(0, 1)

# find the observations that relate to the digits chosen in digits_to_inspect
inds = !is.na(match(y,digits_to_inspect))

# extract the rows of X found above
X = X[inds,]
y=y[inds]

# get the column means of X, subtract them from each row, and perform and SVD on the resulting matrix
means <- colMeans(X)
Xzeromean <- t(apply(X,1,'-',means))
svdres <- svd(Xzeromean)

# extract the matrices containing the left and right singular vectors, respectively
U=svdres$u; V=svdres$v;

########################
# calculate and plot the variance explained by the PCs
########################
pcvariance <- svdres$d^2/sum(svdres$d^2)
plot(cumsum(pcvariance), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained")
# insert a vertical line into the plot at 22 to check that the 22 first PCs explain 90% of the variance in data
abline(v=22)

########################
# Plot principal components 1 and 2 against each other in a scatterplot,
# i.e. plot the projections of observations onto PCs 1 and 2
########################

pc_projections <- Xzeromean%*%V # these two ways of calculating pc_projections are equivalent
pc_projections <- U%*%diag(svdres$d)
pcproj1 <- pc_projections[,1]
pcproj2 <- pc_projections[,2]

plot(c(min(pcproj1), max(pcproj1)), c(min(pcproj2), max(pcproj2)), type="n", xlab="PC 1", ylab="PC 2")
points(pcproj1[y==0], pcproj2[y==0], col="red")
points(pcproj1[y==1], pcproj2[y==1], col="green")
legend("topleft", legend=c("0", "1"), fill=c("red", "green"))


####################
# Reconstruction of images of digits
####################

# number of PCs to include in reconstruction of digits
K = 5
# digits to visualize
nD = 1:5

reconstructions <- pc_projections[,1:K]%*%t(V[,1:K])

layout(matrix(c(1:length(nD), (length(nD)+1):(2*length(nD))), 2, length(nD), byrow = FALSE))
for(d in 1:length(nD)){
  origImage <- X[nD[d],]
  dim(origImage) <- c(16,16)
  image(origImage[,ncol(origImage):1], main="Original", col=gray((32:0)/32))
  reconstructedImage <- reconstructions[nD[d],]+means
  dim(reconstructedImage) <- c(16,16)
  image(reconstructedImage[,ncol(reconstructedImage):1], main="Reconstruction", col=gray((32:0)/32))
}
par('mar')
par(mar = c(4, 2, 2, 1) )
###################
# Visualize PCs
###################

# visualize the first 12 PCs
par(mfrow=c(3,4))
for(nth_pc in 1:12){
  W <- U[,nth_pc]%*%t(V[,nth_pc])
  pc <- W[nth_pc,]
  dim(pc) <- c(16,16)
  # view image of PC
  image(pc[,ncol(pc):1], col=gray((32:0)/32), main=paste("PC ", nth_pc))
}
