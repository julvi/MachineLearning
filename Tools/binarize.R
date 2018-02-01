binarize <- function(X, Y)
{
  # Construct binary versions of the matrices X and Y
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  if(dim(X)[2]==1)
  {
   X = t(X)
  }

  if(dim(Y)[2]==1)
  {
   Y = t(Y)
  }

N1 = dim(X)[1]; M = dim(X)[2];
N2 = dim(Y)[1]; M = dim(Y)[2];




'Attributes non-binary: Forcing representation to be binary.'
med = apply(rbind(X,Y), 2, median)
X <- X > med
medmat <- matrix(rep(med, times=N2), byrow=TRUE, nrow=N2)
Y <- Y>medmat

binarized <- list()
binarized$X <- X
binarized$Y <- Y

binarized
}
