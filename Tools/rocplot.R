rocplot <- function(p, y){
# ROCPLOT Plots the receiver operating characteristic (ROC)  curve and
# calculates the area under the curve (AUC).
# Notice: The method assumes all values of p are distinct!
#
# Usage:
#   rocplot(p, y);
#   res = rocplot(p, y);
# 
# Input: 
#   p: Estimated probability of class 1. (Between 0 and 1.)
#   y: True class indices. (Equal to 0 or 1.)
#
# Output:
#    list containing:
#   AUC: The area under the ROC curve
#   TPR: True positive rate
#   FPR: False positive rate
#
# Author: Laura Frølich, lff@imm.dtu.dk
  
res <- sort(p, decreasing=FALSE, index.return=TRUE);
val <- res$x
ind <- res$ix
x = y[ind];
FNR = cumsum(x==1)/sum(x==1);
TPR = 1-FNR;
TNR = cumsum(x==0)/sum(x==0);
FPR = 1-TNR;
TPR = c(1, TPR);
FPR = c(1, FPR);
AUC = t(-diff(FPR)) %*% (TPR[1:(length(TPR)-1)]+TPR[2:length(TPR)])/2;

plot(c(0, 1), c(0, 1), col='black', type='l', xlab='False positive rate (1-Specificity)', ylab='True positive rate (Sensitivity)', main='Receiver operating characteristic (ROC)', yaxt='n', xaxt='n')
ticks <- seq(from=0, to=1, by=0.1)
axis(1, at=ticks)
axis(2, at=ticks)
mtext(paste('AUC =', round(AUC, digits=3)))
points(FPR, TPR, col='red')
grid(nx=length(ticks), ny=length(ticks), lwd=2)

res <- list()
res$AUC <- AUC
res$TPR <- TPR
res$FPR <- FPR
}
