# exercise 4.1.5

observationColors <- c("blue", "green3", "red")[unclass(y+1)]
pairs(X, bg=observationColors, pch=21)
par(xpd=TRUE)
legend(0, 1, classNames, fill=unique(observationColors))



