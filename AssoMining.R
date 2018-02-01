#---------------------------------------------------------------
#Association mining
#---------------------------------------------------------------

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows

#Library and sources
source("categoric2numeric.R")
source("binarize.R")
source("writeAprioriFile.R")
source("runApriori.R")

#----------------------
#Load the data into R
#----------------------
#Set working directory
#setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
setwd("/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning/Project3")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#remove phage ID, categorical attributes and attributes related to the host
X<- dat[,-c(1,4,5,6,7,8)] 

#binarize the data - instead of using the function
med = apply(X, 2, median)
Xbin <- X > med
dim(Xbin)
Xbin <- data.frame(Xbin)

#--------------------
#One out-of-k-coding
#--------------------
#First column 4
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")


col4 = categoric2numeric(dat[,4])
Xbin[11:19]<-col4[[1]]
familynames <- col4[[2]]
colnames(Xbin)[11:19] <- familynames

#Then column 8
col7 = categoric2numeric(dat[,8])
Xbin[20:28]<-col7[[1]]
predictedHost <- col7[[2]]
colnames(Xbin)[20:28] <- predictedHost

#Remove outlier
Xbin=Xbin[-77,]
classlabels <- dat[-77,5] 


#y <- as.numeric(as.factor(classlabels))
#y <- y-1


#standardize
#Xbin <- scale(Xbin)
#dattmp<-Xbin[,11:28]*(1/sqrt(9))
#Xbin[,11:28] <- dattmp
attributeNamesC <- colnames(Xbin)
#Xbin <- data.frame(Xbin) # der er rownames med - fjern evt

#Define size
N <- 125
M <- 28
C <-8

#Save Xbin in a format that the apriori function can handle
fileApriori <- writeAprioriFile(Xbin,"dataApriori.txt")

#Run the apriori
result=runApriori("dataApriori.txt",40,80)
#change so that suport is around 40 and suport is 70 or 80.
#Before change we had 180 rules. To prune out we raise the bar.

