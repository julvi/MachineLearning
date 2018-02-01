#---------------------------------------------------------------
#Association mining
#---------------------------------------------------------------

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows

#Library and sources
# Load packages
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/ToolBox/02450Toolbox_R")
source("setup.R")
source("Tools/categoric2numeric.R")
source("Tools/binarize.R")

source("~/Desktop/IntroductionToMachineLearning/Report3/Tools/Apriori/writeAprioriFile.R")
source("~/Desktop/IntroductionToMachineLearning/Report3/Tools/Apriori/runApriori.R")

#----------------------
#Load the data into R
#----------------------
#Set working directory

setwd("~/Desktop/IntroductionToMachineLearning/Report3")
#setwd("/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning/Project3")

#Read data in
dat <- read.table("~/Desktop/IntroductionToMachineLearning/phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#remove phage ID, and categorical attributes
X<- dat[,-c(1, 4, 5, 8)] 

#binarize the data - instead of using the function
med = apply(X, 2, median)
Xbin <- X > med
dim(Xbin)
Xbin <- data.frame(Xbin)
dim(Xbin)
#--------------------
#One out-of-k-coding
#--------------------
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")

#First column 4 (phage family x9)
col4 = categoric2numeric(dat[,4])
Xbin[13:21]<-col4[[1]]
familynames <- col4[[2]]
colnames(Xbin)[13:21] <- familynames

#Then column 5 (annotated host x8)
col5 = categoric2numeric(dat[,5])
Xbin[22:29]<-col5[[1]]
annotatedHost <- col5[[2]]
colnames(Xbin)[22:29] <- annotatedHost

#Then column 8 (predicted host x9)
col8 = categoric2numeric(dat[,8])
Xbin[30:38]<-col8[[1]]
predictedHost <- col8[[2]]
colnames(Xbin)[30:38] <- predictedHost

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
dim(Xbin)
#Define size
N <- 125
M <- 38
C <-8

#Save Xbin in a format that the apriori function can handle
fileApriori <- writeAprioriFile(Xbin,"dataApriori.txt")

#Run the apriori
result=runApriori("dataApriori.txt",40,80)
#change so that suport is around 40 and suport is 70 or 80.
#Before change we had 180 rules. To prune out we raise the bar.

