####################################################################################
#Hierarchical clustering
####################################################################################
#Use script 9_3_2


#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
#setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')

source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")
source("clusterplot.R")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)




#names(dat)
#[1] "Phage_ID"                 "phage_genome_size"        "phage_Gccontent"          "phage_family"            
#[5] "annotated_host"           "host_genome_size"         "host_GCcontent"           "predicted_host"          
#[9] "frac_d"                   "frac_q"                   "Score"                    "Expected"                
#[13] "z"                        "coverage"                 "unique_kmers_in_template" "unique_kmers_in_query" 
X <- dat[,-c(1,4,5,6,7,8)] 

#--------------------
#One out-of-k-coding
#--------------------
#First column 4
col4 = categoric2numeric(dat[,4])
X[11:19]<-col4[[1]]
familynames <- col4[[2]]
colnames(X)[11:19] <- familynames

#Then column 8
col7 = categoric2numeric(dat[,8])
X[20:28]<-col7[[1]]
predictedHost <- col7[[2]]
colnames(X)[20:28] <- predictedHost

X <- scale(X)
dattmp<-X[,11:28]*(1/sqrt(9)) #If including info about the host then 12:29
X[,11:28] <- dattmp

#Remove outlier
idxOutlier = X[,6]>4
#dat[77,]
#Phage_ID phage_genome_size phage_Gccontent phage_family          annotated_host host_genome_size
#77 KC691257.1            154872        64.68309   Myoviridae Mycobacterium_smegmatis          6813778
#host_GCcontent          predicted_host frac_d frac_q Score Expected       z coverage unique_kmers_in_template
#77       66.62636 Mycobacterium_smegmatis   0.42   0.43 65707 2975.492 239.366     0.85                   155290
#unique_kmers_in_query
#77                154409
classlabels <- dat[-77,5] 
# Finally we will remove these from the data set
X = X[-which(idxOutlier),]
X <- data.frame(X)

y <- as.numeric(as.factor(classlabels))
y <- y-1
y = y[-which(idxOutlier)]
attributeNamesC <- colnames(X)


#Define size
N <- 125
M <- 28
C <-8

#Analyze your data by hierarchical clustering and try interpret the generated dendrogram.
#Use the cluster validity measures to evaluate how well the clusters reflect your 
#labeled information at one of the levels of the dendrogram.

## Hierarchical clustering
# Maximum number of clusters
Maxclust = 2;

# Compute hierarchical clustering
hc <- hclust(dist(X), method="single")
#hc <- hclust(dist(Xdf), method="ward")
# Compute clustering by thresholding the dendrogram
i <- cutree(hc, k = Maxclust)

## Plot results

# Plot dendrogram
#plclust(hc)
plot(hc)

# Plot data
dev.new()
source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/clusterplot.R")

clusterplot(X, classlabels, i, main='Hierarchical')

