library(tm)
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/ToolBox/02450Toolbox_R")
textfolder = "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern="textDocs*"), readerControl = list(language="en")))

inspect(docs)

docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)

mystopwords=scan("./Data/stopWords.txt", character(0))

docs_nostopwords <- tm_map(docs_nopunct, removeWords, mystopwords)
dtm_nostopwords <- DocumentTermMatrix(docs_nostopwords, control=list(stopwords=TRUE))
inspect(dtm_nostopwords)

docs_stemmed <- tm_map(docs_nostopwords, stemDocument, language="english")
inspect(docs_stemmed)
dtm_stemmed <- DocumentTermMatrix(docs_stemmed, control=list(stopwords=TRUE))
inspect(dtm_stemmed)

#install.packages("sos")
library(sos)
#findFn is a function of sos package that allows for looking for packages that have a wanted function
#findFn("cosine", maxPages=2, sortby="MaxScore") # to check whether a certain function exists in some R package, and if so, find out which package.
#install.packages("lsa")
library(Snowball)
library(RWeka)
library(lsa) # the above function shows that a function for calculating the cosine measure exists in the package lsa.

#get the dimensions of the matrix
size <- dim(dtm_stemmed)
cosmeas <- c()
dtm_stemmed
q <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0)
for(irow in 1:size[1])
{
doc <- dtm_stemmed[irow,]
cosmeas[irow] <- cosine(inspect(dtm_stemmed)[irow,],q)
}