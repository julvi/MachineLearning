library(tm)
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/ToolBox/02450Toolbox_R")
textfolder = "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern="textDocs*"), readerControl = list(language="en")))

inspect(docs)
docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)
#learning the unwanted words
mystopwords=scan("./Data/stopWords.txt", character(0))

#remove u-words
docs_nostopwords <- tm_map(docs, removeWords, mystopwords)
inspect(docs_nostopwords)
#making the matrix and removing punctuation
dtm_nostopwords <- DocumentTermMatrix(docs_nostopwords, control=list(removePunctuation=TRUE, stopwords=TRUE))
inspect(dtm_nostopwords)

control=list(stopwords=TRUE)