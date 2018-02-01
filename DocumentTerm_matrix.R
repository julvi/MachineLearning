library(tm)
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/ToolBox/02450Toolbox_R")
textfolder = "./Data/"

#takes all the textDocs* files
(docs <- Corpus(DirSource(textfolder, pattern="textDocs*"), readerControl = list(language="en")))

inspect(docs)

docs_nopunct <- tm_map(docs, removePunctuation)
inspect(docs_nopunct)

#create the matrix
dtm <- DocumentTermMatrix(docs_nopunct)

inspect(dtm)
#contains significant words