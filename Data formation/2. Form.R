#Data science specialisation Capstone
#A.J.Nicholson
#Code chunk 2: Form

#Purpose: 
#1. Create term document matrices  
#2. Combine information from all matrices
#3. Split the quad gram to give final word, previous word, previous 2 words, and previous3 words.

#############################

#Step1: load require package and functions (see SplitGram.R for function details)
setwd("filepath")
library(stringr);library(RWeka);library(tm)
source("SplitGram.R")

############################

#Step 2: load data from last code chunk
load("filepath/sample20.RData")

#############################

#Step3: create term document matrices (word,bigram,trigram and quadgram)
#increase the java limit (so in no danger of hitting and not completing code)
options(java.parameters = "-Xmx6000m")
#Tokenise and create tdms
SUStdm<-tdm(Sample)
rm(Sample)

############################

#Step 4: Turn tdms into useable data frames
SUSdf<-df_tdm(SUStdm)
rm(SUStdm)

###########################

#Step5: streamline the data, only keep sums of all three sources
SUSdf<-streamline(SUSdf)

###########################

#Step6:split quad gram
SUSdata<-splitgram(SUSdf[[4]])

###########################

#Step7: merge data into single data frame
SUSdata<-mergeDF(SUSdata,SUSdf)

############################

#Step8: write out dataframe as a csv document
write.csv(SUSdata,file="./data/data20.csv",row.names=FALSE)
rm(SUSdata)

