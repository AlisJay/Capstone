#Data science specialisation Capstone
#A.J.Nicholson
#Code chunk 1: Load and clean 

#Purpose: 
#1. load and clean raw data 
#2. randomly split clean data into 20 even samples
#(due to computational limitations in next stages)

#####################

#Step1: load required packages and source functions (see CleanCorpus.R for function details)
setwd("filepath")
library(tm);library(stringr);library(RWeka)
source("CleanCorpus.R")

#####################

#Step2: Download and unzip data
url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(url,destfile="capdata.zip")
unzip("capdata.zip")

#####################

#Step3: Read in data 
UStxt<-"./final/en_US"
USsource<-DirSource(directory=UStxt,encoding="UTF-8",mode="text")
US<-Corpus(USsource,
           readerControl=list(reader=readPlain,language="en_US",load=TRUE))

#####################
		   
# Step4: Clean data set
USclean<-cleancorpus(US)

######################

# Step5: Create random splits for data 
#create random number sequence for each source (seed set for each to make reproducible)
set.seed(125)
nBlog<-sample(1:899288,899288,replace=FALSE)#n.b 899288 = number of line in blog text document
set.seed(200)
nNews<-sample(1:77259,77259,replace=FALSE)
set.seed(965)
nTwitter<-sample(1:2360148,2360148,replace=FALSE)

#split each sequence into 20 even parts 
nBlog<-split(nBlog,1:20)
nNews<-split(nNews,1:20)
nTwitter<-split(nTwitter,1:20)
#n.b non of the sequences are exactly divisible by 20 so there will be some repetition

#######################

#Step6:extract Sample
#n.b currently set to take 20th sample
USBlogSample<-USclean[[1]][1][[1]][nBlog[[20]]]
USNewsSample<-USclean[[2]][1][[1]][nNews[[20]]]
USTwitterSample<-USclean[[3]][1][[1]][nTwitter[[20]]]

#combine samples into a single corpus
Sample<-c(PlainTextDocument(USBlogSample),PlainTextDocument(USNewsSample),PlainTextDocument(USTwitterSample))
#Adding ids to meta data
meta(Sample[[3]])$id<-"twitter";meta(Sample[[1]])$id<-"blog";meta(Sample[[2]])$id<-"news"
#remove samples
rm(USBlogSample,USNewsSample,USTwitterSample)

#######################

#Step 7: save progress
save.image("filepath/sample20.RData")




