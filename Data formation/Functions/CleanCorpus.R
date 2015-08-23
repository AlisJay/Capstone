#Data science specialisation Capstone
#A.J.Nicholson
#function block 1: Functions for cleaning

#see 1.load and clean for implementation

####################

#Function1: cleancorpus()  
#To clean raw data
cleancorpus<-function(c){
  #replace & with and
  And<- function(x){gsub("&"," and ",x)}
  c<-tm_map(c,content_transformer(And))
  
  # replace non standard apostrophes
  y<-grep("My friend makes one LARGE pitcher",US[[1]][1][[1]],value=TRUE)
  y<-strsplit(y,split=" ");y<-y[[1]][72]
  y<-gsub("don","",y);y<-gsub("t","",y)
  apos<-function(x){gsub(y,"'", x)}
  c<-tm_map(c,content_transformer(apos))
  
  #Replace double apostrophes ('') with speach marks (")
  DApos<-function(x){gsub("''",".",x)}
  c<-tm_map(c,content_transformer(DApos))
  
  #Remove all non alphanumeric except hashtags and apostrophes 
  HashApos<-function(x){gsub("[^[:alnum:][:space:]#']", " ", x)}
  c<-tm_map(c,content_transformer(HashApos))
  
  #Remove all numbers
  c<- tm_map(c, removeNumbers)
  
  #Remove erronious apostrophes
  #i.e those at start of words or on there own
  Eapos<-function(x){gsub(" '|^'| ' "," ",x)}
  c<-tm_map(c,content_transformer(Eapos))
  
  #Remove hastags on there own 
  hash<-function(x){gsub(" # "," ",x)}
  c<-tm_map(c,content_transformer(hash))
  
  #Remove excess spaces
  c<-tm_map(c,stripWhitespace)
  
  #Turn all to lower case letters 
  c<-tm_map(c,content_transformer(tolower))
  
  c
}



