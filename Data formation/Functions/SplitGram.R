#Data science specialisation Capstone
#A.J.Nicholson
#function block 2: Split gram

#See 2.Form for implementation

##########################################################

#Function 1: tdm()
#Create term document matrices (word, bigrams,trigrams and quadgrams)

tdm<-function(c){
  
  #tokenizer functions
  Tok<-function(x){NGramTokenizer(x,Weka_control(min = 1, max = 1,delimiters=' \r\n\t'))}
  Tokbi<-function(x){NGramTokenizer(x,Weka_control(min = 2, max = 2,delimiters=' \r\n\t'))}
  Toktri<-function(x){NGramTokenizer(x,Weka_control(min = 3, max = 3,delimiters=' \r\n\t'))}
  Tokquad<-function(x){NGramTokenizer(x,Weka_control(min=4,max=4,delimiters=' \r\n\t'))}
  
  #tdm creation
  tdm<-TermDocumentMatrix(c,control=list(tokenize=Tok))
  tdmbi<-TermDocumentMatrix(c,control=list(tokenize=Tokbi))
  tdmtri<-TermDocumentMatrix(c,control=list(tokenize=Toktri))
  tdmquad<-TermDocumentMatrix(c,control=list(tokenize=Tokquad))
  
  #output
  list(tdm,tdmbi,tdmtri,tdmquad)
}

##########################################################

#Function 2: tdmtransform()
#Transforms a term document matrix into dataframe (use inside df_tdm())
#(see function 3)
tdmtransform<-function(t){
  #create matrix then turn into dataframe
  t<-as.matrix(t)
  t<-data.frame(t)
  #creates sum field
  t$all<-rowSums(t)
  #takes row names and turns them into a proper column
  t$phrase<-row.names(t)
  row.names(t)<-NULL
  t
}

##########################################################

#Function3: df_tdm()
#applies tdmtransform() to all of the listed output of tdm()
df_tdm<-function(t){
  #word
  tdm<-t[[1]]
  tdm<-tdmtransform(tdm)
  #bigram
  tdmbi<-t[[2]]
  tdmbi<-tdmtransform(tdmbi)
  #trigram
  tdmtri<-t[[3]]
  tdmtri<-tdmtransform(tdmtri)
  #quadgram
  tdmquad<-t[[4]]
  tdmquad<-tdmtransform(tdmquad)
  #output
  list(tdm,tdmbi,tdmtri,tdmquad)
}

##########################################################

#Function 4:streamline()
#creates common names for fields and removes non-sum columns
streamline<-function(d){
  
  #Split list input for ease of reference
  tdm<-d[[1]];tdmbi<-d[[2]];tdmtri<-d[[3]];tdmquad<-d[[4]]
  
  #Change names 
  names(tdm)<-c("Freq1B","Freq1N","Freq1T","Freq1","word")
  names(tdmbi)<-c("Freq2B","Freq2N","Freq2T","Freq2","bi")
  names(tdmtri)<-c("Freq3B","Freq3N","Freq3T","Freq3","tri")
  names(tdmquad)<-c("Freq4B","Freq4N","Freq4T","Freq4","quad")
  
  #Remove individual source frequencies
  tdm$Freq1B<-NULL;tdm$Freq1N<-NULL;tdm$Freq1T<-NULL
  tdmbi$Freq2B<-NULL;tdmbi$Freq2N<-NULL;tdmbi$Freq2T<-NULL
  tdmtri$Freq3B<-NULL;tdmtri$Freq3N<-NULL;tdmtri$Freq3T<-NULL
  tdmquad$Freq4B<-NULL;tdmquad$Freq4N<-NULL;tdmquad$Freq4T<-NULL
  
  #re-list for output
  output<-list(tdm,tdmbi,tdmtri,tdmquad)
  output
  
}

##########################################################

#Function 5: splitgram()
#Takes quad gram and produces word, previous1,previous2, previous3, bi and tri
#e.g "once apon a time":
#word="time",Previous1="a",Previous2="apon a",Previous3="once apon a"
#bi="a time", tri="apon a time"

splitgram<-function(d){
  #create trigram, bigram and word
  tri<-word(d$quad,-3,-1)
  bi<-word(tri,-2,-1)
  word<-word(bi,-1)
  
  #create previous 3, 2 and 1
  Previous3<-word(d$quad,1,3)
  Previous2<-word(Previous3,-2,-1)
  Previous1<-word(Previous2,-1)
  
  #create dataframe from created vectors
  d2<-data.frame(Previous1,Previous2,Previous3,bi,tri,word)
  rm(Previous1,Previous2,Previous3,bi,tri,word)
    
  #combine new dataframe with the old
  d<-cbind(d,d2)
  d
}

##########################################################

#Function 6: mergeDF()
#merges the list of dataframe and the output of splitgram into single useable dataframe
  
mergeDF<-function(a,b){
  #merge with single word dataframe
  a<-merge(a,b[[1]],by="word")
  #merge with bigram dataframe
  a<-merge(a,b[[2]],by="bi")
  #merge with trigram dataframe
  a<-merge(a,b[[3]],by="tri")
  #remove merge fields
  a$tri<-NULL;a$bi<-NULL
  #print output
  a
}