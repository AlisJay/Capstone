#Data science specialisation Capstone
#A.J.Nicholson
#function block 3: Merge data

#for implementation see 3.Merge

##########################################################

#Function1: readin
#reads in the sample and selects words that start with speficed letters
readin<-function(f,a){
  d<-read.csv(file[x[[f]]])
  d<-d[grep(alpha[a],d$word),]
  row.names(d)<-NULL
  d
}

##########################################################

#Function2: merge data
# Merges two datasets together by quad-gram, combining the frequencies
mergedata<-function(A,B){
  #Step 1: simple merge
  #select part of B for merge (to avoid repeated fields)
  fields<-c("quad","Freq1","Freq2","Freq3","Freq4")
  B2<-B[,fields]
  
  #merge data
  m<-merge(A,B2, by="quad")
  
  #combine the frequencies
  m$Freq1<-m$Freq1.x+m$Freq1.y
  m$Freq1.x<-NULL;m$Freq1.y<-NULL
  m$Freq2<-m$Freq2.x+m$Freq2.y
  m$Freq2.x<-NULL;m$Freq2.y<-NULL
  m$Freq3<-m$Freq3.x+m$Freq3.y
  m$Freq3.x<-NULL;m$Freq3.y<-NULL
  m$Freq4<-m$Freq4.x+m$Freq4.y
  m$Freq4.x<-NULL;m$Freq4.y<-NULL
  
  #Step2: find data that wasn't combined (that which was only in one dataset)
  quad<-m$quad
  
  B2<-B[!(B$quad %in% quad),]
  B2$tri<-paste(B2$Previous2,B2$word);B2$bi<-paste(B2$Previous1,B2$word)
  # bi and tri fields created for later merging
  
  A2<-A[!(A$quad %in% quad),]
  A2$tri<-paste(A2$Previous2,A2$word);A2$bi<-paste(A2$Previous1,A2$word)
  
  #Step3:Find right word,bigram and trigram frequencies for data from step2
  #freq1,word
  Freq1<-data.frame(word=m$word,Freq1=m$Freq1)#frequencies from combined data
  wordA<-A2[!(A2$word %in% Freq1$word),c("word","Freq1")]#frequencies from A not in m
  wordB<-B2[!(B2$word %in% Freq1$word),c("word","Freq1")]#frequencies from B not in m
  
  wordM<-merge(wordA,wordB,by="word")#merge wordA and wordB
  wordM$Freq1<-wordM$Freq1.x+wordM$Freq1.y#combine frequencies
  wordM$Freq1.x<-NULL;wordM$Freq1.y<-NULL#remove extraneous fields
  wordA<-wordA[!(wordA$word %in% wordM$word),]#wordA not in wordm
  wordB<-wordB[!(wordB$word %in% wordM$word),]#wordB not in wordm
 
  Freq1<-rbind(Freq1,wordA,wordB,wordM)#combine freqencies
  rm(wordA,wordB,wordM)#remove unnecessary objects
  Freq1<-unique(Freq1)#only one entry per word
  
  B2$Freq1<-NULL#remove original Freq1 from B2
  B2<-merge(B2,Freq1,by="word")# merge by word to give new Freq1
  A2$Freq1<-NULL#remove original Freq1 from A2
  A2<-merge(A2,Freq1,by="word")# merge by word to give new Freq1
  rm(Freq1)# remove unecessary object
  
  #freq2,bigram
  Freq2<-data.frame(bi=paste(m$Previous1,m$word),Freq2=m$Freq2)
  biA<-A2[!(A2$bi %in% Freq2$bi),c("bi","Freq2")];biB<-B2[!(B2$bi %in% Freq2$bi),c("bi","Freq2")]
  biM<-merge(biB,biA,by="bi")
  biM$Freq2<-biM$Freq2.x+biM$Freq2.y
  biM$Freq2.x<-NULL;biM$Freq2.y<-NULL
  biA<-biA[!(biA$bi %in% biM$bi),];biB<-biB[!(biB$bi %in% biM$bi),]
  Freq2<-rbind(Freq2,biA,biB,biM)
  rm(biA,biB,biM)
  Freq2<-unique(Freq2)
  B2$Freq2<-NULL;A2$Freq2<-NULL
  B2<-merge(B2,Freq2,by="bi");A2<-merge(A2,Freq2,by="bi")
  rm(Freq2)
  
  #freq3,trigram
  Freq3<-data.frame(tri=paste(m$Previous2,m$word),Freq3=m$Freq3)
  triA<-A2[!(A2$tri %in% Freq3$tri),c("tri","Freq3")];triB<-B2[!(B2$tri %in% Freq3$tri),c("tri","Freq3")]
  triM<-merge(triA,triB,by="tri")
  triM$Freq3<-triM$Freq3.x+triM$Freq3.y
  triM$Freq3.x<-NULL;triM$Freq3.y<-NULL
  triA<-triA[!(triA$tri %in% triM$tri),];triB<-triB[!(triB$tri %in% triM$tri),]
  Freq3<-rbind(Freq3,triA,triB,triM)
  rm(triA,triB,triM)
  Freq3<-unique(Freq3)
  B2$Freq3<-NULL;A2$Freq3<-NULL
  B2<-merge(B2,Freq3,by="tri");A2<-merge(A2,Freq3,by="tri")
  rm(Freq3)
  
  #remove bi and tri field
  A2$bi<-NULL;A2$tri<-NULL
  B2$bi<-NULL;B2$tri<-NULL
  
  #step 4: combine all data
  m<-rbind(m,A2,B2)
  rm(A2,B2)
  
  #print output
  m
}