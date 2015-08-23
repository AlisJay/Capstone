#Data science specialisation Capstone
#A.J.Nicholson
#Predict 1: Base functions

#Function for prediction arranged over three levels:
#1= shell/ top level used in server.R (prediction() and cleantxt())
#2= model level, called by prediction() (hashtagPredict(),backwardPredict(),forwardPredict())
#3= base level, called by model functions (NextWord(),tops())

##########################################################

#Function1: Next word()
#Base level function that is used in all models, check input against data from given file 
#arguments: x= cleaned input, f=file containing reference table, g=gram level to check 
NextWord<-function(x,f,g){
    f<-f;x<-x
  #if looking at quad gram level
  if(g=="quad"){
    f<-f[f$Previous3==x$Previous3,c("word","Freq4")]#look for match,extracts word and frequency of the quadgram 
    names(f)<-c("word","freq")# change names of fields
    f<-unique(f)# remove repeats 
    f$stage<-rep(1,length(f[,1]))# new column stating level of match
  }else{
    
    #if looking at trigram level
    if(g=="tri"){
      f<-f[f$Previous2==x$Previous2,c("word","Freq3")]
      names(f)<-c("word","freq")
      f<-unique(f)
      f$stage<-rep(2,length(f[,1]))
    }else{
      
      #if looking at bi gram level
      f<-f[f$Previous1==x$Previous1,c("word","Freq2")]
      names(f)<-c("word","freq")
      f<-unique(f)
      f$stage<-rep(3,length(f[,1]))
    }
  }
  #print output
  f
}

##########################################################

#function 2:tops()
#second base level function that is used in all models 
#takes output of next word and orders to produce output (top word and next 10)
tops<-function(x){
  x<-x[order(x$stage,-x$freq),]# order by level(ascending) then frequency (descending)
  words<-as.character(unique(x$word))# find unique words (will be in order they appear in ordered x)
  topword<-words[1]# gives top word
  next10<-words[2:11]#gives the next ten
  output<-list(topword,next10)
}

