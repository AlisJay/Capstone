#Data science specialisation Capstone
#A.J.Nicholson
#Predict 2: Model functions 

#Function for prediction arranged over three levels:
#1= shell/ top level used in server.R (prediction() and cleantxt())
#2= model level, called by prediction() (hashtagPredict(),backwardPredict(),forwardPredict())
#3= base level, called by model functions (NextWord(),tops())

##########################################################

#Function 1 :forwardPredict
#Standard prediction model that words through most common words, runs most common to least common
#arguments: x=cleaned input
forwardPredict<-function(x){
  two<-read.csv("two.csv",stringsAsFactors=FALSE)#load in first dataset, miss one.csv as only has words and and the 
  
  A<-NextWord(x,two,"quad")# match quadgrams in the first dataset (and,the)
  
  if(length(unique(A$word))>10){output<-tops(A);rm(two,A)}# if enough matches then stop cycle and calculate output
  
  else{#if not read in next dataset
    three<-read.csv("three.csv",stringsAsFactors=FALSE)
    B<-NextWord(x,three,"quad")
    A<-rbind(A,B);rm(B)#join output
    
    if(length(unique(A$word))>10){output<-tops(A);rm(two,three,A)}
    
    else{
      four<-read.csv("four.csv",stringsAsFactors=FALSE)
      B<-NextWord(x,four,"quad")
      A<-rbind(A,B);rm(B)
      
      if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,A)}
      
      else{
        five<-read.csv("five.csv",stringsAsFactors=FALSE)#last dataset read in
        B<-NextWord(x,four,"quad")
        A<-rbind(A,B);rm(B)
        
        if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
        
        
        else{# if get to here then move on to tri grams matching  
          B<-NextWord(x,two,"tri")
          A<-rbind(A,B);rm(B)
          
          if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)} 
          
          else{
            B<-NextWord(x,three,"tri")
            A<-rbind(A,B);rm(B)
            
            if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
            
            else{
              B<-NextWord(x,four,"tri")
              A<-rbind(A,B);rm(B)
              
              if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
              
              else{
                B<-NextWord(x,five,"tri")
                A<-rbind(A,B);rm(B)
                
                if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
                
                else{#if get to this point then move on to bi grams
                  B<-NextWord(x,two,"bi")
                  A<-rbind(A,B);rm(B)
                  
                  if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
                  
                  else{
                    B<-NextWord(x,three,"bi")
                    A<-rbind(A,B);rm(B)
                    
                    if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
                    
                    else{
                      B<-NextWord(x,four,"bi")
                      A<-rbind(A,B);rm(B)
                      
                      if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
                      
                      else{#last check
                        B<-NextWord(x,five,"bi")
                        B<-rbind(A,B);rm(B)
                        
                        if(length(unique(A$word))>10){output<-tops(A);rm(two,three,four,five,A)}
                        
                      else{#if still can't find enough matches add top eleven most frequent words to end of A
                        B<-data.frame(word=c("the","and","for","that","with","this","was","are","not","all","just"),freq=c(11,10,9,8,7,6,5,4,3,2,1),stage=rep(4,11))
                        A<-rbind(A,B);rm(B)
                        output<-tops(A)
                        
                      }}}}}}}}}}}}#close all else brackets
  output #print output
}


    
##########################################################

#function 2: backwardPredict() original version
#backward model that checks the least common words in inverse order of word frequency
#arguments: x=cleaned input
backwardPredict<-function(x){
  eight<-read.csv("eight.csv",stringsAsFactors=FALSE)#read first dataset
  A<-NextWord(x,eight,"quad")#check for quadgram matches
  
  if(length(unique(A$word))>10){output<-tops(A);rm(eight,A)}
  
  else{
    seven<-read.csv("seven.csv",stringsAsFactors=FALSE)
    B<-NextWord(x,seven,"quad")
    A<-rbind(A,B);rm(B)
    
    if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,A)}
    
    else{#last read in 
      six<-read.csv("six.csv",stringsAsFactors=FALSE)
      B<-NextWord(x,six,"quad")
      A<-rbind(A,B);rm(B)
      
      if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
      
      else{#if get to here switch to tri grams
        B<-NextWord(x,eight,"tri")
        A<-rbind(A,B);rm(B)
        
        if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
        
        else{
          B<-NextWord(x,seven,"tri")
          A<-rbind(A,B);rm(B)
          
          if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
          
          else{
            B<-NextWord(x,six,"tri")
            A<-rbind(A,B);rm(B)
            
            if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
            
            else{#if get to here switch to bi grams
              B<-NextWord(x,eight,"bi")
              A<-rbind(A,B);rm(B)
              
              if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
              
              else{
                B<-NextWord(x,seven,"bi")
                A<-rbind(A,B);rm(B)
                
                if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
                
                else{#last check
                  B<-NextWord(x,six,"bi")
                  A<-rbind(A,B);rm(B)
                  
                  if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
                  
                  else{#if still not enough matches then add most common 11 words to the end of A$word
                    B<-data.frame(word=c("clean","tickets","six","conference","per","present","plans","anymore","goal","shop","sort"),freq=c(11,10,9,8,7,6,5,4,3,2,1),stage=rep(4,11))                   
                    A<-rbind(A,B);rm(B)
                    output<-tops(A);rm(eight,seven,six,A)
                    
                  }}}}}}}}}#close else
  output #print output
}

##########################################################

#function 3:backwardPredict() short version
#version that was ultimately used in app

backwardPredict<-function(x){
  eight<-read.csv("eight.csv",stringsAsFactors=FALSE)#read first dataset
  A<-NextWord(x,eight,"quad")#check for quadgram matches
  rm(eight)#nolonger need
  
  if(length(unique(A$word))>10){output<-tops(A);rm(eight,A)}
  
  else{
    seven<-read.csv("seven.csv",stringsAsFactors=FALSE)
    B<-NextWord(x,seven,"quad")
    A<-rbind(A,B);rm(B,seven)
    
    if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,A)}
    
    else{#last read in 
      six<-read.csv("six.csv",stringsAsFactors=FALSE)
      B<-NextWord(x,six,"quad")
      A<-rbind(A,B);rm(B)
      
      if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
      
      else{#if get to here switch to tri grams (only six.csv)
        B<-NextWord(x,six,"tri")
        A<-rbind(A,B);rm(B)
        
        if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
        
        else{#switch to bigrams (only six.csv)
          B<-NextWord(x,six,"bi")
          A<-rbind(A,B);rm(B,six)
          
          if(length(unique(A$word))>10){output<-tops(A);rm(eight,seven,six,A)}
          
          else{
            
            B<-data.frame(word=c("clean","tickets","six","conference","per","present","plans","anymore","goal","shop","sort"),freq=c(11,10,9,8,7,6,5,4,3,2,1),stage=rep(4,11))
            A<-rbind(A,B);rm(B)
            output<-tops(A);rm(eight,seven,six,A)
            
          }}}}}#close else
  output #print output
}

##########################################################

#function 5: hashtagPredict
#model that predicts hashtags 
#arguments: x=cleaned input 
hashtagPredict<-function(x){
  hashtag<-read.csv("hashtag.csv",stringsAsFactors=FALSE)#read in dataset
  A<-NextWord(x,hashtag,"quad")# check for quadgram matches
  
  if(length(unique(A$word))>10){# if enough matches found stop cycle
    output<-tops(A)
  
    }else{#check trigram matches
    B<-NextWord(x,"hashtag.csv","tri")
    A<-rbind(A,B);rm(B)
    if(length(unique(A$word))>10){
      output<-tops(A)
    
      }else{#check bigram matches
      B<-NextWord(x,"hashtag.csv","bi")
      A<-rbind(A,B);rm(B)
      if(length(unique(A$word))>10){
        output<-tops(A)
      
        }else{#if still not enough then add to 11 hashtags to end of collected data
        B<-data.frame(word=c("#ff","#brewers","#np","#oomf","#sxsw","#nowplaying","#follow","#packers","#twitter","#chicago","#shoutout"),Freq=c(11,10,9,8,7,6,5,4,3,2,1),stage=rep(4,11))
        A<-rbind(A,B);rm(B)
        output<-tops(A)
      }}}
  output
}



