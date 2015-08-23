#Data science specialisation Capstone
#A.J.Nicholson
#Predict 3: Shell functions 

#Function for prediction arranged over three levels:
#1= shell/ top level used in server.R (prediction() and cleantxt())
#2= model level, called by prediction() (hashtagPredict(),backwardPredict(),forwardPredict())
#3= base level, called by model functions (NextWord(),tops())

##########################################################

#Function 1: Prediction (shell function used in server.R)
#selects which model to run
#arguements: x= clean data, m=model to use
prediction<-function(x=cleantxt,m=input$model){
  if(m==1){
    p<-forwardPredict(x)
  }else{
    if(m==2){
      p<-backwardPredict(x)
    }else{
      p<-hashtagPredict(x)
    }
  }
  p
}

##########################################################

#Function 2: cleantxt()
#takes raw input and turns it into a form that can be checked against the reference datasets
cleantxt<-function(x){
  library(stringr)
  #clean
  text<- gsub("&"," and ",x)
  text<-gsub("''",".",text)
  text<-gsub("[^[:alnum:][:space:]#']", " ", text)
  text<-gsub("\\d","",text)
  text<-gsub(" '|^'| ' "," ",text)
  text<-gsub(" # "," ",text)
  text<-gsub("\\s+"," ",text)
  text<-tolower(text)
  
  #split
  Previous1<-word(text,-1)
  Previous2<-word(text,-2,-1)
  Previous3<-word(text,-3,-1)
  
  
  output<-data.frame(Previous3,Previous2,Previous1)
  output  
}

