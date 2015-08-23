#Data science specialisation Capstone
#A.J.Nicholson
#server.R file for NextWord App

source("helpers.R")# file with all predict functions 

shinyServer(function(input, output){
  #run functions when predict action button pressed
  X<-eventReactive(input$P,
                   {prediction(cleantxt(input$txt),input$model)})
  
  #outputs 
  output$word<-renderText({X()[[1]]})
  output$topten<-renderText({X()[[2]]})
  
  
 })