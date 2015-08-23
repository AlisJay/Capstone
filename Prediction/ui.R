#Data science specialisation Capstone
#A.J.Nicholson
#NextWord user interface (ui.R)

#####################################

#load required packages 
library("shiny");library("shinythemes");library(stringr)

#shinyUI
shinyUI(navbarPage("NextWord",inverse=TRUE,
                   
				   #first tab (help page with instructions)
				   tabPanel("Help",
                            fluidPage(theme=shinytheme("cerulean"),
                                      titlePanel("Welcome to NextWord"),
                                      mainPanel(
                                        h3("Introduction"),
                                        p("NextWord is a simple app that aims to predict the next word you type."),
                                        p("It was created by myself (A.J.Nicholson(2015)) for the John Hopkins' Coursera Data Science Capstone project."),
                                        h3("How to use"),
                                        p("Using NextWord couldn't be simpler!"),
                                        p(strong("Step 1:"), "Go to the predict tab (but you may want to read the rest of the instruction first)"),
                                        p(strong("Step 2:"), "Type your sentence into the text input box (minus the word you want to predict)"),
                                        p(strong("Step 3:"), "Select your model (if this is your first go you probably want to go with Standard, see model section below for more info)"),
                                        p(strong("Step 4:"), "Hit go. The app will then give you the most likely next word 
      (the time this takes does depend on the sentence typed)"),
                                        p(strong("Step 5:"), "If this isn't your word hit the not my word button to get more options"),
                                        h3("Models"),
                                        p("In NextWord you can run one of three models: Standard, Backwards and Hashtag"),
                                        p("The", strong("Standard"), "model is the best place to start. This runs through the most common half of the words from the dataset in order of word frequency."),
                                        p("The", strong("Backwards"), "model runs through the least common half of the words in inverse order of word frequency. 
      If your word is unusual or you haven't had luck with the standard model give this one a try. 
      Not sure if you word is common? Check the word cloud in the About tab"),
                                        p("The", strong("Hashtag"), "model aims to predict the #hashtag your going to use at the end of your sentence. A bit of fun for the tweeters :)"),
                                        h3("More information"),
                                        p("Information about the reference dataset used in prediction can be found in the About tab.
                                          Also see", a("http://rpubs.com/AlisJay/NextWord")),
                                        width=12)
                            )
                            
                   ),
                   
				   #second tab where prediction occurs 
				   tabPanel("Predict",
                            fluidPage(
                              titlePanel("Predict"),
                              fluidRow(
                                column(width=6,
                                       #input 
									   textInput("txt",label=h3("Type your sentence here")), 
                                       selectInput("model","model selection",
                                                   choice=c("Standard"=1,"Backward"=2,"Hashtag"=3),selected=1),
                                       #predict input button (press once input entered)
									   actionButton("P","Predict",icon("play-circle",lib="font-awesome"))
                                ),
                                column(width=6,
                                       
									   #appears after predict button pressed
									   conditionalPanel(condition="input.P>0",
                                                        #appears immediately
														h4("running analysis please wait..."),
                                                        h3("Your word is..."),
                                                        #appears once analysis run
														h2(textOutput("word")),
                                                        
														#not my word button
														actionButton("no","Not my word",icon("thumbs-down",lib="font-awesome")),
                                                        #appears if not my word button pushed
														conditionalPanel(
                                                          condition= "input.no>0",
                                                          h3("The next ten most likely words are ..."),
                                                          textOutput("topten"),
                                                          #if running standard model this will appear
														  conditionalPanel(condition="input.model==1",
                                                                           h4("Still not your word? try running the backwards model")
                                                        ))                                 
                                       )
                                )
                              )
                              
                            )
                            
                   ),
                   
				   #third tab gives info on data and models 
				   tabPanel("About",
                            fluidPage(
                              fluidRow(h2("About the data")),
                              fluidRow(
                                column(width=7,
                                       p("Between them the models use a set of 8 data frames.Each with 7 fields; 
									   the word (that could be predicted); the previous 1, 2 and 3 words;and the frequencies of the n-grams in the raw dataset.
                                         The data is split into sets by word frequency"),
                                       p("The raw data comes from blog, news and twitter entries in American English.
                                         It can be downloaded from:", a("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")),
                                       h3("How the models work"),
                                       p("All the models start by trying to match the last three words of your sentence with the Previous 3 field in their first dataset.
                                         If this is unsuccessful it will move on to the next dataset and try again.
                                         If no previous three matches can be found then it will try and match the previous 2 fields and then previous 1 fields.
                                         If their still aren't enough matches at the end of the cycle then the model will predict the most frequent words"
                                         ),
                                       p("The models run through their cycles until 11 unique words have been found. 
                                         This means that the length of time taken is dependant on the sentence entered.
                                         Once the matches have been found the results are ordered by the stage of the match and then by frequency.
                                         meaning previous3 matches are prioritised over previous 2 and more common tri-grams over less common."),
                                       p("The standard model uses 4 data frames containing the most common words, and begins with the most common words.
                                         The backwards model uses 3 data frames, starting with the most rare.
                                         The hashtag model uses a single data frame in which all words start with a #.")
                                                                              
                                ),
                                
                                #word clouds for each model
								column(width=5,
                                       h3("Some words from the models"),
                                       #pick which model's cloud to view 
									   selectInput("cloud","Choose a model",
                                                   choice=c("Standard"=1,"Backward"=2,"Hashtag"=3),selected=1),
                                      
									  #loads clouds stored as png images in www folder 
									   conditionalPanel(condition="input.cloud==1",
                                                        img(src="topwords.png",height=300,width=300)),
                                       conditionalPanel(condition="input.cloud==3",
                                                        img(src="hashtags.png",height=300,width=300)),
                                       conditionalPanel(condition="input.cloud==2",
                                                        img(src="bottomwords.png",height=300,width=300)))
                                
                              
                            )
                            
                   )
                   
)
)
)

