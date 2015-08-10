# User Interface to perform sentiment analysis
library(shiny)

# Define the UI for the application
shinyUI(fluidPage(
          # Application Title
          titlePanel("Analyzing the Sentiment of your message"),
          
          # Sidebar with a slider input for the number of bins
          sidebarLayout(
                    sidebarPanel(
                              helpText("This tool uses natural language processing techniques to determine the how positive or negative your message is"),
                              
                              textInput("text", label = h3("Input Text"), value = "Enter your Message"),
                              submitButton("Submit")
                              
                              
                    ),
                    mainPanel(
                              h2(helpText("Your input's word cloud")),
                              h3(textOutput('text1')),
                              helpText("The more negative your score the more negative the sentiment. The 
                                       more positive the sentiment score the more negative the sentiment.")
                              
                              )
          )
          ))
