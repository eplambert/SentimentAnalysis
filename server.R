# server side 

library(shiny)


# Load the AFINN dictionary
afinn <- read.csv("AFINN.csv")
afinn$score = as.numeric(afinn$score)

# Sentiment function
sentiment <- function(text) {
          text <- paste(text, collapse = " ")
          # remove punctuation
          text <- gsub("[[:punct:]]", "", text)
          # split the string at spaces
          text_vector <- strsplit(text, " ")[[1]]
          
          # check for significant word combinations
          for (i in 1:(length(text_vector) - 1)) {
                    if (text_vector[i] == "cant" && text_vector[i+1] == "stand") {
                              text_vector[i] <- "cant stand"
                              text_vector <- text_vector[-(i+1)]
                    } 
                    if (text_vector[i] == "cashing" && text_vector[i+1] == "in") {
                              text_vector[i] <- "cashing in"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "cool" && text_vector[i+1] == "stuff") {
                              text_vector[i] <- "cool stuff"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "does" && text_vector[i+1] == "not" && text_vector[i+2] == "work") {
                              text_vector[i] <- "does not work"
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              break
                    }
                    if (text_vector[i] == "dont" && text_vector[i+1] == "like") {
                              text_vector[i] <- "dont like"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "green" && text_vector[i+1] == "wash") {
                              text_vector[i] <- "green wash"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "green" && text_vector[i+1] == "washing") {
                              text_vector[i] <- "green washing"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "made" && text_vector[i+1] == "up") {
                              text_vector[i] <- "made up"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "messing" && text_vector[i+1] == "up") {
                              text_vector[i] <- "messing up"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "no" && text_vector[i+1] == "fun") {
                              text_vector[i] <- "no fun"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "not" && text_vector[i+1] == "good") {
                              text_vector[i] <- "not good"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "not" && text_vector[i+1] == "working") {
                              text_vector[i] <- "not working"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "once" && text_vector[i+1] == "in" && text_vector[i+2] =="a" && text_vector[i+3] == "lifetime") {
                              text_vector[i] <- "once in a lifetime"
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              break
                    }
                    if (text_vector[i] == "screwed" && text_vector[i+1] == "up") {
                              text_vector[i] <- "screwed up"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "self" && text_vector[i+1] == "confident") {
                              text_vector[i] <- "self confident"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "self" && text_vector[i+1] == "deluded") {
                              text_vector[i] <- "self deluded"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "short" && text_vector[i+1] == "sighted") {
                              text_vector[i] <- "short sighted"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "short" && text_vector[i+1] == "sightedness") {
                              text_vector[i] <- "short sightedness"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "some" && text_vector[i+1] == "kind") {
                              text_vector[i] <- "some kind"
                              text_vector <- text_vector[-(i+1)]
                    }
                    if (text_vector[i] == "son" && text_vector[i+1] == "of" && text_vector[i+2] == "a" && text_vector[i+3] == "bitch") {
                              text_vector[i] <- "son of a bitch"
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              break
                    }
                    if (text_vector[i] == "not" && text_vector[i+1] == "a" && text_vector[i+2] == "fan") {
                              text_vector[i] <- "son of a bitch"
                              text_vector <- text_vector[-(i+1)]
                              text_vector <- text_vector[-(i+1)]
                              break
                    }
          }
          
          # check if a word is in the word vector then add the score
          score <- 0
          
          for (j in 1:length(text_vector)) {
                    if (length(afinn$score[afinn$word == text_vector[j]]) != 0) {
                              score <- score + afinn$score[afinn$word == text_vector[j]]
                    }
          }
          
          return(score)
}

shinyServer(function(input, output) {
          
          output$text1 <- renderText({
                    score <- 0
                    score <- sentiment(input$text)
                    
                    paste("Your sentiment score is: ", score)
          })
          
          
})
