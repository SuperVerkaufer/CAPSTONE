#library(reticulate)
#use_python("/use/local/bin/python")
#library(rsconnect)
#rsconnect::deployApp('C:/Users/spencer.ng/Desktop/TH training/R Code Training MMM/CAPSTONE/rsconnect')

# TRY THIS IF ALL ELSE FAILS remotes::install_github("rstudio/rsconnect")

# SPENCER NG - Capstone NLP Shiny application
#install.packages("shinythemes")
#
#install.packages("caret")
library(caret)
library(reticulate)
library(markdown)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shiny)
library(readr)
library(stringr)
#
Table1 <- readRDS(file = "Table1.rds")
Table2 <- readRDS(file = "Table2.rds")
Table3 <- readRDS(file = "Table3.rds")
Table4 <- readRDS(file = "Table4.rds")
Table5 <- readRDS(file = "Table5.rds")
#
shinyServer(function(input, output) {
  wordPred <- reactive({
    wordstring <- input$inputString
    wordstring <- tolower(wordstring)
    wordstring <- trimws(wordstring, which = "left") 
    wordstring <- gsub(' {2,}',' ', wordstring) 
    strlength <- length(strsplit(wordstring,' ')[[1]])
    if (strlength >3){
      n4pref <- word(wordstring, start=strlength-3, end=strlength)
      n3pref <- word(n4pref, start=2, end=4)
      n2pref <- word(n3pref, 2, 3)
      n1pref <- word(n2pref, 2)
      n4pref <- gsub(" ", "_", n4pref)
      n3pref <- gsub(" ", "_", n3pref)
      n2pref <- gsub(" ", "_", n2pref)
      goodrow5 <- filter(Table5, prefix == n4pref)
      goodrow4 <- filter(Table4, prefix == n3pref)
      goodrow3 <- filter(Table3, prefix == n2pref)
      goodrow2 <- filter(Table2, prefix == n1pref)
      goodrow2 <- goodrow2[!(goodrow2$nextword %in% goodrow3$nextword), ]
      goodrow3 <- goodrow3[!(goodrow3$nextword %in% goodrow4$nextword), ]
      goodrow4 <- goodrow4[!(goodrow4$nextword %in% goodrow5$nextword), ]
    } else if (strlength == 3){
      n3pref <- word(wordstring, start=1, end=3)
      n2pref <- word(n3pref, 2, 3)
      n1pref <- word(n2pref, 2)
      n3pref <- gsub(" ", "_", n3pref)
      n2pref <- gsub(" ", "_", n2pref)
      goodrow4 <- filter(Table4, prefix == n3pref)
      goodrow3 <- filter(Table3, prefix == n2pref)
      goodrow2 <- filter(Table2, prefix == n1pref)
      goodrow2 <- goodrow2[!(goodrow2$nextword %in% goodrow3$nextword), ]
      goodrow3 <- goodrow3[!(goodrow3$nextword %in% goodrow4$nextword), ]
    } else if (strlength == 2) {
      n2pref <- word(wordstring, start=1, end=2)
      n1pref <- word(n2pref, 2)
      n2pref <- gsub(" ", "_", n2pref)
      goodrow3 <- filter(Table3, prefix == n2pref)
      goodrow2 <- filter(Table2, prefix == n1pref)
      goodrow2 <- goodrow2[!(goodrow2$nextword %in% goodrow3$nextword), ]
    } else if (strlength == 1){
      n1pref <- trimws(wordstring, which = "right")
      goodrow2 <- filter(Table2, prefix == n1pref)
    }
    goodrow1 <- Table1[1:5,]
    goodrow0 <- Table1[1:3]
    goodrow0$nextword[1] <- ""
    if (strlength > 3){
      ResultsTab <- rbind(goodrow5, goodrow4, goodrow3, goodrow2, goodrow1)  
    } else if (strlength == 3){
      ResultsTab <- rbind(goodrow4, goodrow3, goodrow2, goodrow1)
    } else if (strlength == 2){
      ResultsTab <- rbind(goodrow3, goodrow2, goodrow1)
    } else if (strlength == 1){
      if (n1pref %in% c("the","to","and")){
        ResultsTab <- goodrow2
      } else
        ResultsTab <- rbind(goodrow2, goodrow1)
    } else {
      ResultsTab <- goodrow0
    }
    ResultsTab <- ResultsTab[order(-ResultsTab$S),]
    ResultsTab <- ResultsTab[1:10,]
    for (i in 1:5){
      if (ResultsTab$nextword[i] == "i"){
        ResultsTab$nextword[i] <- "I"
      } else if (ResultsTab$nextword[i] == "ive"){
        ResultsTab$nextword[i] <- "I've"
      } else if (ResultsTab$nextword[i] == "id"){
        ResultsTab$nextword[i] <- "I'd"
      }
    }
    output$NextWord <- renderText(ResultsTab$nextword[1])
    TopFive <- ResultsTab[1:5,2:3]
    if (strlength >0) {
      return <- TopFive
    } else {
      return <- NA
    }
  })
output$barplot <- renderPlot({ggplot(data = wordPred(), aes_string(x = reorder(Table5$nextword, -Table5$S), y = S)) + 
    geom_col(fill = "blue") + 
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), 
          axis.title.y = element_text(angle=90)) +
    labs(x = "Words")})
  

})

#output$barplot <- renderPlot({ggplot(data = wordPred(), aes_string(x = reorder(nextword, -S), y = S)) + 
#    geom_col(fill = "blue") + 
#    theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), 
#          axis.title.y = element_text(angle=90)) +
#    labs(x = "Words")})
#
#})