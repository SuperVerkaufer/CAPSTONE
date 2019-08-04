
# SPENCER NG - Capstone NLP Shiny application
#
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shiny)
library(readr)
library(stringr)
#
shinyUI(navbarPage("Coursera Data Science: Capstone Project",
                   #theme = shinytheme("united"),
                   #theme = shinytheme("yeti"),
                   theme = shinytheme("yeti"),
                   tabPanel("Predictive Textual Analytics", HTML("<strong>Developer: spencer ng</strong>"),
                            br(),
                            HTML("<strong>Time: 30 July, 2019</strong>"),
                            fluidRow(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              column(3), column(6, tags$div(textInput("inputString",
                                                                      label = h2("Enter words here:"),
                                                                      value = ),
                                                            tags$span(style="color:maroon",("HIT ENTER TO SEE PREDICTED WORD!")),
                                                            br(),
                                                            tags$hr(),
                                                            h2("Predicted Next Word:"),
                                                            tags$span(style="color:maroon",
                                                                      tags$strong(tags$h3(textOutput("NextWord")))),
                                                            br(),
                                                            br(),
                                                            h3("Try again. Type in another Word!"),
                                                            plotOutput("barplot")),
                                                align="center")
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                              column(4, p("")),
                              column(12, includeMarkdown("about.md"))
                            )
                   )
                   
)
)