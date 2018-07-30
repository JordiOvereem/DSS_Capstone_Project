library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Coursera Data Science Capstone Project - Predict next word",
             tabPanel("Predict word",
                      HTML("<strong>Author: J. Overeem</strong>"),
                      br(),
                      HTML("<strong>Date: 27 July 2018</strong>"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This is a tool for predicting the next word of the word, text or sentence you will type in the textbox below."),
                          hr(),
                          textInput("input_sentence", "Enter a word, text or a sentence",value = ""),
                          hr()
                          ),
                        mainPanel(
                          h4("Word prediction"),
                          h5("The next word for the given word, text or sentence is predicted using some 
                             NLP techniques applied to the corpus provided by the Coursera"),
                          h5("Note that the app may take some time to load."),
                          strong("Your input"),
                          verbatimTextOutput("input2"),
                          hr(),
                          strong("The predicted next word is:"),
                          strong(code(textOutput("next_word"))),
                          hr()
                          )
                        )
                      )
             )
)