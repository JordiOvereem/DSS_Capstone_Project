#setwd("C:/Users/joov2/Desktop/Capstone Project/Capstone_Project_JOOV")

suppressPackageStartupMessages(c(
  library(tm),
  library(ggplot2),
  library(dplyr),
  library(RWeka),
  library(stringi),
  library(tidyr),
  library(stringr),
  library(textreg)))

# Load data
corpus_sample <- readRDS("./corpus_sample.RData")
cleaned_corpus <- readRDS("./cleaned_corpus.RData")
df1 <- readRDS("./unigrams.RData")
df2 <- readRDS("./bigrams.RData")
df3 <- readRDS("./trigrams.RData")
df4 <- readRDS("./quadgrams.RData")

source("./DSS Capstone Project - Prediction Algorithm_001.R")

shinyServer(function(input, output) {
  output$next_word <- renderPrint({
    result <- predict(input$input_sentence)
    result
  });
  output$input2 <- renderText({
    input$input_sentence});
}
)