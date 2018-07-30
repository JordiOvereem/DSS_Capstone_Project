# Set working directory and load libraries
setwd("C:/Users/joov2/Desktop/Capstone Project")
library(tm, warn.conflicts=F, quietly=T)
library(ggplot2, warn.conflicts=F, quietly=T)
library(dplyr, warn.conflicts=F, quietly=T)
library(RWeka, warn.conflicts=F, quietly=T)
library(stringi, warn.conflicts=F, quietly=T)
library(tidyr, warn.conflicts=F, quietly=T)
library(stringr, warn.conflicts=F, quietly=T)
library(textreg, warn.conflicts=F, quietly=T)

# Load data
blogs_file <- file("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt","r")
blogs <- readLines(blogs_file, encoding = "UTF-8", skipNul = TRUE)
close(blogs_file)

news_file <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt","r")
news <- readLines(news_file, encoding = "UTF-8", skipNul = TRUE)
close(news_file)

twitter_file <- file("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt","r")
twitter <- readLines(twitter_file, encoding = "UTF-8", skipNul = TRUE)
close(twitter_file)

# Clean and sample data
blogs <- iconv(blogs, "latin1", "ASCII", sub="")
news <- iconv(news, "latin1", "ASCII", sub="")
twitter <- iconv(twitter, "latin1", "ASCII", sub="")

### start | for saving complete cleaned corpus ###
blogs <- chartr("a-zA-Z", "a-za-z", blogs)
news <- chartr("a-zA-Z", "a-za-z", news)
twitter <- chartr("a-zA-Z", "a-za-z", twitter)

#corpus <- c(blogs, news, twitter)
#corpus <- removePunctuation(corpus)
#saveRDS(corpus, "corpus_large.RData")
### end | for saving complete cleaned corpus ###

set.seed(2507)
sample <- c(sample(blogs, length(blogs)/80), sample(news, length(news)/80), sample(twitter, length(twitter)/80))
###for saving sample corpus cleaned
sample <- removePunctuation(sample)
sample <- removeNumbers(sample)
saveRDS(sample, "corpus_sample.RData")
sample <- removeWords(sample, stopwords("en"))
sample <- stripWhitespace(sample)
saveRDS(corpus, "cleaned_corpus.RData")


corpus <- VCorpus(VectorSource(sample))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# Tokenize sample and calculate n-grams frequencies
# Functions for tokenizing sample
uni <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quad <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
#penta <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

# Create matrices
uni_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = uni))
bi_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = bi))
tri_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = tri))
quad_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = quad))
#penta_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = penta))

# Calculate frequencies
uni_corpus <- findFreqTerms(uni_matrix, lowfreq = 50, highfreq = Inf)
bi_corpus <- findFreqTerms(bi_matrix, lowfreq = 30, highfreq = Inf)
tri_corpus <- findFreqTerms(tri_matrix, lowfreq = 5, highfreq = Inf)
quad_corpus <- findFreqTerms(quad_matrix, lowfreq = 3, highfreq = Inf)
#penta_corpus <- findFreqTerms(penta_matrix, lowfreq = 10, highfreq = Inf)

uni_frequencies <- rowSums(as.matrix(uni_matrix[uni_corpus,]))
bi_frequencies <- rowSums(as.matrix(bi_matrix[bi_corpus,]))
tri_frequencies <- rowSums(as.matrix(tri_matrix[tri_corpus,]))
quad_frequencies <- rowSums(as.matrix(quad_matrix[quad_corpus,]))
#penta_frequencies <- rowSums(as.matrix(penta_matrix[penta_corpus,]))

df1 <- data.frame(unigram = names(uni_frequencies), frequency = uni_frequencies)
df2 <- data.frame(bigram = names(bi_frequencies), frequency = bi_frequencies)
df3 <- data.frame(trigram = names(tri_frequencies), frequency = tri_frequencies)
df4 <- data.frame(quadgram = names(quad_frequencies), frequency = quad_frequencies)
#df5 <- data.frame(pentagram = names(penta_frequencies), frequency = penta_frequencies)

df <- df1[order(-df1$frequency),]
saveRDS(df, "unigrams.RData")
rm(df)

df <- df2[order(-df2$frequency),]
df <- df %>% separate(bigram, c("start", "end"))
saveRDS(df, "bigrams.RData")
rm(df)

df <- df3[order(-df3$frequency),]
df <- df %>% separate(trigram, c("word1", "word2", "end"))
df <- df %>% unite("start", c("word1", "word2"), sep = " ")
saveRDS(df, "trigrams.RData")
rm(df)

df <- df4[order(-df4$frequency),]
df <- df %>% separate(quadgram, c("word1", "word2", "word3", "end"))
df <- df %>% unite("start", c("word1", "word2", "word3"), sep = " ")
saveRDS(df, "quadgrams.RData")
rm(df)

#df <- df5[order(-df5$frequency),]
#df <- df %>% separate(pentagram, c("word1", "word2", "word3", "word4", "end"))
#df <- df %>% unite("start", c("word1", "word2", "word3", "word4"), sep = " ")
#saveRDS(df, "pentagrams.RData")
#rm(df)

rm(list=ls())
