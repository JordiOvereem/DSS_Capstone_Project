# Predict function part 1 | Predictions based upon predefined N-grams
predict <- function(sentence){
  value = ""
  sentence <- chartr("a-zA-Z", "a-za-z", sentence)
  sent <- unlist(strsplit(sentence, " "))
  sent <- removePunctuation(sent)
  sent <- removeNumbers(sent)
  
  if(length(sent) == 1){
    lookupvalue <- paste(sent)
    value = bigram(lookupvalue)
  }
  
  if(is.na(value)||length(sent) == 2){
    lookupvalue <- paste(sent[length(sent)-1], sent[length(sent)], sep = " ")
    value = trigram(lookupvalue)
    
  }
  
  if(is.na(value)||length(sent) >= 3){
    lookupvalue <- paste(sent[length(sent)-2], sent[length(sent)-1], sent[length(sent)], sep = " ")
    value = quadgram(lookupvalue)
  }
  
  if(is.na(value)){
    if(length(sent) >= 3){
      lookupvalue <- paste(sent[length(sent)-2], sent[length(sent)-1], sent[length(sent)], sep = " ")
      pattern <- gsub(" ", "+ +", lookupvalue)
      pattern <- paste("(", pattern, "+ +[^ ]+ )", sep="")
      value = extragram(lookupvalue, pattern)
  }
    if(is.na(value)){
      sent <- sent[!(sent %in% stopwords("en"))]
      if(length(sent) >= 3){
        lookupvalue <- paste(sent[length(sent)-2], sent[length(sent)-1], sent[length(sent)], sep = " ")
        pattern <- gsub(" ", "+ +", lookupvalue)
        pattern <- paste("(", pattern, "+ +[^ ]+ )", sep="")
        value = extragram2(lookupvalue, pattern)
      }
    }
  }
  
  if(is.na(value)){
    value = "the"
  }
  
  next_word <- value
  return(next_word)
}

bigram <- function(one_word) {
  df2 <- readRDS("./bigrams.RData")
  list <- df2[df2$start == one_word,]
  if(length(list) == 0) return(NA)
  if(length(list) > 0) return(list$end[1])
}

trigram <- function(two_words) {
  df3 <- readRDS("./trigrams.RData")
  list <- df3[df3$start == two_words,]
  if(length(list) == 0) return(NA)
  if(length(list) > 0) return(list$end[1])
}

quadgram <- function(three_words) {
  df4 <- readRDS("./quadgrams.RData")
  list <- df4[df4$start == three_words,]
  if(length(list) == 0) return(NA)
  if(length(list) > 0) return(list$end[1])
}

# Predict function part 2 | Predictions based on "at the moment searching"
# and in case nothing is found from the predefined N-gram libraries
extragram <- function(words, pattern) {
  corpus_sample <- readRDS("./corpus_sample.RData")
  sample <- corpus_sample[grepl(words, corpus_sample)]
  str <- ""
  df <- ""
  i <- length(sample)
  if(i == 0) return(NA)
  if (i > 0) {
    for (i in 1:i) {  str[i]<- str_extract(sample[i],  pattern)
    df[i]<- stri_extract_last_words(str[i]) 
    }
  }
  if(length(df) == 0) return(NA)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  df <- as.data.frame(table(df$df), stringsAsFactors=FALSE)
  names(df)[1] <- "word"
  names(df)[2] <- "frequency"
  df <- df[order(-df$frequency),]
  if(length(df) == 0) return(NA)
  return(df$word[1])
}

extragram2 <- function(words, pattern) {
  cleaned_corpus <- readRDS("./cleaned_corpus.RData")
  sample <- cleaned_corpus[grepl(words, cleaned_corpus)]
  str <- ""
  df <- ""
  i <- length(sample)
  if(i == 0) return(NA)
  if (i > 0) {
    for (i in 1:i) {  str[i]<- str_extract(sample[i],  pattern)
    df[i]<- stri_extract_last_words(str[i]) 
    }
  }
  if(length(df) == 0) return(NA)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  df <- as.data.frame(table(df$df), stringsAsFactors=FALSE)
  names(df)[1] <- "word"
  names(df)[2] <- "frequency"
  df <- df[order(-df$frequency),]
  if(length(df) == 0) return(NA)
  return(df$word[1])
}

##### Below is used for quick testing some phrases ######
#input_sentence <- "over here test"
#sentence <- chartr("a-zA-Z", "a-za-z", input_sentence)
#sent <- unlist(strsplit(sentence, " "))
#lookupvalue <- paste(sent[length(sent)-2], sent[length(sent)-1], sent[length(sent)], sep = " ")
#pattern <- gsub(" ", "+ +", lookupvalue)
##pattern <- paste("(", pattern, "+ +[^ ]+ )", sep="")

#sample <- corpus[grepl(lookupvalue, corpus)]
#str <- ""
#df <- ""
#i <- length(sample)
#if (i > 0) {
#  for (i in 1:i) {  str[i]<- str_extract(sample[i],  pattern)
#  df[i]<- stri_extract_last_words(str[i]) 
#  }
#}
#if(length(df) == 0) return(NULL)
#df <- as.data.frame(df, stringsAsFactors=FALSE)
#df <- as.data.frame(table(df$df), stringsAsFactors=FALSE)
##names(df)[1] <- "word"
#names(df)[2] <- "frequency"
#df <- df[order(-df$frequency),]
#list(head(df,5))
