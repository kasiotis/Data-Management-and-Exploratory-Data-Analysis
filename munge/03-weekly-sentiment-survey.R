library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)


get.weekly.sentiment.plot = function(weekly.sentiment.dataset) {
  #Create a vector containing only the text
  reasons = weekly.sentiment.dataset
  #Creating a corpus
  docs = SimpleCorpus(VectorSource(reasons))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  #removing unnecessary symbols from the text
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove English common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # specifying my stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("course")) 
  # Remove punctuation
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  #Using TermDocumentMatrix to create a data frame containing each
  #word in your first column and their frequency in the second column
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  set.seed(1234)
  #Generating the word cloud
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.45, 
            colors=brewer.pal(8, "Dark2"))
  
}


