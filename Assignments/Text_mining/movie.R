#setwd("C:\\Desktop")
library(rvest)
library(XML)
library(magrittr)
library(syuzhet)
library(tm)
library(wordcloud)
# Inception movie Reviews #############################
aurl <- "https://www.imdb.com/title/tt1375666/reviews?ref_=tt_ql_3"
Inception_reviews <- NULL
for (i in 1:7){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  Inception_reviews <- c(Inception_reviews,rev)
  View (Inception_reviews)
}
write.table(Inception_reviews,"Inception_reviews.txt",row.names = F)
write.csv(Inception_reviews, "Inception_reviews.csv",row.names = F)
data <- read.csv(file.choose(), header = TRUE)
reviews <- as.character(data$text)
class(reviews)
s <- get_nrc_sentiment(reviews)
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Inception movie reviews')
############################Including Word Clouds#######################################

inception <- read.delim(file.choose(), header = TRUE)
str(inception)
View(inception)
corpus <- inception[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset<-tm_map(cleanset,removeWords, c('can','film'))

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset,stripWhitespace)
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
library(wordcloud)

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

