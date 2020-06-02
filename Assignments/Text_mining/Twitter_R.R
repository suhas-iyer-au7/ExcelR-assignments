#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library(rvest)
library(XML)
library(magrittr)
library(syuzhet)
library(tm)
library(wordcloud)

library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='cEismW87NLA3JeMWfeYsGiy44', # Consumer Key (API Key)
                         consumerSecret='hmayixY74kAFrGuPULNLWcEZz07X0Niq6q48wQccPClAK8s3mP', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("cEismW87NLA3JeMWfeYsGiy44", # Consumer Key (API Key)
                    "hmayixY74kAFrGuPULNLWcEZz07X0Niq6q48wQccPClAK8s3mP", #Consumer Secret (API Secret)
                    "1083324711205974018-MNEk2XVaW9qa8hMousEjJEa6PK0q0d",  # Access Token
                    "lu9ClXFAFNbYpmGjBStqUrEAVpU9o8BkDZEgkSchC7pOm")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('imVkohli', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "kohli.csv",row.names = F)
write.table(TweetsDF, "kohli.txt",row.names = F)
getwd()

data <- read.csv(file.choose(), header = TRUE)
data['reviews'] <- as.character(data$text)
class(reviews)
s <- get_nrc_sentiment(as.character(data$text))
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Kohli Tweets')
############################Including Word Clouds 1 snapdeal #######################################
kohli<- read.delim(file.choose(), header = TRUE)
str(kohli)
View(kohli)
corpus <- kohli[-1,]
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
cleanset<-tm_map(cleanset,removeWords, c('can','bat'))

cleanset<-tm_map(cleanset,removeWords, c('cricket'))
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


