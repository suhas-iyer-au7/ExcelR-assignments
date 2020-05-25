#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
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

getwd()

data <- read.csv(file.choose(), header = TRUE)
data['reviews'] <- as.character(data$text)
class(reviews)
s <- get_nrc_sentiment(as.character(data$text))
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Kohli Tweets')
