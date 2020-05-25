#setwd("C:\\Desktop")
library(rvest)
library(XML)
library(magrittr)
library(syuzhet)
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
write.table(Inception_reviews,"Inception_reviews",row.names = F)
write.csv(Inception_reviews, "Inception_reviews.csv",row.names = F)
data <- read.csv(file.choose(), header = TRUE)
reviews <- as.character(data$text)
class(reviews)
s <- get_nrc_sentiment(reviews)
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Inception movie reviews')
