#setwd("C:\\Desktop")
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/product-reviews/B07XVMDRZY?ref=ods_ucc_cust_kindle_B07XVMDRZY_nrc_ucc"
amazon_reviews <- NULL
for (i in 1:7){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
  View (amazon_reviews)
}
write.table(amazon_reviews,"iphone.txt",row.names = F)
write.csv(amazon_reviews, "iphone.csv",row.names = F)


#######################################################################################
# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}
write.csv(snapdeal_reviews, "snapdeal_reviews.csv",row.names = F)
write.table(snapdeal_reviews,"samsung.txt",row.names = FALSE)
getwd()

data <- read.csv(file.choose(), header = TRUE)
reviews <- as.character(data$text)
class(reviews)
s <- get_nrc_sentiment(reviews)
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon reviews')
