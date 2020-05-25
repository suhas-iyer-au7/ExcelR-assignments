#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)
data<- read.csv(file.choose())
View(data)
hist(data$Book.Rating)
#the datatype should be realRatingMatrix inorder to build recommendation engine

data_matrix <- as(data, 'realRatingMatrix')
#Popularity based 

book_recomm_model1 <- Recommender(data_matrix, method="POPULAR")

#Predictions
recommended_items1 <- predict(book_recomm_model1, data_matrix[1], n=4)
as(recommended_items1, "list")
## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering
book_recomm_model2 <- Recommender(data_matrix, method="UBCF")
?Recommender
#install.packages("IBCF.MTME")
#library(IBCF.MTME)

#Predictions 
recommended_items2 <- predict(book_recomm_model2,data_matrix[1], n=4)
as


