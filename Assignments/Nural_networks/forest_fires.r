forest_fires <- read.csv(file.choose())
View(forest_fires)
class(forest_fires)
forest_fires <- as.data.frame(Startups)
attach(forest_fires)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest_fires_norm<-as.data.frame(lapply(forest_fires,FUN=normalize))
summary(forest_fires$Profit) # Normalized form of profit

ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
forest_fires_train <- Startups_norm[ind==1,]
forest_fires_test  <- Startups_norm[ind==2,]


# Creating a neural network model on training data


forest_fires_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(forest_fires_model)

plot(forest_fires_model, rep = "best")

set.seed(12323)
model_results <- compute(forest_fires_model,startups_test[1:4])
predicted_output <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)


str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")