credit_card <- read.csv(file.choose())
View(credit_card)
attach(credit_card)
data1=credit_card[,-1]
View(data1)
credit_card <- na.omit(credit_card)
View(credit_card)
model <- glm(card~.,data=credit_card[,-1],family = "binomial",maxit = 100)

# Confusion matrix table 
prob <- predict(model,type=c("response"),credit_card)
prob

confusion<-table(prob>0.5,card)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #92.03%

pred_values <- NULL
yes_no <- NULL
for (i in 1:1319){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

credit_card[,"prob"] <- prob
View(prob)
credit_card[,"pred_values"] <- pred_values
View(pred_values)
credit_card[,"yes_no"] <- yes_no
View(credit_card)

# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
