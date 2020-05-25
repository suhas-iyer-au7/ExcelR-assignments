bank <- read.csv(file.choose(),sep=';')
View(bank)
attach(bank)
data1=credit_card[,-1]
View(data1)
bank <- na.omit(bank)
View(credit_card)
model <- glm(y~.,data=bank,family = "binomial",maxit = 100)

# Confusion matrix table 
prob <- predict(model,type=c("response"),bank)
prob

confusion<-table(prob>0.5,y)

confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #90.03%

pred_values <- NULL
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

bank[,"prob"] <- prob
View(prob)
bank[,"pred_values"] <- pred_values
View(pred_values)
bank[,"yes_no"] <- yes_no
View(bank)



# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
