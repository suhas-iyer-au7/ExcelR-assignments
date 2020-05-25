delivery_time <- read.csv("C:/Users/suhas/Downloads/delivery_time.csv")
View(delivery_time)
attach(delivery_time)

# Exploratory data analysis
summary(delivery_time)

# Graphical exploration
boxplot(Sorting.Time,col="dodgerblue4")

hist(Delivery.Time)
hist(Sorting.Time)

qqnorm(Delivery.Time)
qqline(Sorting.Time)

qqnorm(Delivery.Time)
qqline(Sorting.Time)


#scatter plot
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)

reg <- lm(Delivery.Time~Sorting.Time) # Y ~ X
summary(reg)


confint(reg,level=0.95)


pred <- predict(reg,interval="predict")

pred <- as.data.frame(pred)

print(pred)
View(pred)



# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time))
summary(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)


reg_log<-lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
pred2
cor(pred2$fit, wc.at$AT) 



reg1<-lm(log(AT)~Waist + I(Waist*Waist), data=wc.at)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred3<-predict(reg1,interval="predict")
?predict


pred3<-as.data.frame(pred)
pred3
View(pred3)



?exp

exp(pred3$fit)

cor(exp(pred$fit),wc.at$AT)





reg_sqrt1<-lm(sqrt(AT)~Waist, data=wc.at)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")

reg_log1<-lm(log(AT)~Waist, data=wc.at)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")
