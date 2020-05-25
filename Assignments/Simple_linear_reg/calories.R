calories_consumed <- read.csv("C:/Users/suhas/Downloads/calories_consumed.csv")
View(calories_consumed)
attach(calories_consumed)

# Exploratory data analysis
summary(Calories.Consumed)

# Graphical exploration
boxplot(Calories.Consumed,col="dodgerblue4")

hist(Calories.Consumed)
hist(Weight.gained..grams.)

qqnorm(Calories.Consumed)
qqline(Calories.Consumed)

qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)


#scatter plot
plot(Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams., Calories.Consumed)

reg <- lm(Weight.gained..grams.~Calories.Consumed ) # Y ~ X
summary(reg)


confint(reg,level=0.95)


pred <- predict(reg,interval="predict")

pred <- as.data.frame(pred)

print(pred)
View(pred)
?predict


# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed))
summary(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)


reg_log<-lm(AT~log(Waist), data=wc.at)
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
