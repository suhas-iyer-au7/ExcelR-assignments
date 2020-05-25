 Salary_Data <- read.csv("C:/Users/suhas/Downloads/Salary_Data.csv")
  View(Salary_Data)
attach(Salary_Data)

# Exploratory data analysis
summary(Salary_Data)

# Graphical exploration
boxplot(YearsExperience,col="dodgerblue4")

hist(YearsExperience)
hist(Salary)

qqnorm(YearsExperience)
qqline(Salary)

qqnorm(YearsExperience)
qqline(Salary)


#scatter plot
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)

reg <- lm(YearsExperience~Salary) # Y ~ X
summary(reg)


confint(reg,level=0.95)


pred <- predict(reg,interval="predict")

pred <- as.data.frame(pred)

print(pred)
View(pred)



# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike))
summary(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)


reg_log<-lm(Churn_out_rate~log(Salary_hike))
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)



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
