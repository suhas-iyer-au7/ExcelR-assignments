emp_data <- read.csv("C:/Users/suhas/Downloads/emp_data.csv")
View(emp_data)
attach(emp_data)

# Exploratory data analysis
summary(emp_data)

# Graphical exploration
boxplot(Churn_out_rate,col="dodgerblue4")

hist(Salary_hike)
hist(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Churn_out_rate)


#scatter plot
plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)

reg <- lm(Churn_out_rate~Salary_hike) # Y ~ X
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
