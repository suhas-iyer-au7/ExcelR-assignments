library(C50)
library(party)
library(tree)
library(caret)
df <- read.csv(file.choose(),1)
View(df)
Target = ifelse(df$Taxable.Income<=30000, "Risky", "Good")
View(Target)
df1 <- cbind(df,Target)
View(df1)
df1 <- df1[-3]
intrain <- createDataPartition(df1$Target,p=.70, list=F)
training1 <- df1[intrain,]
View(training1)
testing1 <- df1[-intrain,]
View(testing1)
#model building
model <- C5.0(training1$Target~.,data = training1,trails = 1000)
?C5.0
summary(model)
pred <- predict.C5.0(model,testing1[,-6])
a <- table(testing1$Target,pred)
View(a)
sum(diag(a)/sum(a))
plot(model)
model1 <- ctree(training1$Target~.,data = training1)
plot(model1)
