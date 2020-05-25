library(readr)
install.packages("e1071")
library(e1071)
library(caret)

train_sal <- read.csv(file.choose())
str(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)

test_sal <- read.csv(file.choose())
str(test_sal)

test_sal$educationno <- as.factor(test_sal$educationno)

Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model
Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)confusionMatrix(Model_pred,test_sal$Salary)
