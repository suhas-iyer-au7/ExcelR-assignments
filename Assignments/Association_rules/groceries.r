install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

mydata1 <- read.csv(file.choose(),1)
View(mydata1)
summary(mydata1)
rules <- apriori(mydata1,parameter=list(support=0.002, confidence = 0.5,minlen=1))
rules

inspect(rules)                 
inspect(head(sort(rules, by = "lift")))
plot(rules)

head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "graph")


#Different support and confidence levels

rules1<- apriori(mydata1,parameter=list(support=0.003, confidence = 0.3,minlen=1))
rules1

inspect(rules1)                 
inspect(head(sort(rules1, by = "lift")))
plot(rules1)

head(quality(rules1))
plot(rules1, method = "grouped")
plot(rules1,method = "graph")

#Different support and confidence levels

rules2<- apriori(mydata1,parameter=list(support=0.005, confidence = 0.5,minlen=1))
rules2

inspect(rules2)                 
inspect(head(sort(rules2, by = "lift")))
plot(rules2)

head(quality(rules2))
plot(rules2, method = "grouped")
plot(rules2,method = "graph")
