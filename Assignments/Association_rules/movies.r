install.packages("arules")
library(arules)
install.packages("readxl")
library(readxl)

mydata1 <- read.csv(file.choose(),1)
View(mydata1)
rules1=mydata1[,1:5]
rules2=mydata1[,6:15]
View(rules1)
View(rules2)

rules22 <- apriori(as.matrix(rules2),parameter=list(support=0.02,confidence=0.7,minlen=1)) 
inspect(rules22)
inspect(sort(rules22, by="lift"))
inspect(head(sort(rules22,by="lift")))

# Different Ways of Visualizing Rules
plot(rules22)
plot(rules22,method="grouped")
plot(rules22[1:30],method = "graph") # for good visualization try plotting only few rules
