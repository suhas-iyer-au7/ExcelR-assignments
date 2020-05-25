
######## Cutlet data ###################
cutlet<-read.csv(file.choose())# Promotion.xlsx
View(cutlet)
attach(cutlet)
shapiro.test(Unit.A)
shapiro.test(Unit.B)
var.test(Unit.A,Unit.B)
############2 sample T Test  because y is continous x is discrete in two ##################
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)


####################Chi Square(Buyer_ratio)##########

buyer_ratio<-read.csv(file.choose())   # JohnyTalkers.xlsx
View(buyer_ratio) 
attach(buyer_ratio)
str(buyer_ratio)
table1 <- table(Drinks,Person)

buyer_ratio=data.frame(lapply(buyer_ratio, as.character), stringsAsFactors=FALSE)
Stacked_Data <- stack(buyer_ratio,select=c(East=East,West=West,North=North,South=South ))
attach(Stacked_Data)
chisq.test(ind, values)

#########Chi Square(customer_order)#################

library(readxl)
customer_order<-read.csv(file.choose()) # customerorder
View(customer_order)
attach(customer_order)
is.vector(customer_order$Phillippines)
customer_order=data.frame(lapply(customer_order, as.character), stringsAsFactors=FALSE)
str(customer_order)
customer_order <- as.character(customer_order)
table('Error free','Defective')
stack( d , select=c(x1,x2) )
Stacked_Data <- stack(customer_order,select=c(Phillippines=Phillippines,Indonesia=Indonesia,Malta=Malta,India=India ))
View(Stacked_Data)
attach(Stacked_Data)
#t2 <- prop.table(table(Defective))
#t1 <- table(Country)
chisq.test(ind, values)
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

#############Anova (lab turn around time )##########
library(car)
Lab<-read.csv(file.choose())   # ContractRenewal_Data(unstacked).xlsx
View(Lab)
attach(Lab)
shapiro.test(Lab$Laboratory.1)
shapiro.test(Lab$Laboratory.2)
shapiro.test(Lab$Laboratory.3)
shapiro.test(Lab$Laboratory.4)
Stacked_Data <- stack(Lab)
View(Stacked_Data)
attach(Stacked_Data)
bartlett.test(values~ind, data=Stacked_Data) #test for homogeneity of variances
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value =  < 0.05 reject null hypothesis 
#Alternate hypothesis: means are not equal





