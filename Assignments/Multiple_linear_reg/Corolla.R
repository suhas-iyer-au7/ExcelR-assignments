ToyotaCorolla <- read.csv("C:/Users/suhas/Downloads/ToyotaCorolla.csv")
View(ToyotaCorolla)
Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
attach(Corolla)
summary(Corolla)
plot(Age_08_04,Price)
plot(KM,Price)
plot(HP,Price)
plot(cc,Price)
plot(Doors,Price)
plot(Gears,Price)
plot(Quarterly_Tax,log(Price))
plot(log(Weight),log(Price))
pairs(Corolla) 
cor(Corolla)
#######Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
####PARTIAL CO_EFFICIENTS####
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla))
###MODEL with all features###
model.Corolla <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model.Corolla)
######Effect of CC feature ####
model.Corollacc <- lm(Price~cc)
summary(model.Corollacc)
######Effect of Door feature ####
model.Corolladoor <- lm(Price~Doors)
summary(model.Corolladoor)
############
model.Corolladoorandcc <- lm(Price~cc+Doors)
summary(model.Corolladoorandcc)
library(car)
# Deletion Diagnostics for identifying influential variable
influence.measures(model.Corolla)
infIndexPlot(model.Corolla,id.n=3) # Index Plots of the influence measures
influencePlot(model.Corolla,id.n=3)# A user friendly representation of the above
influencePlot

model.Corolla1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(model.Corolla1)

