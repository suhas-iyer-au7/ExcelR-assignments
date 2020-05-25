Startups <- read.csv("C:/Users/suhas/Downloads/50_Startups.csv")
  View(Startups)
  attach(Startups)
  summary(Startups)
  Startups$State <- as.numeric(Startups$State) - 1  
head(Startups) 

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
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
cor(Startups)
cor2pcor(cor(Startups))


model.Startups <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State) # lm(Y ~ X)
summary(model.Startups)


model.Startupsm <- lm(Profit~Marketing.Spend) # lm(Y ~ X)
summary(model.Startupsm)
# Diagnostic Plots
#install.packages(car)
library(car)
View(car)

plot(model.Startups)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

#qqplot(model.car)# QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.Startups)
influenceIndexPlot(model.Startups,id.n=3) # Index Plots of the influence measures
influencePlot(model.Startups,id.n=3)# A user friendly representation of the above
influencePlot

## Regression after deleting the 50th observation
model.Startups1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = Startups[-50])
summary(model.Startups1)
