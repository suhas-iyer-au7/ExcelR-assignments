Computer_Data <- read.csv("C:/Users/suhas/Downloads/Computer_Data.csv")
View(Computer_Data)
Computer_Data <- within(Computer_Data, rm(X))
attach(Computer_Data)
summary(Computer_Data)
Computer_Data$cd <- as.numeric(Computer_Data$cd) - 1
head(cd)
Computer_Data$multi <- as.numeric(Computer_Data$multi) - 1
Computer_Data$premium<- as.numeric(Computer_Data$premium) - 1
pairs(Computer_Data)
cor(Computer_Data)
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
pairs(Computer_Data, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
model.Computer_Data <- lm(price ~ speed + hd + ram + screen + cd + multi + premium +ads + trend)
summary(model.Computer_Data)
model.Computer_Data1 <- lm(price ~ log(speed) + log(hd) + ram + log(screen) + cd + (multi) + premium +ads +trend)
summary(model.Computer_Data1)
