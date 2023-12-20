# Exercise.

# Please install sandwich package and execute the following code in R :
data(Investment, package="sandwich")
Investment <- as.data.frame(Investment)

# Investments in the USA, an annual time series from 1963 to 1982 with 7 variables. 
# Please investigate factors  related to investments in the USA, create visualization using lattice package.

Investment

# compare price and interest
library(lattice)
xyplot(Price ~ Interest, data = Investment, col = "blue",
       type="o", pch=16, lty=4, 
       main="Price vs Interest")

# correlation between realGNP and realInv
library(lattice)
a<-data.frame(Investment$RealGNP,Investment$RealInv)
corMatrix <- cor(a)
levelplot(corMatrix, xlab = " ", ylab = " ", main = "Correlation Plot")
trellis.par.set(panel.grid = panel.grid(lty = 1, lwd = 0.5))

#	relation between interest and inv & realInv 
library(latticeExtra)
a<-xyplot(Investment~Interest,data = Investment, pch=3, type="b")
b<-xyplot(RealInv~Interest,data = Investment,pch=3 , type="b")
c<-c(a, b, layout = 1:2)
update(c, scales = list(y = list(rot = 0)), ylab = c("Investment", "Real Investment"),xlab="Interest")


# compare factors which consider price index from 1963 to 1982
library(latticeExtra)
Investment1 <- data.frame(Years=c(1963:1982),Investment)
a<-xyplot(RealGNP~Years, Investment1, pch=1)
b<-xyplot(RealInv~Years, Investment1, pch=3)
c<-xyplot(RealInt~Years, Investment1, pch=4)
d<-xyplot(Price~Years, Investment1, pch=4)
graph<-c(a, b, c, d, layout=c(1, 4))
update(graph, scales = list(y = list(rot = 0)), ylab = c("RelGNP", "RelInv","RelInt","Price"),
       panel=function(x, y) {
         panel.loess(x, y)
         panel.xyplot(x, y)
       })



       
