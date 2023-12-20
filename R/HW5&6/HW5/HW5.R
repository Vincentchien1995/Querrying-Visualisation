#Exercise 1
#program1
par(mai = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
xx <- c(9.20, 6.00, 6.00, 11.25, 11.00, 7.25, 9.7, 13.25, 14.00, 8.00)
hist(xx, breaks = c(6, 8, 10, 12, 14), right = F)

#AN1
library(lattice)
xx <- c(9.20, 6.00, 6.00, 11.25, 11.00, 7.25, 9.7, 13.25, 14.00, 8.00)
histogram(~ xx, main="Histogram of xx", xlab="xx", ylab="Frequency",type="count",
          breaks = c(6, 8, 10, 12, 14),right = F,col="gray")

#program2
par(mai = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
set.seed(591)
xx1 <- rnorm(20, mean = 3, sd = 3.6)
xx2 <- rpois(40, lambda = 3.5)
xx3 <- rchisq(31, df = 5, ncp = 0)
box1 <- boxplot(xx1, xx2, xx3, names = c("Group-1", "Group-2",
                                         "Group-3"), cex = 0.7)

#AN2
library(lattice)
set.seed(591)
xx1 <- rnorm(20, mean = 3, sd = 3.6)
xx2 <- rpois(40, lambda = 3.5)
xx3 <- rchisq(31, df = 5, ncp = 0)
df1 <- data.frame(xx1, levels ="Group-1")
df2 <- data.frame(xx2, levels ="Group-2")
df3 <- data.frame(xx3, levels ="Group-3")
colnames(df1) <- c("Data","Group")
colnames(df2) <- c("Data","Group")
colnames(df3) <- c("Data","Group")
df <- bind_rows(df1,df2,df3)
bwplot(Data~Group,df)

#Exercise 2
install.packages("car")
library(car)
install.packages("corrplot")
library(corrplot)

Eggs<-read.csv("http://jolej.linuxpl.info/Eggs.csv", header=TRUE)
data.frame(Eggs)

Eggs$First.Week <-as.factor(Eggs$First.Week)
Eggs$Month<-as.factor(Eggs$Month)
Eggs$Easter<-as.factor(Eggs$Easter)

summary(Eggs)
print(Eggs)
#graph1
a1<-data.frame(Eggs$Egg.Pr,Eggs$Cases)
b1<-cor(a1)
corrplot(b1, method="number")

#AN
Eggs<-read.csv("http://jolej.linuxpl.info/Eggs.csv", header=TRUE)
library(lattice)
a1<-data.frame(Eggs$Egg.Pr,Eggs$Cases)
b1<-cor(a1)
corMatrix <- cor(a1)
levelplot(corMatrix, xlab = " ", ylab = " ", main = "Correlation Plot")
trellis.par.set(panel.grid = panel.grid(lty = 1, lwd = 0.5))


#graph2
a2<-data.frame(Eggs$Egg.Pr,Eggs$Beef.Pr,Eggs$Pork.Pr,Eggs$Chicken.Pr,Eggs$Cereal.Pr)
b2<-cor(a2)
corrplot(b2, method="pie")

#AN
Eggs<-read.csv("http://jolej.linuxpl.info/Eggs.csv", header=TRUE)
library(lattice)
a2<-data.frame(Eggs$Egg.Pr,Eggs$Beef.Pr,Eggs$Pork.Pr,Eggs$Chicken.Pr,Eggs$Cereal.Pr)
b2<-cor(a2)
corMatrix <- cor(b2)
levelplot(corMatrix, xlab = " ", ylab = " ", main = "Correlation Plot")
trellis.par.set(panel.grid = panel.grid(lty = 1, lwd = 0.5))


