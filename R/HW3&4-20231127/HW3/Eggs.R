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

# Egg.pr & Egg selling cases
a1<-data.frame(Eggs$Egg.Pr,Eggs$Cases)
b1<-cor(a1)
corrplot(b1, method="number")


# Egg pr & Beef pr & Chicken pr & Pork pr & Cereal pr 
a2<-data.frame(Eggs$Egg.Pr,Eggs$Beef.Pr,Eggs$Pork.Pr,Eggs$Chicken.Pr,Eggs$Cereal.Pr)
b2<-cor(a2)
corrplot(b2, method="pie")

