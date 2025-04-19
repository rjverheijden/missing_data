original <- data.frame(id=c(1:5),
                       sex=c("m", "m", "m", "f", "f"),
                       LDL = c(1.5,2.0,3.5,2.0,3.0),
                       weight = c(70,90,80,65,70),
                       length = c(1.85,1.90,1.75,1.70,1.65),
                       bmi = c(20.5,24.9,26.1,22.5,25.7))
original$sex<-factor(original$sex)

missings <- original
missings[c(1,3),2]<- NA
missings[c(3,4), c(4,6)] <- NA

formula = formula(~LDL+length)
model.frame(as.formula(formula), data = missings[c(3,4),], na.action = na.pass)

library(mice)
library(ggmice)
imp.0 <- mice(missings, maxit = 0)
imp.0$loggedEvents
imp.0$method

pred <- imp.0$predictorMatrix
pred[,]<-0
pred["id",]<-0
pred[,"id"]<-0
pred[c("weight", "length"), "bmi"]<-0
pred["sex", c("LDL", "weight", "length", "bmi")]<-1
pred["LDL", c("sex", "weight", "length", "bmi")]<-1
pred["weight", c("sex", "LDL", "length")]<-1
pred["length", c("sex", "LDL", "weight")]<-1
pred["bmi", c("weight", "length")]<-1

plot_pred(imp.0$predictorMatrix)
plot_pred(pred)
