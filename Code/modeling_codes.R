#Codes for modelling of STAT 628 Module 1 Group 4


# read data and clean data
rawData <- read.csv("../Data/BodyFat.csv" , header = T)
rawData$DENSITY[96] = 1.0592
rawData$HEIGHT[42] = 69.428
rawData <- rawData[-c(39,182),]
y <- rawData$BODYFAT
z <- rawData$DENSITY
x <- rawData[,4:17]
#This cleaned data is written as "BodyFat_cleaned.csv" in our data folder.


#stepwise feature selection
step(lm(y~.,data=x),direction="both", criterion="aic")
step(lm(y~.,data=x),direction="both", criterion="bic")


#Three functions are written in the following three R files. 
source("bootstrap.R") #This is a function to get a bootstrap list.
source("sort_feature.R") # This is a function to sort features by importance.
source("cal_score.R") #This is a function to calculate the score of models using mean square error.


# random feature selection
f <- sort_feature(y,x)
print(f)

print(round(cal_score(y , x[,c("ABDOMEN" , "AGE" , "CHEST" , "ADIPOSITY" , 
                               "WRIST" , "HEIGHT")]), 2))
summary(lm(y~., data = x[,c("ABDOMEN" , "AGE" , "CHEST" , "ADIPOSITY" , 
                               "WRIST" , "HEIGHT")]))

print(round(cal_score(y , x[,c("ABDOMEN" , "AGE" , "CHEST" , 
                               "WRIST" , "HEIGHT")]), 2))
summary(lm(y~., data = x[,c("ABDOMEN" , "AGE" , "CHEST" , 
                            "WRIST" , "HEIGHT")]))

print(round(cal_score(y , x[,c("ABDOMEN" , "AGE" , "WRIST" , "HEIGHT")]), 2))
summary(lm(y~., data = x[,c("ABDOMEN" , "AGE" , "WRIST" , "HEIGHT")]))

print(round(cal_score(y , x[,c("ABDOMEN" , "WRIST" , "HEIGHT")]), 2))
summary(lm(y~., data = x[,c("ABDOMEN" , "WRIST" , "HEIGHT")]))

print(round(cal_score(y , x[,c("ABDOMEN" , "AGE" , "WRIST")]), 2))
summary(lm(y~., data = x[,c("ABDOMEN" , "AGE" , "WRIST")]))

print(round(cal_score(y , x[,c("ABDOMEN" , "AGE" , "WRIST")] , intercept = F), 2))
summary(lm(y~-1+., data = x[,c("ABDOMEN" , "AGE" , "WRIST")]))


# generating feature
newx <- x[,c(2,5:14)]/x$HEIGHT
newx$ADIPOSITY <- x$ADIPOSITY
newx$AGE <- x$AGE

f2 <- featureWeight(y,newx)
print(f2)

print(round(cal_score(y , newx[,c("ABDOMEN") , drop = F]), 2))
summary(lm(y~., data = newx[,c("ABDOMEN") , drop = F]))

print(round(cal_score(y , newx[,c("ABDOMEN" , "ADIPOSITY")]), 2))
summary(lm(y~., data = newx[,c("ABDOMEN" , "ADIPOSITY")]))

print(round(cal_score(y , newx[,c("ABDOMEN" , "AGE")]), 2))
summary(lm(y~., data = newx[,c("ABDOMEN" , "AGE")]))

print(round(cal_score(y , newx[,c("ABDOMEN" , "WRIST")]), 2))
summary(lm(y~., data = newx[,c("ABDOMEN" , "WRIST")]))

print(round(cal_score(y , newx[,c("ABDOMEN" , "CHEST")]), 2))
summary(lm(y~., data = newx[,c("ABDOMEN" , "CHEST")]))

print(round(cal_score(y , as.data.frame(newx$ABDOMEN - newx$WRIST)), 2))
summary(lm(y~., data = as.data.frame(newx$ABDOMEN - newx$WRIST)))

print(round(cal_score(y , as.data.frame(newx$ABDOMEN - newx$NECK)), 2))
summary(lm(y~., data = as.data.frame(newx$ABDOMEN - newx$NECK)))


#We chose the following AAW model as our final model.
lm_AAW <- lm(y~-1+., data = x[,c("ABDOMEN" , "AGE" , "WRIST")])


#Dignosis Plots for final model
plot(lm_AAW, pch=23, bg="darkorange", which=1, cex=0.9)
plot(lm_AAW, pch=23, bg="darkorange", which=2, cex=0.9)
plot(lm_AAW, pch=23, bg="darkorange", which=4, cex=0.9)

