#Importing data 
reqD<- read.csv("scores.csv")
str(reqD)

#Splitting data
library(caTools)
set.seed(54178)
split<- sample.split(reqD$Scores,SplitRatio = 3/4)
trainS<-subset(reqD,split==TRUE)
testS<-subset(reqD,split==FALSE)
head(trainS)
head(testS)

#ML model using training values
ModeL<- lm(Scores~Hours,trainS)
summary(ModeL)

#Testing the model using test set
NewPred <- predict(ModeL,newdata = testS)
Comparison <- as.data.frame(NewPred)
Comparison$ActualMarks<-testS$Scores

#Plotting the predicted vs actual
plot(Comparison$NewPred,type= "l",lty=1.8, col="red")
lines(Comparison$ActualMarks,type = "l",lty=1.8,col="blue")

#Predicting the required value
predict(ModeL,newdata = data.frame("Hours"=9.25))
#93.02705

#Plotting linear regression of training set
library(ggplot2)
library(scales)

ggplot() +
  geom_point(aes(x=trainS$Hours, y=trainS$Scores),
             colour = 'red') +
  geom_line (aes( x= trainS$Hours, y=predict(ModeL, newdata= trainS)),
             colour = 'navy') +
  ggtitle ('Hours Spent on study vs Marks scored (Training Set)') +
  xlab ('Hours spent on study') +
  ylab ('Marks scored') +
  scale_x_continuous(limits = c(0, 10)) + 
  scale_y_continuous(limits = c(0, 100)) 

# Visualising the Test set results
ggplot() +
  geom_point(aes(x=testS$Hours, y=testS$Scores),
             colour = 'red') +
  geom_line (aes( x= testS$Hours, y=predict(ModeL, newdata= testS)),
             colour = 'navy') +
  ggtitle ('Hours Spent on study vs Marks scored (Testing Set)') +
  xlab ('Hours spent on study') +
  ylab ('Marks scored') +
  scale_x_continuous(limits = c(0, 10)) + 
  scale_y_continuous(limits = c(0, 100))  