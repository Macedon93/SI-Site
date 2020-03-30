#LogReg,KNN comparison

library(MASS)
library(ISLR)

summary(Smarket)
cor(Smarket[,-9])# correlation of variables with one another, qualitative feature removed. 
attach(Smarket)#allow for variable selection. 
plot(Year)#Lines 9-10 some descriptive plots. 
plot(Volume)
abline(Volume)


glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)#fitting the logistic regression with 6 predictors. 
summary(glm.fits)
coef(glm.fits)

#Below here we are looking at setting up the predictions 
#of direction, using our fit. 
glm.probs=predict(glm.fits,type = "response")#prediciting direction
glm.probs[1:10]
# 
contrasts(Direction)
#for the prediction we are assessing the probabilities of
#whether they will go down or up. 
glm.predict=rep("Down",1250)
glm.predict[glm.probs>.5]="Up"
table(glm.predict,Direction)

mean(glm.predict==Direction)
#Here we train on only a portion of the data and seperate a test 
# set (2005) observations. This will give a better approximation of 
#test error. 
train=(Year<2005)
Smarket.2005=Smarket[!train,]#set variable to whats not in train. 
dim(Smarket.2005)
Direction.2005=Direction[!train]
#now we train and test on the two different sets. 
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket ,family =binomial ,subset =train )#subset is new here. saying to fit only the training data. 
glm.probs =predict (glm.fits,Smarket.2005 , type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#now use only lag1 and lag2 in the prediction(Lowest pvalue)
glm.fits=glm(Direction~Lag1+Lag2, data = Smarket,family = binomial,subset = train)
glm.probs=predict(glm.fits,Smarket.2005,type = "response")
glm.pred=rep("Down",252)#convert the predicted probabilities into class labels. 
glm.pred[glm.probs>.5]="Up"#convert the predicted probabilities into class labels.
table(glm.pred,Direction.2005)
mean(glm.pred==direction.2005)

#predict Direction on a day when Lag1 and Lag2 equal 1.2 and 1.1, respectively,
#and on a day when they equal 1.5 and 0.8.

predict(glm.fits,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = "response")
#KNN
library(MASS)
library(ISLR)
library(class)
attach(Smarket)
set.seed (1)
knn.pred=knn (train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)