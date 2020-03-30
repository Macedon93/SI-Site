#Here we are programming best subset selection. 
#Best subset is a shrinkage/feature selection algorithm that 
#selects the optimum combination of features that infuence the 
#response variable. 

install.packages(leaps) 
library(ISLR)
#here we explore and see that there are NA values in salary feature vector. 
fix(Hitters)
names(Hitters)
attach(Hitters)
dim(Hitters)
sum(is.na(Hitters$Runs))
#Here we see that there are 59 Na values for salary, must remove, no salaries for these players. 
sum(is.na(Hitters$Salary))
Hitters= na.omit(Hitters)

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
reg.summary=summary(regfit.full)#set variable to summize names and find values of performance measures. 
names(reg.summary)
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of variables",ylab = "RSS",type = "l")
plot(reg.summary$adjr2, xlab="Number of variables",ylab = "Rsq",type = "l")
plot(reg.summary$bic, xlab="Number of variables",ylab = "BIC",type = "l")
plot(reg.summary$cp, xlab="Number of variables",ylab = "cp",type = "l")

which.min(reg.summary$rss)
which.max(reg.summary$rsq)
which.min(reg.summary$bic)
which.min(reg.summary$cp)
points(8,reg.summary$rss[8],col="red",cex=2,pch=20)

plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")

coef(regfit.full,6)

#We can now use regsubsets() function to perform forward stepwise or backward stepwise selection, using the
#argument method="forward" or method="backward"
regfit.fwd=regsubsets(Salary~.,data = Hitters,nvmax = 19,method = "forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,data = Hitters,nvmax = 19,method = "backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
#all of the above code has used the full training set. Bad,
#creates and underestimation in test error, monotone decrease in training MSe, whilst u shape decrease in testMSE

#--------------------------------------------------------------------------------------#
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)#set training set
test=(!train)#test set for what is not in train 


regfit.best=regsubsets(Salary~., data = Hitters[train,],nvmax = 19)#fit model on specified train data
test.mat=model.matrix(Salary~.,data = Hitters[test,])#test.matrix is the !train data

#function for loop for each 
val.errors=rep(NA,19)
for (i in 1:19) {
  coefi=coef(regfit.best,id=i) #set coeficient to variable
  pred=test.mat[,names(coefi)]%*%coefi #prediction = test set matrix 
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
min(val.errors)
which.min(val.errors)
coef(regfit.best,7)

#write function that does the predictions shown above^
predict.regsubsets =function (object ,newdata ,id ,...){
   form=as.formula (object$call [[2]])
   mat=model.matrix (form ,newdata )
   coefi =coef(object ,id=id)
   xvars =names (coefi )
   mat[,xvars ]%*% coefi
   }

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))
for (j in 1:k) {
  best.fit=regsubsets(Salary~.,data = Hitters[folds!=j,],nvmax = 19)
  for(i in 1:19) {
     pred=predict (best.fit ,Hitters [folds ==j,], id=i)
     cv.errors [j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
     }
   }
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type = 'b')

#now perfrom best subset on full model and get coefficients from 11 variable model. 
reg.best=regsubsets(Salary~.,data = Hitters,nvmax = 19)
coef(reg.best,11)#best model via cv and best subset. 

