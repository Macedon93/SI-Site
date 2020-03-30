#Lasso and Ridge Regression 
library(MASS)
library(ISLR)
library(glmnet)

x=model.matrix (Salary~.,Hitters )[,-1]
y=Hitters$Salary
#alpha=0, ridge is performed, lasso if alpha=1

#grid=changing lambda range values for ridge regression.
grid=10^seq(10,-2,length=100)
#glmnet automatically standardizes variables
ridge.mod=glmnet(x,y,alpha = 0,lambda = grid)
dim(coef(ridge.mod))

ridge.mod$lambda[60]
#Coefficients when lambda = 705
coef(ridge.mod)[,60]
#we can use the predict function to check coef for when lambda=20 or 
#any sized value. 
predict(ridge.mod,s=50,type = "coefficients")[1:20,]
#time for setting up our training sets.
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]
#time to fit
ridge.mod=glmnet(x[train,],y[train],alpha = 0,lambda = grid,thresh = 1e-12)
#lets evaluate our model with tuning parameter at an arbitraty #, lets say 4.
ridge.pred=predict(ridge.mod,s=4,newx = x[test,])
mean((ridge.pred-y.test)^2)
#lets compare this to if we jsust fit the model with just an intercept
mean((mean(y[train])-y.test)^2)
#------------------------------------------------------#
#Use of crossvalidation to find the best value of lambda
#------------------------------------------------------#
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
best.lam=cv.out$lambda.min
best.lam
#lets do a prediction with this new lambda value. 
ridge.pred3=predict(ridge.mod,s=best.lam,newx=x[test,])
mean((ridge.pred3-y.test)^2)#lowest MSE

# lets now refit our ridge regression on the full dataset with this newly found parameter. 
out=glmnet(x,y,alpha=0)
predict(out,type = "coefficients",s=best.lam)[1:20,]
#as we can expect none of the new coefficients are 0, as ridge regression does not perform variable selection. 

#----------------------------------------------------------------#
#Lasso time! Lets try to make this model more interpretable. 
#----------------------------------------------------------------#

lasso.mod=glmnet(x[train,],y[train],alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
#results much better than least sqaures and is similiar to ridge
#however the model is much more interpretable as some features' coefficients have been set to 0.
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type = "coefficients",s=bestlam)[1:20,]
lasso.coef
#---PCR Regression---#
library(pls)
set.seed(2)
#model fit syntax similiar to lm() for least squares
pcr.fit=pcr(Salary~.,data=Hitters, scale=TRUE,
 validation="CV")

# validation cv makes pcr compute the 10 fold cv error for each M(number of Principle components).
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters, subset=train, scale=TRUE,
validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred=predict(pcr.fit,x[test,],ncomp = 7)
mean((pcr.pred-y.test)^2)
#Lower MSE than ridge and lasso, however at the cost of interpretibility. No theta estimates and 
#absolutely no kind of variable selection. 

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)
