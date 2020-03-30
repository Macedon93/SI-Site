#Dec. Trees p324
library(tree)
library(rpart.plot)
# Here well show the use of classification trees to analyze carseat data set. Sales
# is continous, so we begin by recording it as a binary. 
attach(Carseats)
High= ifelse(Sales<=8,"No","Yes") #variable exceeds 8 its considered high. 
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
#First branch differentiates Good locations from bad locations, this tree shows
#that the most importantn indicator of sales appears to be shelving location. 
#CV > set.seed (2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
tree.carseats =tree(High~.-Sales ,Carseats ,subset =train )
tree.pred=predict(tree.carseats ,Carseats.test ,type ="class")
table(tree.pred ,High.test)
#next we consider pruning the tree to improve the results
set.seed (3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )
cv.carseats
#lets plot the error as a function of size and k now. 
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type = "b")
plot(cv.carseats$k,cv.carseats$dev,type = "b")

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
