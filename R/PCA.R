#Principle Components Analysis 
states=row.names(USArrests)
states
apply(USArrests,2,mean)
apply(USArrests,2,var)
pr.out=prcomp(USArrests, scale. = TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$center

pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
prvar=pr.out$sdev^2
prvar
pve=prvar/sum(prvar)
pve
#We see that the first principal component explains 62.0% of the variance
#in the data, the next principal component explains 24.7% of the variance,
#and so forth.
 plot(pve , xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
       type='b')
#####K-Means Clustering######
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4
km.out=kmeans(x,2,nstart=20)
km.out$cluster

plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
Results with K=2", xlab ="", ylab="", pch =20, cex =2)


set.seed(4)
km.out=kmeans(x,3,nstart = 20)
km.out$tot.withinss
