swirl()
Connor
1
11
1
2
dist(dataFrame)
1
4
hc <- hclust(distxy)
plot(as.dendrogram(hc))
abline(h=1.5,col="blue")
abline(h=.4,col="red")
5
12
abline(h=.05,col="green")
dist(dFsm)
hc
2
heatmap(dataMatrix,col=cm.colors(25))
heatmap(mt)

mt
plot(denmt)
distmt
1
2
2
1
2
2
1
1
cvoglewede@redventures.com
mrdEhh2dK9JvY7I8


1
12
3
3
1
cmat
points(cx,cy,col=c("red","orange","purple"),pch=3,cex=2,lwd=2)
1
mdist(x,y,cx,cy)
1
3
apply(distTmp,2,which.min)
points(x,y,pch=19,cex=2,col=cols1[newClust])
x
tapply(x,newClust,mean)
tapply(y,newClust,mean)
points(newCx,newCy,col=cols1,pch=8,cex=2,lwd=2)
mdist(x,y,newCx,newCy)
2
apply(distTmp2,2,which.min)
points(x,y,pch=19,cex=2,col=cols1[newClust2])
tapply(x,newClust2,mean)
tapply(y,newClust2,mean)
points(finalCx,finalCy,col=cols1,pch=9,cex=2,lwd=2)
kmeans(dataFrame,centers=3)
kmObj$iter

plot(x,y,col=kmObj$cluster,pch=19,cex=2)
points(kmObj$centers,col=c("black","red","green"),pch=3,cex=3,lwd=3)
plot(x,y,col=kmeans(dataFrame,6)$cluster,pch=19,cex=2)
1
1
1
1
1
1
cvoglewede@redventures.com
BQFRROi5o3n7yamN


head(dataMatrix)
heatmap(dataMatrix)
myedit("addPatt.R")
source("addPatt.R",local=TRUE)
heatmap(dataMatrix)
1
mat
svd(mat)
matu %*% diag %*% t(matv)
svd(scale(mat))
prcomp(scale(mat))
svd1$v[,1]
svd1$d
head(constantMatrix)
svd2$d
1
2
svd2$v[,1:2]
svd2$d
1
dim(faceData)
3
a1 <- svd1$u[,1] %*% t(svd1$v[,1])*svd1$d[1]
myImage(a1)
a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
myImage(a2)
myImage(svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]))
myImage(svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))
2
1        
4
1
2
1
1
cvoglewede@redventures.com
W9jsxR6Ipj57fgLK
1
14
dim(ssd)
names(ssd[,c(562,563)])
table(ssd$subject)
2
1
sum(table(ssd$subject))
table(ssd$activity)
2
sub1 <- subset(ssd,subject==1)
sub1
dim(sub1)
names(sub1[,1:12])
myedit("showXY.R")
showMe(1:6)
mdist <- dist(sub1[,1:3])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col=unclass(sub1$activity))
2
mdist <- dist(sub1[,10:12])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col = unclass(sub1$activity))
svd1 <- svd(scale(sub1[,-c(562,563)]))
1
3
dim(svd1$u)
maxCon <- which.max(svd1$v[,2])
mdist <- dist(sub1[,c(10:12,maxCon)])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col=unclass(sub1$activity))
names(sub1[maxCon])
kClust <- kmeans(sub1[,-c(562,563)],centers = 6)
table(kClust$cluster,sub1$activity)
kClust <- kmeans(sub1[,-c(562,563)],centers = 6,nstart = 100)
table(kClust$cluster,sub1$activity)
dim(kClust$centers)
laying <- which(kClust$size==29)
plot(kClust$centers[laying,1:12],pch=19,ylab="Laying Cluster")
names(sub1[,1:3])
walkdown <- which(kClust$size==49)
plot(kClust$centers[walkdown,1:12],pch=19,ylab="Walkdown Cluster")
1
cvoglewede@redventures.com
S96f6bOChqYtu9XE
