set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+.05,y+.05,labels=as.character(1:12))

dataFrame <- data.frame(x=x,y=y)
distDF <- dist(dataFrame)

clusterDF <- hclust(distDF)
plot(clusterDF)



myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}


myplclust(clusterDF,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

dataFrameK <- data.frame(x,y)
kmeansObj <- kmeans(dataFrameK,centers = 3)
names(kmeansObj)

par(mar=rep(.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,lwd=3)


set.seed(12345)
par(mar=rep(.2,4))
dataPCA <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,z=as.matrix(t(dataPCA)[,nrow(dataPCA):1]))



par(mar=rep(.2,4))
heatmap(dataPCA)


set.seed(678910)
for (i in 1:40){
  coinflip1 <- rbinom(n=1,size=1,prob=.5)
  coinflip2 <- rbinom(n=1,size=1,prob=.5)
  if (coinflip1) {
    dataPCA[i,] <- dataPCA[i,]+rep(c(0,3),each=5)
  }
  if (coinflip2) {
    dataPCA[i,] <- dataPCA[i,]+rep(c(0,3),5)
  }
}
hh <- hclust(dist(dataPCA))
dataPCAOrdered <- dataPCA[hh$order,]

svd2 <- svd(scale(dataPCAOrdered))
par(mfrow=c(1,3))
image(t(dataPCAOrdered)[,nrow(dataPCAOrdered):1])
plot(rep(c(0,1),each=5),pch=19)
plot(rep(c(0,1),5),pch=19)


par(mfrow=c(1,3))
image(t(dataPCAOrdered)[,nrow(dataPCAOrdered):1])
plot(svd2$v[,1],pch=19)
plot(svd2$v[,2],pch=19)

par(mfrow=c(1,2))
plot(svd2$d,pch=19)
plot(svd2$d^2/sum(svd2$d^2),pch=19)

install.packages("BiocManager")
BiocManager::install("impute", version = "3.8")
library(impute)

dataMatrixI <- dataPCAOrdered
dataMatrixI[sample(1:100,size=40,replace=FALSE)] <- NA
dataMatrixI <- impute.knn(dataMatrixI)$data
svd1 <- svd(scale(dataPCAOrdered))
svd2 <- svd(scale(dataMatrixI))
