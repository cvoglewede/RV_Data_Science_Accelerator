swirl()
Connor
1
6
2
head(airquality)
xyplot(Ozone~Wind,data=airquality,pch=8,col="red",main="Big Apple Data")
xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1))

p <- xyplot(Ozone~Wind,data=airquality)
p
names(p)
mynames[myfull]
p[["formula"]]
p[["x.limits"]]
table(f)
xyplot(y~x|f,layout=c(2,1))
v1
v2
myedit("plot1.R")
2
3
2
1
1

source(pathtofile("plot2.R"),local = TRUE)

str(diamonds)
table(diamonds$color)
table(diamonds$color,diamonds$cut)
3
myedit("myLabels.R")


source(pathtofile("myLabels.R"),local = TRUE)
xyplot(price~carat|color*cut,data=diamonds,pch=20,xlab=myxlab,ylab=myylab,main=mymain)
2
2
1
1
2
1
2
2
1
2
1
2
1
cvoglewede@redventures.com
exItrCLkxy2IqnAS
1
7

2
sample(colors(),10)
pal <- colorRamp(c("red","blue"))
pal(0)
3
pal(1)
1
1
3
pal(seq(0,1,len=6))
p1 <- colorRampPalette(c("red","blue"))
p1(2)
p1(6)
0xcc
p2 <- colorRampPalette(c("red","yellow"))
p2(2)
p2(10)
showMe(p1(20))
showMe(p2(20))
showMe(p2(2))
p1
?fun
?fun(rgb)
?rgb
p3 <- colorRampPalette(c("blue","green"),alpha=.5)
p3(5)
plot(x,y,pch=19,col=rgb(0,.5,.5,alpha=.3))
cols <- brewer.pal(3,"BuGn")
showMe(cols)
pal <- colorRampPalette(cols)
showMe(pal(3))
showMe(pal(20))
image(volcano,col=p1(20))
2
3
2
3
3
``
1
2
1
1
4
2
cvoglewede@redventures.com

str(mpg)
qplot(displ,hwy,data=mpg,color=drv,geom=c("point","smooth"))
qplot(y=hwy,data=mpg,color=drv)
myhigh
qplot(drv,hwy,data=mpg,geom="boxplot",color=manufacturer)
qplot(hwy,data=mpg,fill=drv)
qplot(displ,hwy,data=mpg,facets=.~drv)
qplot(hwy,data=mpg,facets=drv~.,binwidth=2)
4
3
3
3
3
1
2
1
1
2
cvoglewede@redventures.com
ZexNrg9M7DWqSsE6
1
9
1
3
3
4
qplot(displ,hwy,data=mpg,geom=c("point","smooth"),facets=.~drv)
g <- ggplot(mpg, aes(displ,hwy))
g
summary(g)
g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)+ggtitle("Swirl Rules!")
g+geom_point(color="pink",size=4,alpha=.5)
g+geom_point(aes(color=drv),size=4,alpha=.5)
g+geom_point(aes(color=drv))+labs(title = "Swirl Rules!")+labs(x="Displacement",y="Hwy Mileage")
g+geom_point(aes(color=drv),size=2,alpha=.5)+geom_smooth(size=4,linetype=3,method="lm",se=FALSE)
g+geom_point(aes(color=drv))+theme_bw(base_family = "Times")
plot(myx,myy,type="l",ylim = c(-3,3))
g <- ggplot(testdat,aes(x=myx,y=myy))
g+geom_line()+coord_cartesian(ylim=c(-3,3))
g <- ggplot(mpg,aes(x=displ,y=hwy,color=factor(year)))
3
g+geom_point()+facet_grid(drv~cyl,margins=TRUE)+geom_smooth(method="lm",size=2,se=FALSE,color="black")+labs(x="Displacement",y="Highway Mileage",title = "Swirl Rules!")            
1
cvoglewede@redventures.com
F27oItWqmViQpxrG
1
10
str(diamonds)
2
4
qplot(price,data=diamonds)
range(diamonds$price)
qplot(price,data=diamonds,binwidth=18497/30)
brk
counts
qplot(price,data=diamonds,binwidth=18497/30,fill=cut)
qplot(price,data=diamonds,geom="density",color=cut)
qplot(carat,price,data=diamonds,color=cut,facets = .~cut)+geom_smooth(method="lm")
3
3
1
1
2
1
g <- ggplot(diamonds,aes(depth,price))
summary(g)
g+geom_point(alpha=1/3)
cutpoints <- quantile(diamonds$carat,seq(0,1,length=4),na.rm = TRUE)
cutpoints
diamonds$car2 <- cut(diamonds$carat,cutpoints)
g <- ggplot(diamonds,aes(depth,price))
g+geom_point(alpha=1/3)+facet_grid(cut~car2)+geom_smooth(method="lm",size=3,color="pink")
diamonds[myd,]
ggplot(diamonds,aes(carat,price))+geom_boxplot()+facet_grid(.~cut)
2
cvoglewede@redventures.com
kCXBqIYNpFQb5gYR
quit()
0
