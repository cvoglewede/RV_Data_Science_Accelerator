# Exploratory Data Analysis Swirl

library(swirl)
swirl()
Connor
1
1
1
3
4
1
4
1
3
2
1
1
3
1
1
4
1
2
cvoglewede@redventures.com
3ZOnRvNSWR9pLYAP
1
2
4
4
head(pollution)
dim(pollution)
summary(pollution$pm25)
3
quantile(ppm)
3
boxplot(ppm,col="blue")
1
abline(h=12)
1
hist(ppm,col="green")
3
rug(ppm)
low
high
hist(ppm,col="green",breaks = 100)
3
rug(ppm)
hist(ppm,col="green")
abline(v=12,lwd=2)
abline(v=median(ppm),col="magenta",lwd=4)
names(pollution)
reg <- table(pollution$region)
reg
barplot(reg,col="wheat",main="Number of Counties in Each Region")
1
boxplot(pm25~region,data=pollution,col="red")

par(mfrow=c(2,1),mar=c(4,4,2,1))
east <- subset(pollution,region=="east")
head(east)
hist(east$pm25,col="green")
hist(subset(pollution,region=="west")$pm25,col="green")


with(pollution,plot(latitude,pm25))
abline(h=12,lwd=2,lty=2)
plot(pollution$latitude,ppm,col=pollution$region)
1
abline(h=12,lwd=2,lty=2)
par(mfrow = c(1, 2),mar = c(5, 4, 2, 1))


west <- subset(pollution,region=="west")

plot(west$latitude,west$pm25,main="West")

plot(east$latitude,east$pm25,main="East")
4
1
1
2
cvoglewede@redventures.com
W6E84wITUt80lkXG


1
3
?Devices
with(faithful,plot(eruptions,waiting))
title("Old Faithful Geyser data")
dev.cur()
pdf(file="myplot.pdf")
with(faithful,plot(eruptions,waiting))
title("Old Faithful Geyser data")
dev.cur()
dev.off()
dev.cur()
with(faithful,plot(eruptions,waiting))
title("Old Faithful Geyser data")
dev.copy(png,file="geyserplot.png")
dev.off()
1
cvoglewede@redventures.com
j5P63xTNk0k5QLCA
1
4
2
2
head(cars)
with(cars,plot(speed,dist))
text(mean(cars$speed),max(cars$dist),"SWIRL rules!")
head(state)
table(state$region)
xyplot(Life.Exp~Income|region,data=state,layout=c(4,1))
3
xyplot(Life.Exp~Income|region,data=state,layout=c(2,2))
head(mpg)
dim(mpg)
table(mpg$model)

qplot(displ,hwy,data=mpg)
1
2
2
2
2
1
4
4
1
cvoglewede@redventures.com
0ujHeBJOxu4gXwq6
1
5
1
4
head(airquality)
range(airquality$Ozone,na.rm = TRUE)
hist(airquality$Ozone)
2
1
1
4
4
dev.cur()
dev.off()
dev.cur()
table(airquality$Month)
boxplot(Ozone~Month,data=airquality)
boxplot(Ozone~Month,data=airquality,xlab="Month",ylab="Ozone (ppb)",col.axis="blue",col.lab="red")
title("Ozone and Wind in New York City")


with(airquality,plot(Wind,Ozone))
length(par())
names(par())
par()$pin
3
par("fg")
1
1
par("pch")
3
par("lty")
1
5
2
plot(airquality$Wind,type="n",airquality$Ozone)
title("Wind and Ozone in NYC")
may <- subset(airquality,Month==5)

points(may$Wind,may$Ozone,col="blue",pch=17)

notmay <- subset(airquality,Month!=5)

points(notmay$Wind,notmay$Ozone,col="red",pch=8)

legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("May","Other Months"))
abline(v=median(airquality$Wind),lty=2,lwd=2)


par(mfrow=c(1,2))
plot(airquality$Wind,airquality$Ozone,main="Ozone and Wind")
plot(airquality$Solar.R,airquality$Ozone,main="Ozone and Solar Radiation")
plot(airquality$Temp,airquality$Ozone,main="Ozone and Temperature")
mtext("Ozone and Weather in New York City",outer = TRUE)
1
cvoglewede@redventures.com
AaWoIkLPE2laUDuT
