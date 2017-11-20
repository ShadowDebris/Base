setwd("C:/Users/h/Desktop/可视化数据集/1数据集一：Wind数据")
mydata <- read.table("dailyprice.txt",header = T,sep = "\t",stringsAsFactors = FALSE)
library(sampling)
library(ggplot2)
library(car)
library(corrplot)
library(lubridate)
library(lattice)
library(plotly)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
sub1 <- sample(mydata$trade_code,10)
mydata0 = subset(mydata,trade_code == sub1)
mydata0 <- na.omit(mydata0)
str(mydata0)
mydata0$datetime <- as.Date(mydata0$datetime)
year <- year(mydata0$datetime)
month <- month(mydata0$datetime)
day <- day(mydata0$datetime)
mydata0 <- cbind(mydata0[,1:2],year,month,day,mydata0[,3:20])
mydata0$trade_status = factor(mydata0$trade_status)
mydata0$year=factor(mydata0$year)
mydata0$month=factor(mydata0$month)
mydata0$day=factor(mydata0$day)
#气泡图
p1 <- ggplot(mydata0,aes(x=year,y=volume,size=amt))+geom_point(shape=21,colour="black",fill="lightblue")+scale_size_area(max_size = 25)
p1
#密度图
p2 <- ggplot(mydata0,aes(x=open,y=close))+geom_point()
p2+stat_density2d()
#分面的散点图
p3 <- ggplot(mydata0,aes(x=volume,y=chg))+geom_point()
p3+facet_grid(mydata0$year~.)
#密度曲线
p4 <- ggplot(mydata0,aes(x=open,colour=trade_code))+geom_line(stat = "density")+expand_limits(y=0)
p4
#分组密度曲线
p5 <- ggplot(mydata0,aes(x=open,fill=trade_code))+geom_density(alpha=0.35,colour="white")
p5
ggplot(mydata0,aes(x=open,fill=trade_code))+geom_density()+facet_grid(trade_code~.)
#散点图矩阵
matrix <- mydata0[,c(3,4,5,6)]
matrix <- matrix[sample(nrow(matrix),50,replace=T),]
p6 <- scatterplotMatrix(matrix,mian="散点图矩阵")
#相关系数矩阵
cormatrix <- cor(mydata0[,-c(1,2,3,4,5,23)],use="complete.obs")
corrplot(cormatrix,method = "shade")
#热图
p7 <- ggplot(mydata0,aes(x=year,y=month,fill=amt/volume))
p7+geom_tile()
winlose <- subset(mydata0,select = c("open","volume","amt","turn","mkt_cap","trade_code"))
winlose <- winlose[sample(nrow(winlose),50,replace=T),]
winlose[,1:5]<- scale(winlose[,1:5])
distance_op <- dist(winlose[,-6],method = "euclidean")
distance_op <- as.matrix(distance_op)
p8 <- heatmap(distance_op,main="热图")
#平行坐标图
p9 <- parallelplot(~winlose[,1:5],winlose,groups = trade_code,horizontal.axis = F)
p9
parallelplot(~winlose[,1:5]|trade_code,winlose,horizontal.axis = F,scales = list(x=list(rot=90)))
#雷达图
tar <- mydata0[,c(6,7,8,9,2)]
tar <- tar[sample(nrow(tar),10,replace=T),]
tar[,1:4] <- scale(tar[,1:4])
p10 <- stars(tar[,1:4],scale=T,main = "Star(Spider/Radar Plot")
stars(tar[,1:4],locations = c(0,0),col.lines = 1:10,radius = F,scale = F,key.loc = c(0,0),lwd = 1.5)
legend(1,1.2,cex = 0.5,legend =tar$trade_code,col=c(1:10),lty=1)
#交互图
plot_ly(midwest,x=~percollege,color=~state,type = "box")
p11 <- ggplot(data=sub1,aes(x=open,fill=year))+geom_density(alpha=0.7)+facet_grid(trade_code~.)
p11
ggplotly(p11)
data("volcano")
p12=plot_ly(z=~volcano)
p13=add_surface(p12)
p13
#直方图与密度曲线图 
sub1 <- mydata0[mydata0$year %in% c(2011:2015),]
p14 <- ggplot(sub1,aes(x=open,fill=year))+geom_histogram(position = "identity",alpha=0.4)
p14
p15 <- ggplot(sub1,aes(x=open))+geom_histogram(aes(y=..density..),fill="blue",colour="white",alpha=0.3)+geom_density(colour="pink",size=1)
p15
#箱线图
p16 <- ggplot(sub1,aes(x=year,y=open))+geom_boxplot(outlier.color = "pink")+stat_summary(fun.y = "mean",geom = "point",shape=23,size=3,fill="white")
p16
p17  <- ggplot(sub1,aes(x=year,y=open))+geom_boxplot(outlier.colour = NA)+stat_summary(fun.y = "mean",geom = "point",shape=23,size=3,fill="white")
p17
#小提琴
p18 <- ggplot(sub1,aes(x=year,y=open))+geom_violin()+geom_boxplot(width=.1,fill="blue")+stat_summary(fun.y = median,geom = "point",shape=23,size=3,fill="white")
p18
p19 <- ggplot(sub1,aes(x=year,y=open))+geom_violin()+geom_boxplot(width=.1,fill="blue",outlier.colour = NA)+stat_summary(fun.y = median,geom = "point",shape=23,size=3,fill="white")
p19
#指数分布(plot)
set.seed(1)
x<-seq(-1,2,length.out=100)
y<-dexp(x,0.5)
plot(x,y,col="red",xlim=c(0,2),ylim=c(0,5),type='l',
     xaxs="i", yaxs="i",ylab='density',xlab='',
     main="The Exponential Density Distribution")
lines(x,dexp(x,1),col="green")
lines(x,dexp(x,2),col="blue")
lines(x,dexp(x,5),col="orange")
legend("topright",legend=paste("rate=",c(.5, 1, 2,5)), lwd=1,col=c("red", "green","blue","orange"))

set.seed(1)
x<-seq(-1,2,length.out=100)
y<-pexp(x,0.5)
plot(x,y,col="red",xlim=c(0,2),ylim=c(0,1),type='l',
     xaxs="i", yaxs="i",ylab='density',xlab='',
     main="The Exponential Cumulative Distribution Function")
lines(x,pexp(x,1),col="green")
lines(x,pexp(x,2),col="blue")
lines(x,pexp(x,5),col="orange")
legend("bottomright",legend=paste("rate=",c(.5, 1, 2,5)), lwd=1, col=c("red", "green","blue","orange"))
#Weibull分布
d<- seq(0, 5, length.out=10000)
df1<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=0.5",rw=dweibull(d, scale=1,shape=0.5 ))
df2<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=1",rw=dweibull(d, scale=1,shape=1 ))
df3<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=1.5",rw=dweibull(d, scale=1,shape=1.5 ))
df4<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=5",rw=dweibull(d, scale=1,shape=5 ))
df5<-sqldf("
           select num,groupID,rw from df1
           union all
           select num,groupID,rw from df2
           union all
           select num,groupID,rw from df3
           union all
           select num,groupID,rw from df4 ")

df<-subset(df5, rw <2 )
ggplot(df,aes(x=num,y=rw,group=factor(groupID),colour=factor(groupID)))+geom_line()+ggtitle("Weibull distribution ")


d<- seq(0, 5, length.out=10000)
df6<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=0.5",rw=pweibull(d, scale=1,shape=0.5 ))
df7<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=1",rw=pweibull(d, scale=1,shape=1 ))
df8<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=1.5",rw=pweibull(d, scale=1,shape=1.5 ))
df9<-data.frame(num=seq(0,5,length=10000),groupID="λ=1,k=5",rw=pweibull(d, scale=1,shape=5 ))
df10<-sqldf("
           select num,groupID,rw from df6
           union all
           select num,groupID,rw from df7
           union all
           select num,groupID,rw from df8
           union all
           select num,groupID,rw from df9 ")
ggplot(df10,aes(x=num,y=rw,group=factor(groupID),colour=factor(groupID)))+geom_line()+ggtitle("Weibull cummulative distribution ")
