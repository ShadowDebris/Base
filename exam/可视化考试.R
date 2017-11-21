###读入数据
setwd("C:/Users/h/Desktop/bigdata_baes/2017《大数据统计基础》考试题")
mydata<-read.csv("LoanStats3c.csv",header=T,skip=1)
###删除缺失值
mydata0<-na.omit(mydata)
###加载包
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)  
library(maps)
library(mapdata)
library(maptools)
library(graphics)
library(REmap)
str(mydata0)
##一、在loan data中，自选合适的变量，绘制以下图形
#1)分面的风玫瑰图
ggplot(mydata0,aes(x=grade,fill=emp_length))+
  geom_bar(color='white')+coord_polar(theta = 'x')+
  scale_fill_brewer(palette='Spectral')+
  facet_wrap(~verification_status,ncol = 2,nrow = 2)+
  theme_bw()+ 
  labs(x="信用评级",y="频数",fill="工作年限",title='分面风玫瑰图')+
  scale_x_discrete()+
  coord_polar(theta="x")+
  theme(plot.title = element_text(hjust = 0.5,size=20,color="darkred"),
        panel.background=element_rect(fill='seashell')) 

#2)直方图
#查看funded_amnt资助金额的极差、四分位数
summary(mydata0$funded_amnt)
#设置组数和组距
num_bin = 18
size_bin=diff(range(mydata0$funded_amnt))/(num_bin-1)
size_bin
mydata2<-cut(mydata0$funded_amnt,
             breaks=c(0,2000*(1:17),
                      max(mydata0$funded_amnt)))
table(mydata2)#为添加标签做准备
ggplot(mydata0,aes(x=funded_amnt))+
  geom_histogram(aes(y=..density..),#密度直方图图层
                 binwidth=size_bin,
                 fill='deepskyblue2',
                 color='white')+
  stat_function(fun=dnorm,#dnorm正态分布曲线
                args=list(mean(mydata0$funded_amnt),sd(mydata0$funded_amnt)),
                size=0.5,
                color='red',alpha=0.5)+
  geom_line(stat='density',color='limegreen',size=1)+#核密度估计曲线
  expand_limits(y=0)+
  theme(plot.background=element_rect(fill="seashell",colour="black",size=2),
        panel.background=element_rect(fill=NA),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size=20,color="darkred"))+
  labs(x='存款总额',y='频数',title='存款总额分布直方图')+
  annotate(geom="text",x=9e3,y=8e-5,label='此处密度最大',size=4,colour="gold2")+
  annotate(geom="text",x=2e3,y=1e-5,label="221",size=3,colour="Sienna4")+
  annotate(geom="text",x=4e3,y=3e-5,label='913',size=3,colour="Sienna4")+
  annotate(geom="text",x=6e3,y=3.1e-5,label='1905',size=3,colour="Sienna4")+
  annotate(geom="text",x=8e3,y=3e-5,label='1739',size=3,colour="Sienna4")+
  annotate(geom="text",x=1e4,y=3.8e-5,label='2146',size=3,colour="Sienna4")+
  annotate(geom="text",x=1.2e4,y=2.2e-5,label='1484',size=3,colour="Sienna4")+
  annotate(geom="text",x=1.4e4,y=2.1e-5,label='807',size=3,colour="Sienna4")+
  annotate(geom="text",x=1.6e4,y=1e-5,label='1262',size=3,colour="Sienna4")+
  annotate(geom="text",x=1.8e4,y=0.9e-5,label='566',size=3,colour="Sienna4")+
  annotate(geom="text",x=2e4,y=1.2e-5,label='678',size=3,colour="Sienna4")+
  annotate(geom="text",x=2.2e4,y=0.6e-5,label='261',size=3,colour="Sienna4")+
  annotate(geom="text",x=2.4e4,y=0.5e-5,label='263',size=3,colour="Sienna4")+
  annotate(geom="text",x=2.6e4,y=0.2e-5,label='259',size=3,colour="Sienna4")+
  annotate(geom="text",x=2.8e4,y=0.4e-5,label='181',size=3,colour="Sienna4")+
  annotate(geom="text",x=3e4,y=0.4e-5,label='169',size=3,colour="Sienna4")+
  annotate(geom="text",x=3.2e4,y=0.2e-5,label='61',size=3,colour="Sienna4")+
  annotate(geom="text",x=3.4e4,y=0.4e-5,label='40',size=3,colour="Sienna4")+
  annotate(geom="text",x=3.6e4,y=0.1e-5,label='297',size=3,colour="Sienna4")
#3)密度图
#生成几何对象
pt=ggplot(mydata0,aes(x=annual_inc,y=funded_amnt))
#将level值映射给线条颜色
pt+stat_density2d(aes(colour=..level..))+
  theme_bw()+
  annotate("rect",xmin=4e4,xmax=6e4,ymin=5000,ymax=12000,alpha=0.5,fill="aquamarine")+
  labs(x='年收入额',y='存款总额',title='年收入额与存款总额的相关密度图',color='水平')+
  theme(plot.title = element_text(hjust = 0.5,size=16,color="royalblue4"),
        panel.background=element_rect(fill='white')) 
#4)热图
heatset<-mydata0 %>%
  subset(select=c(loan_amnt,funded_amnt,annual_inc,dti,installment)) 
#取完整数据集
heatset<-na.omit(heatset)
#标准化处理
heatset<-scale(heatset)
obsnum<-rownames(heatset)
heatset=heatset[sample(nrow(heatset),60),]#只抽取60条数据作为样本
ds<-dist(heatset,method="euclidean") 
ds<-as.matrix(ds)
heatmap(ds,main="Heatmap")

##二、使用province数据中合适的变量，绘制两幅不同的图，进行空间数据的展示
#1.中国各地区GDP分布图
mapset<-read.csv("province.csv",header=T)
#除去年份和单位行
mapset<-mapset[-c(1,2),]
colnames(mapset)[1] <-'地区' 
attach(mapset)
#获取中国地图数据
map("china")
x=maptools::readShapePoly('bou2_4p.shp')
dsnames<-c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
            "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
            "浙江省", "安徽省", "福建省", "江西省", "山东省",
            "河南省", "湖北省", "湖南省", "广东省",
            "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省",
            "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省",
            "宁夏回族自治区", "新疆维吾尔自治区")
GDP<-mapset$地区生产总值
#定义颜色函数
GDP<-as.numeric(as.character(GDP))
GDP <-GDP-min(GDP)  
getColor=function(mapdata,dsnames,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,dsnames); 
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}
nf <- layout(matrix(c(1,1,1,1,1,2,1,1,1),3,3,byrow=TRUE), c(3,1), c(3,1), TRUE)#设定图例位置 
layout.show(nf)  
gdpcol=rgb(green=1-GDP/max(GDP)/2,blue=1-GDP/max(GDP)/2,red=1-GDP/max(GDP)/20)
plot(x,col=getColor(x,dsnames,gdpcol,"white"),xlab="",ylab="",main='2016年中国各地区GDP分布状况')
par(mar=c(0,0,0,0))  
par(mar=c(1,1,2,0),cex=0.5)  # 添加图例 
barplot(as.matrix(rep(1,31)),col=sort(gdpcol,dec=T),horiz=T,axes=F,border = NA )  
axis(1,seq(1,32,by=3),sort(GDP[seq(1,32,by=3)])) 
detach(mapset)


##2.中国各地区人均可支配收入空间柱状图
#处理数据
data1<-read.csv("province.csv",header=T)
data1<-data1[-c(1,2),]
colnames(data1)[1] <-'省市' 
attach(data1)
provname<-c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
            "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
            "浙江省", "安徽省", "福建省", "江西省", "山东省",
            "河南省", "湖北省", "湖南省", "广东省",
            "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省",
            "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省",
            "宁夏回族自治区", "新疆维吾尔自治区")
data1$省市<-provname
province <- data.frame(get_geo_position (provname))
str(province)
names(province)[3] <- c("NAME" )
colnames(data1)[1] <-'NAME' 
china_data_REmap <- join(data1,province,type="full",by="NAME")  
#依次设置图数据参数
china_map <- readShapePoly('bou2_4p.shp')    
china_map1 <- 
  china_map@data%>%
  data.frame(china_map1,id=seq(0:36)-1) 
china_map2 <- fortify(china_map)
china_map3 <- join(china_map2, china_map1, type="full",by="id")  
china_map4 <- join(china_map3, data1, type="full",by="NAME")
china_data_REmap$可支配收入<-as.numeric(as.character(china_data_REmap$可支配收入))
china_map4$年末人口数<-as.numeric(as.character(china_map4$年末人口数))
#利用ggplot2包绘制
ggplot ()+
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group),fill="orange",alpha=0.5,colour="aliceblue")+
  scale_fill_gradient(name="各省市人口分布",low="white",high="red")+
  geom_errorbar(data=china_data_REmap,aes(x=lon, ymin=lat, ymax=lat + 可支配收入/5000 ),
                colour="darkred",size=5, width=0,alpha=0.6)+
  labs(title ="各地区人均可支配收入")+ylim (18, 54)+
  theme(plot.title = element_text(hjust = 0.5,size=12,color="royalblue4"))


