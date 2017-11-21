# 模拟数据——伪随机数
# 在软件中，随机数实际是基于确定性的数学算法产生的，
# 因此，我们称之为伪随机数
#
# Yuehan Yang. 2015/10/27 yyh@cufe.edu.cn

# 常见分布的随机数
# 用dnorm获得密度曲线，并用图形表示
# 分布函数pnorm，效果类似

## e.g.2 Normal distribution
x = seq(from= -5, to = 5,length = 101)
# density() 可以给出一个观测值样本的密度曲线
density1 = dnorm(x,0,1) 
# plot() 是R中最基本的画图函数
plot(x,density1)
density2 = dnorm(x,0,2)
plot(x,density2)
density3 = dnorm(x,0,1/2)
plot(x,density3)

plot(x,density1,main ="Main title", sub="subtitle", 
     xlab="x-label",ylab="y-label")

# 一张合格绘图
plot(x,density1,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',
     ylab='density',xlab='',
     main="The Normal Density Distribution",lwd = 2)
# type = 'l' 图形类型（线）
# 增加其他密度曲线
lines(x,dnorm(x,0,0.5),col="green",lwd = 2)
lines(x,dnorm(x,0,2),col="blue",lwd = 2)
lines(x,dnorm(x,-2,1),col="orange",lwd = 2)
#增加图例
legend("topright",legend=paste("m=",
  c(0,0,0,-2)," sd=", c(1,0.5,2,1)),
  lwd=1, col=c("red", "green","blue","orange"))
# lwd 控制连线宽度
# 完毕

## 图像保存
pdf("D:/density.pdf")
source("F:/work/teaching/statistical(2015)/3. 随机变量，简单画图/densplot.R")
densplot()
dev.off()
##


## 数据保存
data<-data.frame(x,density1,density2,density3)
write.table(data,"D:/density.txt")

# 类似的，还有write.csv, gdata


##
## 类似的，分布函数
## 作为练习，请完全模仿上图绘制，包含上图全部信息，保存所有结果


## e.g.3 大数定律
# 当 n趋于无穷大时，均值随机变量\bar X 趋于理论期望 \mu
mean(runif(1))
mean(runif(10))
mean(runif(100))
mean(runif(1000))
mean(runif(10000000))



## e.g.4 中心极限定理
# 同样的，当n趋于无限大时，随机序列的和的标准化依分布收敛于标准正态分布
# 我们引用程序包 ConvergenceConcepts 来介绍这种演变
install.packages("ConvergenceConcepts")
library(ConvergenceConcepts)
investigate() # 选择第三个例子




# 练习答案
prob <- pnorm(x,0,1)
plot(x,prob,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',
     xaxs="i", yaxs="i",ylab='density',xlab='',
     main="The Normal Cumulative Distribution")

lines(x,pnorm(x,0,0.5),col="green")
lines(x,pnorm(x,0,2),col="blue")
lines(x,pnorm(x,-2,1),col="orange")

legend("bottomright",
       legend=paste("m=",c(0,0,0,-2)," sd=", c(1,0.5,2,1)), 
       lwd=1,col=c("red", "green","blue","orange"))
