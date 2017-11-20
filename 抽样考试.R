setwd("C:/Users/h/Desktop/bigdata_baes/2017《大数据统计基础》考试题")
mydata <- read.csv("LoanStats3C.csv",header = T,skip = 1)
data0 <- na.omit(mydata[,'loan_amnt'])
N <- length(data0)
library(ggplot2)
ggplot(mydata,aes(x=mydata$loan_amnt,fill=cut(mydata$loan_amnt,100)))+
  geom_histogram(bins = 50,show.legend = F)+
  ggtitle("贷款数据分布直方图")
data1 <- cut(data0,breaks = c(0,500*(1:10),8000,5000*(2:5),max(data0)))
PD <- table(data1)/N+0.0000000001
samp = c(100,1000,5000,10000)
n <- length(samp)
Q <- rep(0,n)
J <- NULL
fun1 <- function(i) {
  p<- sample(data0,i)
  p<- c(p,matrix(NA,1,samp[n]-length(p)))
  return(p)
}
set.seed(510)
samp <- as.matrix(samp)
ma <- apply(samp,1,fun1)
fun2 <- function(datasam1){
  datasam11 <- cut(na.omit(datasam1),breaks = c(0,500*(1:10),8000,5000*(2:5),max(data0)))
  PS <- table(datasam11)/length(na.omit(datasam1)) + 0.0000000001
  J = rep(0,16)
  for (i in 1:16){
    J[i] <- (PS[i]-PD[i])*(log(PS[i]/PD[i]))
  }
  JJ <- sum(J)
  q <- exp(-JJ)
  return(q)
}
Q1 <- apply(ma,2,fun2)
plot(samp,Q1,main = "简单随机抽样结果",xlab = "样本容量",ylab = "样本质量")
s <- smooth.spline(samp,Q1)
#当给定阈值为95%时
set_pr = 0.95
pr = predict(s,1)
n=0
while (abs(pr$y-set_pr) > 1e-5) {
  n=n+1
  pr <- predict(s,n)
  print(n)
}
##由于参考样本量过小，可多样本进行拟合预测函数
x=seq(1,30,by=1)
y = (x)^3
plot(x,y)
samp2 = y
samp2 <- as.matrix(samp2)
m = length(samp2)
bfun1 <- function(i) {
  p<- sample(data0,i)
  p<- c(p,matrix(NA,1,samp2[m]-length(p)))
  return(p)
}
mb <- apply(samp2,1,bfun1)
Q2 <- apply(mb,2,fun2)
plot(samp2,Q2,main = "简单随机抽样结果",xlab = "样本容量",ylab = "样本质量")
sb <-smooth.spline(samp2,Q2) 
set_pr = 0.95
pp = predict(sb,10)
n=0
while (abs(pp$y-set_pr) > 0.0001) {
  n=n+1
  pp <- predict(sb,n)
  print(n)
}
n

