setwd("E:/学习资料/研究生学习/统计计算/作业/最终")
library(quantmod)#quantmod包可以帮助从一些开放源直接下载金融数据，包括雅虎财经、谷歌财经等
#获取数据
a=read.csv("code.csv",header = FALSE ,stringsAsFactors=FALSE)#读取500个成分股的代码
X=c()
for(i in 1:500){
  A=get(getSymbols(a[i,],from="2017-05-08"))
  X=cbind(X,A[,4])
}#读取到的数据包括每支成分股的开盘价、最高价、收盘价等。
X=as.matrix(X)
dim(X)#读取了499支成分股共129天的数据
any(is.na(X))#查看是否含有缺失值
#剔除含有缺失值的列
X1=t(X)
X1=na.omit(X1)
X2=t(X1)
dim(X2)#最后剩下489个变量
write.csv(X,"SP500.csv")
