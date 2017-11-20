# 高密度散点图
# 对应问题：数据量大、重复率高（数据点重叠现象）
#
# Yuehan Yang. 2015/3/23 yyh@cufe.edu.cn

# 这是一个人为设计的例子
# 其中10000个观测点分布在两个重叠的数据群里
set.seed(123)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
#
#
with(mydata,
     plot(x, y, pch=19, main="Scatter Plot with 10000 Observations"))
# 问题：严重的重叠，识别困难
# 能够看出哪里的数据点密度最大吗？
# ps: with()用法，不熟悉的人留作熟悉help窗口的练习
#
# 针对这一问题，解决办法如下
with(mydata,
     smoothScatter(x, y, main="Scatterplot colored by Smoothed Densities"))
# smoothScatter
# 利用核密度估计生成用颜色密度来表示点分布的散点图
#

# hexbin中的hexbin函数
# 使用六边形单元格
install.packages("hexbin")
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations")
})
# 六边形封箱图

# 最后，IDPmisc中的iplot也可以
library(IDPmisc)
with(mydata,
     iplot(x, y, main="Image Scatter Plot with Color Indicating Density"))
par(opar)