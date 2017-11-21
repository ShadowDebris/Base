# 时间序列
# ggplot2包也能对时间序列数据绘图，但在处理上需要有些注意的地方。
# 下面我们以上证指数为例进行作图



# 
# Yuehan Yang. 2016/9/29 yyh@cufe.edu.cn

# 首先利用quantmod包从yahoo数据源获取从1997年以来的数据，
# 存于变量^SSEC中
install.packages("quantmod")
install.packages("ggplot2")
library(quantmod)
library(ggplot2)
getSymbols('^SSEC',src='yahoo',from = '1997-01-01')
# 抽取收盘数字
close <- (Cl(SSEC))
# 分别提取时间数据和指数数值
time <- index(close)
value <- as.vector(close)
# 
p <- ggplot(data.frame(time,value),aes(time,value))
p + geom_line()

# 我们希望能够在图中加入一些其它的说明元素，
# 以丰富视图中所包含的信息。
# 这些信息包括用不同的颜色区块
# 表示“江核心”和“胡核心”的执政时期，
# 以及对中国证券市场的若干大事件进行标注。
# 最后的代码和结果如下。
yrng <- range(value)
xrng <- range(time)
data <- data.frame(start=as.Date(c('1997-01-01','2003-01-01')),end=as.Date(c('2002-12-30','2012-01-20')),core=c('jiang','hu'))
timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-01-10','2010-03-31'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机爆发','融资融券试点')
data2 <- data.frame(timepoint,events,stock=value[time %in% timepoint])
# 
p + geom_line()
+ geom_rect(aes(NULL,NULL,xmin = start, xmax = end, fill = core),ymin = yrng[1],ymax=yrng[2],data = data)
+ scale_fill_manual(values = alpha(c('blue','red'),0.2))
+ geom_text(aes(timepoint, stock, label = events),data = data2,vjust = -2,size = 5)
+ geom_point(aes(timepoint, stock),data = data2,size = 5,colour = alpha('red',0.5))



# 将数据按照year这个变量划分为两组，
# 用不同的颜色绘制直方图，
# 用频率而非计数来刻画Y轴，并添加密度曲线。
library(ggplot2)
p <- ggplot(mpg,aes(hwy))
p + geom_histogram(position = 'identity',
                   alpha=0.5,
                   aes(y = ..density..,
                       fill = factor(year))) +
  stat_density(geom = 'line',
               position = 'identity',
               aes(colour = factor(year)))

