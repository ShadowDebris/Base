# 直方图
# 探索性数据分析的基本工具
# （单组数据）
# 给出数据的频率分布图形
# 例如（等距频率直方图）：
# 设 X_1,...,X_n 是来自分布密度为 f(x) 的总体 X 的样本
# 把 X 的取值范围等分为 m 个小区间，用 d 表示区间长度，
# 用 \mu_i 表示落入第 i 个小区间的样本个数
#
# 在等距情况下，图形一般用宽度相等的长条矩形来表示，
# 矩形高低表示频率大小
# 横坐标表示所关心变量的取值区间
# 纵坐标表示频率的大小
# 即频率（频数）直方图 hist()
#
# Yuehan Yang. 2014/9/24 yyh@cufe.edu.cn

hist(x, breaks = "Sturges", freq = NULL, probability = !freq,
     # breaks：1. 向量（用于指明直方图区间的分割位置）
     #         2. 正整数（用于指定直方图的小区间数）
     # freq：取 T 表示使用频数画直方图；F 表示使用频率画直方图
     # probability：T/F 选择与freq 恰好相反
     col = NULL,
     # 矩形颜色
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, nclass = NULL)

#
#
#
# e.g.1 从二项分布 binom(100,0.9) 中抽取容量为 N=100000 的样本.
# 试作出它的直方图及核密度估计曲线.
#
N <- 100000
n <- 100
p <- .9
x <- rbinom(N,n,p)
hist(x,
     xlim=c(min(x),max(x)), probability=T,
     nclass=max(x)-min(x)+1, col='lightblue',
     main='Binomial distribution, n=100, p=.5')
lines(density(x,bw=1), col='red', lwd=3)
#
#
#
#
#
# e.g.2 考查雌性负鼠(fpossum)的总长度的频率分布.
#
library(lattice)
library(DAAG)
data(possum)
fpossum <- possum[possum$sex=="f",]
par(mfrow=c(1,2)) # 分割图形函数
# attach(fpossum) # 激活
hist(possum$totlngth,breaks=72.5+(0:5)*5,
     ylim=c(0,50), xlab="total length",
     main="A:Breaks at 72.5,77.5...")
# totlngth 总长度
# breaks 区间分割
hist(possum$totlngth,breaks=75+(0:5)*5,
     ylim=c(0,50), xlab="total length",
     main="B:Breaks at 75,80...")
#
# 两个图的唯一不同之处是选择的区间端点不同,我们可以看到左
# 边的图不对称,而右边显示该分布是对称的

# 附加内容：

# 茎叶图
# 用于考查数据分布

# e.g.
stem(fpossum$totlngth)
# 茎叶图的外观很像横放的直方图
# 特点：左边茎，右边叶
# 叶增加了具体的数值，从而保留了更多的数据信息

# 左边茎是长度(厘米)的整数部分, 右边是小数点后边的部分
# 数据采用了近似, 所以右边只有0与5

# 框须图
# 五数的图形概括

boxplot(fpossum$totlngth)
# 箱子中的五根横线对应的坐标分别是最小值,第一4分位数, 中位
# 数, 第三4分位数和最大值

