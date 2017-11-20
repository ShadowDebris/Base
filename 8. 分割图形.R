# 绘图功能
# 绘图的工作方式与前面描述的工作方式大为不同
# 并非是赋值，而是使用我们还没有用过的一个窗口：
# Plots
# 一般来说，绘图函数分成两种：
# 高级绘图函数
# 低级绘图函数
# 前者创建图形
# 后者在已有图形上添加元素
#
# Yuehan Yang. 2014/9/10 yyh@cufe.edu.cn

# 首先，图形的分割
# 我们采用 split.screen 函数
split.screen(c(1,2))
# 这时画图设备被分成了两部分
screen(1)
# 该命令选择前半部分
plot(x)
#
screen(2)
# 该命令选择后半部分
plot(y)
#
#
#
# 此外，函数 layout 意味着将图形窗口
# 分割成多个部分，而图形将依次显示在
# 各个部分中
# 相较而言，layout的使用更加广泛
# 注：这些分割函数彼此之间是不兼容的
#
# 不失一般性，构造函数
z <- rnorm(200,0,1)
#
layout(matrix(1:4,2,2))
# 将画图窗口分割成四个部分
#
# 如何查看我们的分割情况
layout.show(4)
#
plot(z[1:50])
plot(z[51:100])
plot(z[101:150])
plot(z[151:200])
#
# 下面给出一个十分特殊的划分例子
m <- matrix(c(1,1,2,1),2,2)
layout(m, widths= c(2,1),heights=c(1,2))
layout.show(2)

plot(x)
x1 <- runif(3,0,1)
plot(x1)
