# lattice 绘图实例（地震位置、鸢尾花数据绘图）
#
# 这个脚本介绍的，是另一种全新的绘图方式
# 特点：更灵活的分割绘图；绘图对象可以单一地被修改
# lattice 主要想法就是在不同条件下的多图实现
# 在这里，大部分函数都把公式作为自变量
#
# 本脚本以讨论 lattice 为主
#
# 首先给出 lattice 绘图函数
#
# barchart(y ~ x)      y对x的直方图
# x ~ y 绘制关于 x 与 y 的图形
# bwplot(y ~ x)         盒形图
# densityplot(~ x)     密度函数图
# dotplot(y ~ x)      Cleveland点图(逐行逐列累加图)
# histogram(~ x)       x的频率直方图
# qqmath(~ x)         x的关于某理论分布的分位数-分位数图
# stripplot(y ~ x)   一维图，x必须是数值型，y可以是因子
# qq(y ~ x)           比较两个分布的分位数，x必须是数值型，y可以是数值
# 型，字符型，或者因子，但是必须有两个"水平"
# xyplot(y ~ x)      二元图（有许多功能）
# levelplot(z ~ x*y)
# contourplot(z ~ x*y)
# 在x，y坐标点的z值的彩色等值线图（x，y和z等长）
# cloud(z ~ x*y)      3-D透视图（点）
# wireframe(z ~ x*y)    同上（面）
# splom(~ x)            二维图矩阵
# parallel(~ x)        平行坐标图
#
# 列表供大家参考
#
# Yuehan Yang. 2014/9/17 yyh@cufe.edu.cn
#
#
library(lattice)
# 类似的，可以采用在 RStudio 的右下角 packages 窗口中加载
#
#
# e.g.1 密度函数图形
# 数据赋值
n <- seq(5, 45, 5) # 不记得向量函数定义的，复习一下第一节课的内容
x <- rnorm(sum(n)) 
y <- factor(rep(n, n), labels=paste("n =", n)) # 因子函数以及 rep 函数
# 前三行产生随机独立正态样本
# 按照 n 的排序进行分割，个数分别为：5,10,15，。。。，45的子样本
# factor（因子）：因子不仅包括分类变量本身，还包括变量不同的水平
# 水平名称用 labels 来指定 （详情参见本节课备注脚本）
#
# 接下来用 densityplot 为每个子样本产生图形,并添加经验密度曲线以及正态密度拟合曲线
# densityplot 密度函数图
# 常见使用方式：densityplot(~ x | y) 
densityplot(~ x | y,
            panel = function(x,...) {
              panel.densityplot(x, col="DarkOliveGreen", ...)
              panel.mathdensity(dmath=dnorm,
                                args=list(mean=mean(x), sd=sd(x)),
                                col="darkblue")
            })
# ~ x | y 表示按照 y 的变化绘制关于 x 的图形
# （x ~ y | z 表示关于 z 绘制 x 与 y 的二元图）
# panel 由于我们添加经验密度曲线与正态密度拟合曲线，
# 需要该函数定义每张图上绘制什么
# panel.densityplot 绘制经验密度函数 （缺省调用）
# panel.mathdensity 绘制拟合正态分布密度函数
#
#
#
# e.g.2 地震图例
# 显示不同深度的地震的地理位置
data(quakes)
# 加载地震数据 quakes：斐济岛附近 1000 个地震位置
mini <- min(quakes$depth)
maxi <- max(quakes$depth)
# 上下界赋值
# depth （地震深度）
int <- ceiling((maxi - mini)/9)
# 自变量为单一的数值参数，返回值为不小于数值参数的最小整数
# 与 ceiling 相对的是 floor，返回值为不大于数值参数的最大整数
inf <- seq(mini, maxi, int)
# 按照 int 等差排序
quakes$depth.cat <- factor(floor(((quakes$depth - mini) / int)),
                           labels=paste(inf, inf + int, sep="-"))
# 创建了一个把深度等分为九个区间的因子：
# 因子水平标签为区间的上下界
xyplot(lat ~ long | depth.cat, data = quakes)
# 二元图函数
# lat (维度) long （经度）非别是 quakes 中的数据
# 关于 depth.cat 的变化情况
# data 自变量指定绘图所用各变量所在的数据框



# e.g.3
# 鸢尾花数据分布
data(iris)
# 载入数据 iris：鸢尾花朵测量值
# 不进行数据处理，直接作图
#
# 图1. 二元图
xyplot(
  Petal.Length ~ Petal.Width, data = iris, groups=Species,
  panel = panel.superpose,
  # 没有再定义 panel.superpose 传递选项，不同组的颜色自动缺省
  type = c("p", "smooth"), span=.75,
  auto.key = list(x = 0.15, y = 0.85)
)
# 这个关于 xyplot 的例子稍复杂一些
# group 定义组以便被其他选项引用
# panel.superpose 将不同的样本数据重叠地绘制在一张图里
# type 制定数据如何显示（与在 plot 中的作用相同）
#   p 表示画点； smooth 表示画光滑曲线，span 表示光滑程度
# auto.key 在图形中添加图例（只需给出坐标列表即可）
#   PS：需要对应图形大小的比例值，也就是在这里的 [0,1]
#
#
#
#
# 图2. 二维图矩阵
splom(
  ~iris[1:4], groups = Species, data = iris,
  auto.key = list(columns = 3)
)
# 二维图矩阵
# 如脚本所示，自变量为 iris 前四列元素构成的矩阵
# auto.key = list(columns = 3) 制定图例显示在 3 列中
#
# 图3.
splom(~iris[1:3] | Species, data = iris, pscales = 0,
      varnames 
      = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"))
# pscales = 0 移除坐标轴的刻度 （所有子图刻度相同）
# varnames 变量名称重新定义（"\n" 换行）
#
# 最后给出一个用平行坐标探索多元数据分析的方法
# 变量安排在一个坐标轴上（例如 y 轴）
# 观测值绘制在另一坐标轴上
# 相同个体不同变量的值连接在一条线上
# 同样采用数据 iris
#
# 图4. 平行坐标图
parallelplot(~iris[, 1:4] | Species, data = iris, layout = c(3, 1))
