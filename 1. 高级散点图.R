# 高级绘图
# 
#
# Yuehan Yang. 2015/3/23 yyh@cufe.edu.cn

# 1. 基本散点图的升级版本
attach(mtcars)

mtcars

attributes(mtcars)

?mtcars

names(mtcars)

head(mtcars)

mpg

wt

install.packages("car")
library(car) 
# car 中的scatterplot函数
#为散点图增加：拟合曲线、边界箱线图、置信椭圆、
#按子集绘图和交互式识别点
scatterplot(mpg ~ wt | cyl, data=mtcars, lwd=2,
            # mpg~wt|cyl 按条件绘图，即按cyl(气缸)的水平分别绘制mpg(英里/每加仑)
            # 与wt(车重量)的关系图。
            main="Scatt er Plot of MPG vs. Weight by # Cylinders", 
            xlab="Weight of Car (lbs/1000)", 
            ylab="Miles Per Gallon", id.method="identify",
            # id.method 设定可通过鼠标单击来交互式地识别数据点，
            # 直到用户选择Stop或Esc
            legend.plot=TRUE, labels=row.names(mtcars), 
            # 前者增加图例，后者设定可通过点的行名称来识别点。
            boxplots="xy")
            # 箱线图
# 随着车重的增加，每加仑英里数的减少。
# 给出了4、6、8个气缸的骑车每加仑英里数与车重的关系图形。
# 默认各子集按照颜色、图形符号区分，
# 同时绘制线性拟合和平滑拟合曲线。



# 下面用ggplot2包内带的汽车测试数据（mpg）来举个例子，
# 用到的三个变量分别是发动机容量(displ)、
# 高速公路上的每加仑行驶里数(hwy)、汽缸数目(cyl)。
# 加载ggplot2包，然后用ggplot定义第一层即数据来源。
# aes参数，将displ映射到X轴，将hwy映射到Y轴，
# 将cyl变为分类数据后映射为不同的颜色。
# 使用+号添加了两个新的图层，
# 第二层是加上了散点，第三层是加上了loess平滑曲线。
install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data=mpg,aes(x=displ,y=hwy,colour=factor(cyl)))
p + geom_point() + geom_smooth()


# 作为对比                                                     
plot(wt, mpg, 
     main="Basic Scatterplot of MPG vs. Weight",       
     xlab="Car Weight (lbs/1000)", 
     ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg ~ wt), col="red", lwd=2, lty=1)            
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)   







