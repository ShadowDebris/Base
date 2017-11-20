# 散点图升级版
# 散点图矩阵
#
# Yuehan Yang. 2015/3/23 yyh@cufe.edu.cn

# 1.基础版本
pairs(~ mpg + disp + drat + wt, data=mtcars, 
      # 图中包含~右边所有变量 
      main="Basic Scatterplot Matrix")
# 两两之间的二元关系
# 例如mpg与disp两变量的交叉处的图显示它们二者之间的相关性。


# scatterplotMatrix 升级版本
library(car)
# 功能：
# 1. 以某个因子为条件绘制散点图矩阵；
# 2. 包含线性和平滑拟合曲线；
# 3. 在主对角线放置箱线图、密度图或者直方图；
# 4. 在各单元格的边界添加轴须图。

# 一言以蔽之，提高逼格，哦不，提高图形辨识率……


scatterplotMatrix(~ mpg + disp + drat + wt, data=mtcars, spread=FALSE,
                  # spread=F表示不添加展示分散度和对称信息的直线
                   main="Scatterplot Matrix via car package")
# 上图对角线添加了核密度曲线和轴须图

scatterplotMatrix(~ mpg + disp + drat + wt | cyl, data=mtcars, spread=FALSE,
                  main="Scatterplot Matrix via car package", diagonal="histogram")
# 以上，对角线更改为直方图（以气缸数为条件）


# 3. cpairs
# 这是个有趣的散点图矩阵异种（变种……什么鬼……）
# 含有可以重排矩阵中变量变量的选项
# 还可以通过对各单元各进行颜色编码来展示变量间的相关性大小
# install.packages("gclus")
library(gclus)  
cor(mtcars[c("mpg", "wt", "disp", "drat")])
mydata <- mtcars[c(1,3,5,6)]  
mydata.corr <- abs(cor(mydata))   
mycolors <- dmat.color(mydata.corr)
myorder <- order.single(mydata.corr)           
cpairs(mydata,                                                   
       myorder, 
       panel.colors=mycolors, 
       gap=.5,
       main="Variables Ordered and Colored by Correlation"
)



library(gclus)  
cor(mtcars[c("mpg", "wt", "disp", "drat")])
# 1. 从mtcars中提取所需变量
mydata <- mtcars[c(1,3,5,6)]  
# 2. 计算它们的相关系数的绝对值
mydata.corr <- abs(cor(mydata))   
# dmat.color; order.single; cpairs 均来自gclus
# 3. 获取绘图颜色（给定一个矩阵，将返回一个颜色矩阵）
mycolors <- dmat.color(mydata.corr)
# 4. 对图中变量进行排序，使得相似的对象更加靠近
myorder <- order.single(mydata.corr)           
# 5. 根据新的变量顺序(myorder)和颜色列表(mycolors)绘图、上色
cpairs(mydata,                                                   
       myorder, 
       panel.colors=mycolors, 
       gap=.5,
       main="Variables Ordered and Colored by Correlation"
)
# 结论：相关性最高——车重(wt)与排量(disp),
# 以及车重(wt)与每加仑英里数(mpg)，两者均标红


