##数据预处理第一题
#1）对数据作图估计预测变量和被解释变量之间的函数关系。
#首先读取数据
library(AppliedPredictiveModeling)
data(abalone)
#除type以外均为数值型变量
head(abalone)
dim(abalone)
str(abalone)
#分别做图展示
library(lattice)
library(ggplot2)
#数值型变量采用plot函数拟合
 myplot<- function(x) {
   xyplot(abalone$Rings~x,
         type=c("p","smooth"),
         ylab = "Rings")}
apply(abalone[,-c(1,9)],2,myplot)
ggplot(data=abalone,aes(x=Rings,fill=Type))+
      geom_bar()
#2)	用散点图和相关系数图解释预测变量之间的相关性
attach(abalone)
library(GGally)
ggscatmat(abalone,columns = 2:9,color = "Type")
#3）对预测变量估计重要性得分。找到一种筛选方法得到预测变量子集，该集合不含冗余变量
#首先利用过滤法进行筛选
any(is.na(abalone[,2:8]))#是否存在缺失值
nearZeroVar(abalone[,2:8])#是否存在低方差预测变量
correlations <- cor(abalone[,2:8])#是否存在高相关性变量
library(corrplot)
corrplot(correlations,order = "hclust")
#其次计算变量估计重要性得分
#方法一、定量分析变量和结果变量之间的关系
#利用caret包filterVarImp可以建立LOESS模型
loessResults <- filterVarImp(x = abalone[,2:8],#数值型预测变量
                             y = abalone[,9],#结果变量
                             nonpara = TRUE)#nonpara参数表示是非参数回归
loessResults
#方法二、利用minerva包中mine函数计算MIC(最大信息数)来筛选预测变量
library(minerva)
micValues <- mine(abalone[,2:8],abalone[,9])
#计算了如下几个统计量
names(micValues)
micValues$MIC

#对于分类变量Type使用方差分析，检验不同组差异显著性
str(abalone$Type)
getAnova <- aov(abalone$Rings~abalone$Type)
#结果显示不同Type之间存在显著差异
summary(getAnova)

##数据预处理第二题
#1)	写一个R函数从该模型中模拟数据
simulation <- function (n) {                                     
  x <- matrix(runif(5 * n), ncol = 5)
  y <- (10 * sin(pi * x[, 1] * x[, 2])+ 
    20 * (x[, 3] - 0.5)^2 + 
    10 * x[, 4] + 5 * x[, 5]+
      rnorm(n, sd = 1))
  list(x = x, y = y)
}
#2)	随机模拟一个数据集，样本量是500，绘制图形研究预测变量和被解释变量之间的关系
set.seed(520)
simu_set <- simulation(500)
set_x <-simu_set$x
set_y <- simu_set$y
dim(set_x)
str(set_x)
hisplot<- function(x) {
  xyplot(set_y~x,
         type=c("p","smooth"),
         ylab = "y_simulation")}
apply(set_x,2,hisplot)

#3)	使用线性回归中的向前法、向后法和逐步回归等变量选择方法，最终模型选择了哪些变量？
library(MASS)
#将模拟集转化为数据库
simu_data <- data.frame(cbind(set_y,set_x))
names(simu_data)=c("Y","x1","x2","x3","x4","x5")
mymodel <- lm(Y~.,data = simu_data)
#根据拟合结果，X4的t检验p值为0.468，其余均显著
summary(mymodel)
#分别用三种方法选择变量
stepAIC(mymodel,direction = "forward")#向前,未剔除变量
stepAIC(mymodel,direction = "backward")#向后,剔除变量X3
stepAIC(mymodel,direction = "both")#逐步选择,剔除变量X3

#4)	应用不同的过滤法，逐个评估变量。一些过滤法同时评估多个变量（如ReliefF算法），两个有交互效应的预测变量 和 否被选中了？是否倾向于选择其中某一个变量？



