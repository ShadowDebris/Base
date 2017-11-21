# 实例：
# 
# 与step()有相同作用的另一个函数
# stepAIC()
# 采用类似的变量选择方式去解决如下例子
#
# Yuehan Yang. 2014/11/18 yyh@cufe.edu.cn


# 研究目标：研究一个州的犯罪率和其他因素的关系
# 其他因素例如：人口数、文盲比例、收入、冷天数等等
# 采用数据集：
state.x77
# （各州各类型数据）
# 由于该数据集里包含数据较多，我们做一个提取工作
states <- as.data.frame(
  state.x77[, c("Murder", "Population", 
                "Illiteracy", "Income", "Frost")])
# Murder 是我们的研究对象（响应变量）
# 预测变量：
# Population（人口）、Illiteracy（文盲）
# Income（收入）、Frost（结霜天数）
# 注：我们并不清楚这几个预测变量是否都能对Murder
# 造成影响，因此需要进行变量选择



# 应用 car 包里的 scatterplotMatrix() 函数构造散点图
library(car)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, 
                  main = "Scatterplot Matrix")
# scatterplotMatrix()
# 在对角线区域绘制变量间的散点图
# 并增加平滑(loess)曲线（见红线）
# 与线性拟合曲线（绿线）
# 对角线区域绘制每个变量的密度图和轴须图

1. 散点图上有哪些信息？

2. 使用MASS程序包中的stepAIC()函数进行变量选择
# 具体操作与step()保持一致
# 向后回归