# 实例：
# 
# 与step()有相同作用的另一个函数
# stepAIC()
# 采用类似的变量选择方式去解决如下例子
#
# Yuehan Yang. 2014/11/18 yyh@cufe.edu.cn


# 看一下我们组合的数据框的统计信息
cor(states)
# 计算彼此相关性
#               Murder   Population Illiteracy    Income
# Murder      1.0000000  0.3436428  0.7029752 -0.2300776
# Population  0.3436428  1.0000000  0.1076224  0.2082276
# Illiteracy  0.7029752  0.1076224  1.0000000 -0.4370752
# Income     -0.2300776  0.2082276 -0.4370752  1.0000000
# Frost      -0.5388834 -0.3321525 -0.6719470  0.2262822
#               Frost
# Murder     -0.5388834
# Population -0.3321525
# Illiteracy -0.6719470
# Income      0.2262822
# Frost       1.0000000

# 应用 car 包里的 scatterplotMatrix() 函数
library(car)
scatterplotMatrix(states, spread = FALSE, 
                  main = "Scatterplot Matrix")
# scatterplotMatrix()
# 在对角线区域绘制变量间的散点图
# 并增加平滑(loess)曲线（见红线）
# 与线性拟合曲线（绿线）
# 对角线区域绘制每个变量的密度图和轴须图

结果分析：
1. Murder 谋杀率是双峰曲线，每个预测变量都一定程度上出现了偏斜

2. 谋杀率随人口和文盲率的增加而增加

3. 随收入、冷天数增加而下降

4. 越冷的州，文盲率越低、收入水平越高


# 使用lm()函数拟合多元线性回归模型
fit <- lm(Murder ~ Population + Illiteracy + Income + 
            Frost, data = states)
summary(fit)
# Coefficients:
#               Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)  1.235e+00  3.866e+00   0.319   0.7510    
#  Population  2.237e-04  9.052e-05   2.471   0.0173 *  
# Illiteracy   4.143e+00  8.744e-01   4.738 2.19e-05 ***
#   Income     6.442e-05  6.837e-04   0.094   0.9253    
#   Frost     5.813e-04  1.005e-02   0.058   0.9541    
# ---
结果分析：
回归参数的检验的显著性水平都不是很好，
人口、文盲率的检验还算合格，其他参数的p值都过大了



# 使用MASS包中的stepAIC()
library(MASS)
stepAIC(fit, direction = "backward")
# stepAIC(fit) 试验过也可以
# 程序结果有点长，我就不复制过来了
结果分析：
最后删除了 Frost 和 Income 两个变量后得到“最优模型”的回归参数
Coefficients:
  (Intercept)   Population   Illiteracy  
    1.6515497    0.0002242    4.0807366 
