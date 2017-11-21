# OLS（算法）
# lm() 
#
#
# Yuehan Yang. 2014/11/5 yyh@cufe.edu.cn

lm(formula, data, subset, weights, na.action,
   method="qr", model=TRUE, x=FALSE, y=FALSE,
   qr=TRUE, singular.OK=TRUE, contrasts=NULL, offset, ...)
# formula是显示回归模型, 
# data是数据框, 
# subset是样本观察的子集, 
# weights是用于拟合的加权向量, 一般无，无即OLS
# na.action显示数据是否包含缺失值,
# method是指出用于拟合的方法, 
# model, x, y, qr是逻辑表达, 如果是TRUE, 应返回其值
# (即输出结果或不输出)

# 除了第一个选项formula是必选项, 其它都是可选项


# 常见 lm() 使用方式
myfit <- lm(formula, data)

formula:
  y~X1+X2+...+Xp
# 注：这里 X1,...,Xp 代表不同的解释变量，不代表样本数

# 下表给出对回归分析有用的函数
~ 分隔符号，左边为响应变量，右边为解释变量
# 例如：通过x,z,w预测y，代码为 y ~ x + z + w
+ 分隔预测变量
: 表示预测变量的交互项
# 例如：要通过x,z及x与z的交互项预测y，代码为y ~ x + z + x:z
* 表示所有可能交互项的简洁方式
# 例如：代码 y ~ x * z * w 展开为
# y ~ x + z + w + x:z + x:w + z:w + x:z:w
^ 表示交互项达到某个次数
# 例如：代码 y ~ (x + z + w)^2 展开为
# y ~ x + z + w + x:z + x:w + z:w
. 表示包含除因变量外的所有变量
# 例如：若一个数据框包含变量x,y,z和w，
# 代码 y~ .可以展开为 y ~ x + z + w
- 减号，表示从等式中移除某个变量
# 例如：y ~ (x + z + w)^2 – x:w 展开为
# y ~ x + z + w + x:z + z:w.
-1 删除截距项
# 例如：y ~ x -1 拟合y在x上的回归，并强制直线通过原点
I() 从算术的角度来解释括号中的元素
# 例如：y ~ x + (z + w)^2 展开为
# y ~ x + z + w + z:w. 
# 相反：y ~ x + I((z + w)^2) 展开为
# y ~ x + h, h是一个由z和w的平方和创建的新变量
function 可以在表达式中用的数学函数
# 例如：log(y) ~ x + z + w 表示通过x, z, w预测log(y)




# 函数列表
summary() 展示拟合模型的详细结果
coefficients() 列出拟合模型的模型参数（回归参数）
confint() 提供模型参数的置信区间（默认95%）
fitted() 列出拟合模型的预测值
residuals() 列出拟合模型的残差
anova() 生成拟合模型的方差分析表
vcov() 列出模型参数的协方差矩阵
AIC() 输出 AIC 统计量
plot() 生成评价拟合模型的散点图
predict() 用拟合模型对新的数据集预测响应变量

# 我们将在实例1中详细介绍所有结果