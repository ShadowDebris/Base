# 单正态总体参数的区间估计
# 2. 方差 \sigma^2 未知时 \mu 的置信区间
#
# Yuehan Yang. 2014/10/15 yyh@cufe.edu.cn

# 方差未知时我们直接利用R语言的t.test( )来求置信区间.
t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)
# 我们这里仅需要函数中的 conf.int 部分

# 原函数用于 t 检验
# 若仅出现数据x, 则进行单样本t检验; 
# 若出现数据x和y, 则进行二样本的t检验(见6.3节);
# alternative=c("two.sided", "less", "greater")
# 用于指定所求置信区间的类型; 
# alternative="two.sided"是缺省值, 
# 表示求置信区间alternative="less"表示求置信上限; 
# alternative="greater"表示求置信下限. 
# mu表示均值, 它仅在假设检验中起作用, 默认值为零.

# e.g.
# 一个人10次称自己的体重(单位:斤): 
# 175 176 173 175 174 173 173 176 173 179, 
# 我们希望估计一下他的体重. 
# 假设此人的体重服从正态分布,方差未知, 
# 我们要求体重的置信水平为95%的置信区间.

# 解
x<-c(175 , 176 , 173 , 175 ,174 ,173 , 173, 176 , 173,179 )
t.test(x)
t.test(x)$conf.int
