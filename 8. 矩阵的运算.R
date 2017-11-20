# 矩阵运算
# 具体的运算涉及很多命令，这里只讲一些常用的
#
# Yuehan Yang. 2014/7/20 yyh@cufe.edu.cn

X = matrix(1:9,3) # 构造一个矩阵
X
#
t(X) # 矩阵的转置
#
diag(X) # 提取对角元素
#
det(X) # 行列式
#
Y <- matrix(1:6,3)
Y
#
Z = X*Y # 矩阵的代数成绩不再是 * 了
#
X*X # 该命令表示逐元乘积，你可以试试 X*Y 行不行
#
Z <- X%*%Y
Z # 是不是比手算快多了？
# 线代作业都在纸上，我怎么输入进去比较快？
# 这是一会儿要讲的内容
#
# 我们添加点有意思的
rownames(X) <- LETTERS[1:3]
X
# 将列名称从默认的数字改成 ABC
#
# 再来进行些粘贴工作
Z1 <- cbind(A=1:4,B=5:8,C=9:12) 
# cbind: c col 的缩写，bind 结合/捆绑在一起
# cbind：按列捆绑在一起
Z1
# 将 cbind 换成 rbind 试试？
Z2 <- rbind(A=1:4,B=5:8,C=9:12)
# 顾名思义，按行进行捆绑
Z2 # 与 t(Z1) 有何相似之处？
#
# 如何求解矩阵

eigen(X)
# 求解特征值
# 
#
# 接下来是大家的练习，利用 R 进行矩阵的统计计算
# 我给出一些命令示范
# apply(X,MARGIN,FUN)
# MARGIN=1 表示按行计算，MARGIN=2 表示按列计算
# FUN 可以是命令函数：max,min,median,var,sd,sum,cov,cor,etc.
# e.g.
# 首先构造矩阵
m <- matrix(rnorm(12),nrow=3) # 现在还不清楚 rnorm 命令的，?rnorm 试试
m
#
# 运算可自由发挥，或按照如下例子
apply(m,MARGIN=1,FUN=mean) # 各行均值
scale(m,center=T,scale=T) # 标准化函数

