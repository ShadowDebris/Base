# 数组和矩阵
# e.g. 构造一个矩阵      
#
# 矩阵可以用 array() 或 matrix() 来建立，对角阵用函数 diag() 更加方便
# array() 构造的一般格式为
# array(data, dim, dimnames)
# data 为向量，其元素构建矩阵（数组）
# dim 表示维数
# dimnames 是由各维度名称构成的向量，缺省为空
# e.g. A <- array(1:6,c(2,3))
#
# Yuehan Yang. 2014/8/3 yyh@cufe.edu.cn

# R 中最常用于建立矩阵的是 matrix()
# matrix (data = NA, nrow = 1, ncol=1, byrow = FALSE,
# dimnames = NULL)
# data 表示数据
# nrow 表示行数
# ncol 表示列数
# byrow 按照行排序……默认为否
# dimnames 给行列命名
#
#
# e.g.
X <- matrix(,nr =2, nc=3)
# nrow 缩写 nr, ncol 缩写 nc
X
#
# 简化版本
X <- matrix(1:6,2,3)
X
# 数据可以是构造向量排列，也可以全为常数
# e.g.
matrix(5,2,3)


#
# 再次简化
X <- matrix(1:6,3)
X
# 特殊情况
X = matrix(1:6,3,byrow=T)
X
# 按照行顺序填充数据
# 有没有同学发现之前的矩阵元素都是按列排序的？
# 默认 byrow = FALSE
#
# 提取
X[2,2] # 提取元素
#
X[2,] # 按行，列进行提取
X[,2]
#
# 筛除
X[-1,] # 同样的，按行、列进行筛除
X[,-2]
X
# 提示：X 本身没有被改变
#
# 添加或替换
X[3,] <- NA # 第三行重新赋值为缺失值
X
#
X[is.na(X)] <- 1 
# 还有人记得 is.na() 命令吗？
# 缺失值用 1 代替
X
#
#
# 未来我们生成数据，还有一种非常常见的方式
#
x1 <- rnorm(10)
# 随机生成服从某正态分布的 10 个数
# r = random
# norm 正态
# 默认为 rnorm(n, mean=10, sd=1)
# sd = standard deviation(标准差)；mean(均值)
#
# 标准表达
x2 <- rnorm(50,0,2)
#
# 类似的
y1 <- runif(10)
# 随即生成服从均匀分布的数据
# 默认为
# runif(n, min = 0, max = 1)
#
# 将生成的随机数构造矩阵
x=matrix(rnorm(100*10),ncol=10)
