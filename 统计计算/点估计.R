# 点估计
# 对应分布的未知参数，提出一个估计值——基于分布产生的样本得到。

# 一个最简单的例子
theta <- 7 #不妨假设是未知的
mean(rnorm(1000,mean=theta)) # 使用观测样本(x1,...,x10000)
# 得到的结果，与未知参数是相近的

# 同样的，
theta<- 2 # Var(X) 假设未知
vecx <- rnorm(1000,sd =sqrt(theta))
mean(vecx^2)-(mean(vecx))^2
