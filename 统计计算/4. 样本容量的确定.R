# 样本容量
#
# Yuehan Yang. 2014/10/22 yyh@cufe.edu.cn

# 用函数size.norm1( )求样本容量

# 1. 方差已知时
size.norm1<-function(d,var,conf.level) {
  alpha = 1 - conf.level
  ((qnorm(1-alpha/2)*var^(1/2))/d)^2
}
# d 最大绝对误差
# var 已知方差
# conf.level 置信水平

# e.g.某地区有10000户, 拟抽取一个简单的样本调查一个月的平均开支, 
# 要求置信度为95%, 最大允许误差为2, 
# 根据经验, 家庭间开支的方差为500, 应抽取多少户进行调查?

size.norm1(2,500,conf.level=0.95)
# 所以应该抽取481户.

# 2. 方差未知时
# 采用如下函数
size.norm2<-function(s,alpha,d,m){
  t0<-qt(alpha/2,m,lower.tail=FALSE)
  n0<-(t0*s/d)^2
  t1<-qt(alpha/2,n0,lower.tail=FALSE)
  n1<-(t1*s/d)^2
  while(abs(n1-n0)>0.5){
    n0<-(qt(alpha/2,n1,lower.tail=FALSE)*s/d)^2
    n1<-(qt(alpha/2,n0,lower.tail=FALSE)*s/d)^2
  }
  n1
}
# 该函数是一个循环结构(while)
# s 对应样本标准差
# alpha 1-置信区间
# d 最大绝对误差
# m 事先给定的一个很大的数


# e.g.某公司生产了一批新产品, 产品总体服从正态分布, 
# 现要估计这批产品的平均重量, 最大允许误差2, 
# 样本标准差s=10, 试问\alpha=0.01下要抽取多少样本

size.norm2(10,0.01,2,100)
# 也就是说在最大允许误差为2的时候应抽取170个样本. 




# 3. 估计比例p时的样本容量
# 采用函数：
size.bin<-function(d, p, conf.level=0.95) {
  alpha = 1 - conf.level
  ((qnorm(1-alpha/2))/d)^2*p*(1-p)
}
# d 最大绝对误差
# p 样本比例

# e.g.某市一所重点大学历届毕业生就业率为90%, 
# 试估计应届毕业生就业率,要求估计误差不超过3%,
# 试问在\alpha=0.05下要抽取应届毕业生多少人?

size.bin(0.03, 0.9, 0.95)
# \alpha=0.05下要抽取应届毕业生385人估计误差不超过3%.


