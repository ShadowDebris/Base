# p区间估计
#
# Yuehan Yang. 2014/10/19 yyh@cufe.edu.cn

# 1. p的区间估计
# 用prop.test( )对p进行估计与检验
prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
# x为样本中具有某种特性的样本数量, 
# n为样本容量, 
# correct选项为是否做连续性校正. 

# e.g.从一份共有3042人的人名录中随机抽200人, 
# 发现38人的地址已变动, 试以95%的置信度, 
# 估计这份名录中需要修改地址的比例.

prop.test(38,200,correct=TRUE)

# 所以我们以95%的置信水平认为这份名录中需要修改地址的
# 比例p为落在(0.1395, 0.2527)中, 其点估计为0.19.

prop.test(38,200,correct=FALSE)
# 此时p的95%置信区间为(0.1417, 0.2500), 其长度比修正的缩短了.

# 2. p_1-p_2的区间估计

# 没有修正的两比例之间的区间估计函数ratio.ci( ) 
ratio.ci<-function(x, y, n1, n2, conf.level=0.95){
  # ratio.ci( )的定义
  xbar1=x/n1;xbar2=y/n2
  xbar=xbar1-xbar2
  alpha = 1 - conf.level
  zstar=qnorm(1-alpha/2)
  *(xbar1*(1-xbar1)/n1+xbar2*(1-xbar2)/n2)^(1/2)
  xbar +c(-zstar, +zstar)
}

# e.g. 据一项市场调查, 在A地区被调查的1000人中有478人喜欢品牌K, 
# 在B地区被调查750中有246人喜欢品牌K, 
# 试估计两地区人们喜欢品牌K比例差的95%置信区间.

ratio.ci(478,246,1000,750,conf.level=0.95)
# 两比例之差的95%的置信区间为(0.1043, 0.1957), 

# 或是修正后结果
like<-c(478, 246)
people<-c(1000, 750)
prop.test(like, people)
# 可以看出A地区喜欢品牌K的人更多, 
# 且A、B两地区喜欢品牌K的比例之差的95%的置信区间为(0.1031, 0.1969).
# 其区间结果较未修正结果增大.