# 单正态总体参数的区间估计
# 3. 方差 \sigma^2 的置信区间
#
# Yuehan Yang. 2014/10/15 yyh@cufe.edu.cn

# 在R中也没有直接求\sigma^2的置信区间的函数, 
# 我们需要编写自己需要的函数,
# 下面的函数chisq.var.test( )可以用来求\sigma^2置信区间. 
# (第六章还将用于关于sigma^2的假设检验.)

chisq.var.test <- function (x,var,alpha,alternative="two.sided"){
  options(digits=4)
  result<-list( )
  n<-length(x)
  v<-var(x)
  result$var<-v
  chi2<-(n-1)*v/var
  result$chi2<-chi2
  p<-pchisq(chi2,n-1)
if(alternative == "less"|alternative=="greater"){
  result$p.value<-p
} else if (alternative=="two.sided") {
  if(p>.5)
    p<-1-p
  p<-2*p
  result$p.value<-p
} else return("your input is wrong")
#
result$conf.int<-c(
  (n-1)*v/qchisq(alpha/2, df=n-1, lower.tail=F),
  (n-1)*v/qchisq(alpha/2, df=n-1, lower.tail=T))
#
result
}

# e.g.
# 一个人10次称自己的体重(单位:斤): 
# 175 176 173 175 174 173 173 176 173 179, 
# 我们希望估计一下他的体重. 
# 假设此人的体重服从正态分布,方差未知, 
# 我们要求体重的置信水平为95%的置信区间.

# 解
x<-c(175 , 176 , 173 , 175 ,174 ,173 , 173, 176 , 173,179 )
chisq.var.test(x,var(x),0.05)
