# 单正态总体参数的区间估计
# 1. 方差 \sigma^2 已知时 \mu 的置信区间
#
# Yuehan Yang. 2014/10/15 yyh@cufe.edu.cn

# 利用以下程序给出总体均值的置信区间：
 conf.int<-function(x,n,sigma,alpha){
   # x 目标
   # n 样本个数
   # sigma 标准差
   # alpha 1-置信水平
  options(digits=4)
   # 控制 R 计算和陈列结果（小数点）
  mean<-mean(x)
  zs<- sigma*qnorm(1-alpha/2,mean=0, sd=1,
                   lower.tail = TRUE)/sqrt(n)
  result<-mean+c(-zs,zs)
  return(result)
  # qnorm(p,...): quantile function
  # lower.tail = TRUE: probabilities are P[X ≤ x]
}

# 下面通过例子看一下在R中
# 如何去求置信度为 1- \alpha 的置信区间.

# e.g.
# 一个人10次称自己的体重(单位:斤): 
# 175 176 173 175 174 173 173 176 173 179, 
# 我们希望估计一下他的体重. 
# 假设此人的体重服从正态分布,标准差为1.5, 
# 我们要求体重的置信水平为95%的置信区间.

# 解
x<-c(175 ,176 ,173,175,174,173,173,176,173,179 )
conf.int(x,10,1.5,0.05)
# 或
# result<-z.test(x,10,1.5,0.05)
# result$conf.int
 
# 主程序
 z.test<-function(x,n,sigma,alpha,u0=0,alternative="two.sided"){
   
   options(digits=4)
   result<-list( )
   mean<-mean(x)
   z<-(mean-u0)/(sigma/sqrt(n))
   p<-pnorm(z,lower.tail=FALSE)
   result$mean<-mean
   result$z<-z
   result$p.value<-p
   if(alternative=="two.sided"){
     p<-2*p
     result$p.value<-p
   }
   else if (alternative == "greater"|alternative =="less" ){
     result$p.value<-p
   }
   else return("your input is wrong")
   #
   result$conf.int<- c(
     mean-sigma*qnorm(1-alpha/2,mean=0, sd=1,
                      lower.tail = TRUE)/sqrt(n),
     mean+sigma*qnorm(1-alpha/2,mean=0, sd=1,
                      lower.tail = TRUE)/sqrt(n))
   #
   result
 }
 
 # 此程序还可用于以后单正态总体均值 \mu 的假设检验, 
 # 之所以在程序中同时完成区间估计与假设检验,
 # 是为了与R中的t检验函数t.test( )相对应. 
 # 实际上, 可以从上面的程序中抽出区间估计的部分, 
 # 也就是我们的区间估计程序
