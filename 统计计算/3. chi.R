# 卡方检验
# 1. 单正态总体参数的假设检验
# 1.3 方差检验
#
# Yuehan Yang. 2014/10/22 yyh@cufe.edu.cn

# 卡方检验函数：chisq.var.test()
chisq.var.test <- function (x,var,alpha,alternative="two.sided"){
  options(digits=4)
  result<-list( )
  n<-length(x) # 长度
  v<-var(x) # 样本方差
  chi2<-(n-1)*v/var # 统计量
  result$var<-v
  result$chi2<-chi2
  p<-pchisq(chi2,n-1,lower.tail = F) # 右侧 p 值
  if(alternative == "greater"){
    result$p.value<-p # 备择假设 \sigma > \sigma_0
  } else if (alternative=="less"){
    result$p.value <-1-p # 备择假设 \sigma < \sigma_0
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


# e.g.检查一批保险丝, 
# 抽出10根测量其通过强电流熔化所需的时间(单位: 秒)为: 
# 42, 65, 75, 78, 59, 71, 57, 68, 54, 55
# 假设熔化所需时间服从正态分布, 超过80为超标
# 对此构造检验 (取\alpha = 0.05).




















time<-c(42, 65, 75, 78, 59, 71, 57, 68, 54, 55)
chisq.var.test(time, 80, 0.05, alternative="less")
# 结论: 因为p值=0.8668>0.05