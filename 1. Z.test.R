# Z检验
# 1. 单正态总体参数的假设检验
# 1.1 方差已知时\mu的检验
#
# Yuehan Yang. 2015/11/10 yyh@cufe.edu.cn

# Z检验函数
z.test<-function(x,n,sigma,alpha,u0=0,alternative="two.sided"){
  # x 目标
  # n 样本数量
  # sigma 标准差
  # alpha 1-显著性水平
  # u0 \mu_0
  # alternative 双边、单边检验
  options(digits=4)
  result<-list() # 将result赋值为列表形式
  mean<-mean(x)
  z<-(mean-u0)/(sigma/sqrt(n)) # 构造统计量
  p<-pnorm(z,lower.tail=FALSE) # 计算右侧的P值
  result$mean<-mean # 结果输出
  result$z<-z # 同上
  result$p.value<-p # 同上
  if(alternative=="two.sided"){ #双边检验
    if(p<0.5){
    p<-2*p}else{p<-2*(1-p)}
    result$p.value<-p
  }
  else if (alternative =="greater" ){
    result$p.value<-p # 备择假设 \mu >\mu_0
  }
  else if (alternative == "less"){
    result$p.value<- 1-p # 备择假设 \mu <\mu_0
  }
  else return("your input is wrong")
  ##
  
  result$conf.int<- c( # 置信区间
    mean-sigma*qnorm(1-alpha/2,mean=0, sd=1,
                     lower.tail = TRUE)/sqrt(n),
    mean+sigma*qnorm(1-alpha/2,mean=0, sd=1,
                     lower.tail = TRUE)/sqrt(n))
  #
  result
}



# e.g.微波炉在炉门关闭时的辐射量是一个重要的质量指标. 
# 设该指标服从正态分布N(\mu, 0.1^2), 均值要求为0.12. 
# 为检查近期产品的质量,从某厂生产的微波炉中抽查了8台, 
# 得其炉门关闭时辐射量为：
# 0.1,0.11,0.141,0.11,0.08,0.09,0.04,0.13
# 问对该厂生产的微波炉炉门关闭时辐射量的检验应该如何设置，结果如何




# 回忆一下单边假设的定义，包括且不限于
# 1. 打个比方，右侧检验对应什么情况？
# 2. 单边检验如何选择原假设与备择假设？
# 3. 结合1,2，思考假设检验对应实际问题（本题）应该如何运用
# 根据以上问题，解决这道题并给出最终结论













# 1. 单边检验对应备择假设
# 2. （1）把极端情况设置为原假设，e.g.产品不合格
# （2）把想要得到的结论放在备择假设
# （3）etc...
# 3. 自行思考

data<-c( 0.1,0.11,0.141,0.11,0.08,0.09,0.04,0.13)

z.test(data, length(data), 0.1, 0.05, u0=0.12, alternative="two.sided")
# p值=1.426>\alpha = 0.05, 
# 这里
#故不能拒绝原假设。
