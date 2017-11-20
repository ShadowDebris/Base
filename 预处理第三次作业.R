#1.读入数据及数据准备
##（1）激活数据集
setwd("C:/Users/h/Desktop/bigdata_baes/tidy/third/homeword3")
wave <- read.csv("siswave.csv",sep = ",",header = T)
attach(wave)
dim(wave)
##（2）生成earnings新变量列
r <- rearn
t <- tearn
r[r<0] <- NA
t[t<0] <- NA
wave$earnings <-(r+t)/1000
attach(wave)
sum(is.na(earnings))/length(earnings)*100
##（3）生成white新变量列
wave$white <- ifelse(race==1,1,0)
white[is.na(white)] <- 0
##（4）生成male新变量列
any(is.na(sex))
wave$male <- ifelse(sex==1,1,0)
##（5）生成over65新变量列
any(is.na(r_age))
wave$over65 <- ifelse(r_age>65,1,0)
##（6）immig缺失值插补
immig[is.na(immig)] <- 0
##（7）educ_r替换
educ_r[is.na(educ_r)] <- 2.5
##（8）生成any.ssi，any.welfare，any.charity新变量列
any(is.na(ssi))
any(is.na(welfare))
any(is.na(charity))
wave$any_ssi <- ifelse(ssi>0,1,0)
wave$any_welfare <- ifelse(welfare>0,1,0)
wave$any_charity <- ifelse(charity>0,1,0)
#2.缺失值模式探索
##（1）生成sis.sm新数据框
attach(wave)
sis.sm <- data.frame(sex,race,educ_r,r_age,earnings,police)
detach(wave)
attach(sis.sm)
head(sis.sm)
sex <- as.factor(sex)
race <- as.factor(race)
police <- as.factor(police)
r_age <- as.numeric(r_age)
earnings <-as.numeric(earnings)
table(educ_r)
educ_r <- ordered(educ_r)
str(educ_r)
##（2）判断earnings是否是MCAR
na.fail(sis.sm)
n1 <- nrow(sis.sm[race==1,])#（1=white, 2=black, 3=hispanic(nonblack), 4=other）
n2 <- nrow(sis.sm[race==2,])
n1/nrow(sis.sm)
n2/nrow(sis.sm)
##（3）使用mice包里的md.pattern()函数查看数据的缺失模式
library(lattice)
library(mice)
library(VIM)
md.pattern(sis.sm)
aggr(sis.sm, prop=FALSE, numbers=TRUE)
matrixplot(sis.sm) 
marginplot(sis.sm[c("r_age","earnings")], pch=c(20),
           col=c("darkgray","red","blue"))
##（4）删除缺失的观测
any(is.na(sis.sm))
md.pattern(sis.sm)
mean(!complete.cases(sis.sm)) 
sum(!complete.cases(sis.sm))
sum(complete.cases(sis.sm))
sis.sm <- na.omit(sis.sm)
#3.简单随机插补
detach(sis.sm)
attach(wave)
random_impute <- function(a)
{
  if (any(is.na(a))){
    i = sample(which(!is.na(a)),1)
    a[is.na(a)] <- a[i]
    return(a)
  }
  else{
    return(a)
  }  
}
earnings_impute <- random_impute(earnings)
any(is.na(earnings_impute))
detach(wave)
#4.回归插补
##（1）生成earnings_top
attach(wave)
wave$earnings_top <- ifelse(earnings>100,100,earnings)
##（2）生成workhrs_top
wave$workhrs_top <- ifelse(workhrs>40,40,workhrs)
##（3）生成SIS数据框
SIS <- data.frame(earnings,earnings_top,male,over65,white,immig,educ_r,workmos,workhrs_top,any_ssi,any_welfare,any_charity)
##（4）建立回归模型1
detach(wave)
str(SIS)
lm_impute1 <- lm(earnings~male+over65+white+immig+educ_r+workmos+workhrs_top+any_ssi+any_welfare+any_charity,
                 data = SIS[earnings!=0,],na.action = "na.omit")
##（5）用回归模型1进行插补
SIS$unset1=ifelse(complete.cases(SIS$earnings),"notNA","imputation1")
SIS$earnings[is.na(SIS$earnings)] = predict(lm_impute1, newdata = SIS[is.na(SIS$earnings),])
library(ggplot2)
any(is.na(SIS$earnings))
table(SIS[earnings!=0,]$unset1)
##（6）建立回归模型2
lm_impute2 <- lm(sqrt(earnings_top)~male+over65+white+immig+educ_r+workmos+workhrs_top+any_ssi+any_welfare+any_charity,
                data = SIS)
##（7）对比插补前与插补后的直方图
SIS$unset2=ifelse(complete.cases(SIS$earnings_top),"notNA","imputation2")
SIS$earnings_top[is.na(SIS$earnings_top)] = predict(lm_impute2, newdata = SIS[is.na(SIS$earnings_top),])
any(is.na(SIS$earnings_top))
table(SIS[SIS$earnings_top!=0,]$unset2)
ggplot(SIS[SIS$earnings_top!=0,], aes(earnings_top, fill = unset2)) +
  geom_histogram(alpha = 0.5, position = 'identity')+theme(panel.grid =element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("修正后的收入/$1000")+ylab("频数")+ggtitle("插补前后收入分布对比直方图（除去0收入）")+
  theme(legend.title=element_blank())+
  scale_fill_discrete(breaks = c('notNA','imputation2'), labels = c('非插补值','插补值'))
#5.随机回归插补
rm(SIS)
SIS <- data.frame(earnings,earnings_top,male,over65,white,immig,educ_r,workmos,workhrs_top,any_ssi,any_welfare,any_charity)
lm_impute2 <- lm(sqrt(earnings_top)~male+over65+white+immig+educ_r+workmos+workhrs_top+any_ssi+any_welfare+any_charity,
                 data = SIS)
SIS$unset2=ifelse(complete.cases(SIS$earnings_top),"notNA","imputation2")
SIS$earnings_top[is.na(SIS$earnings_top)] = predict(lm_impute2, newdata = SIS[is.na(SIS$earnings_top),])+
  rnorm(1,mean = 0,sd = summary(lm_impute2)$sigma)
any(is.na(SIS$earnings_top))
table(SIS[SIS$earnings_top!=0,]$unset2)
ggplot(SIS[SIS$earnings_top!=0,], aes(earnings_top, fill = unset2)) +
  geom_histogram(alpha = 0.5, position = 'identity')+theme(panel.grid =element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("修正后的收入/$1000")+ylab("频数")+ggtitle("插补前后收入分布对比直方图（除去0收入）")+
  theme(legend.title=element_blank())+
  scale_fill_discrete(breaks = c('notNA','imputation2'), labels = c('非插补值','插补值'))
#6.两阶段插补
attach(wave)
rm(SIS)
SIS <- data.frame(earnings,earnings_top,male,over65,white,immig,educ_r,workmos,workhrs,workhrs_top,any_ssi,any_welfare,any_charity)

glm_indicator <- glm(I(earnings>0)~male+over65+white+immig+educ_r+workmos+workhrs_top+any_ssi+any_welfare+any_charity,
                     data = SIS,family = binomial(link = logit))
if (SIS$workhrs == 0 & SIS$workmos == 0) SIS$earnings <- 0
SIS$earnings[is.na(SIS$earnings)]<- predict(glm_indicator, newdata = SIS[is.na(SIS$earnings),])
#7.迭代回归插补
attach(wave)
ITE <- data.frame(interest,earnings,male,over65,white,immig,educ_r,workmos,
                  workhrs_top,any_ssi,any_welfare,any_charity)
detach(wave)
ITE$interest[ITE$interest < 0] <- NA
matrixplot(ITE)
IDE = ITE
ITE$c1=ifelse(is.na(ITE$interest),"imputation","notNA")
ITE$c2=ifelse(is.na(ITE$earnings),"imputation","notNA")
IDE$interest[is.na(IDE$interest)] <- mean(IDE$interest[!is.na(IDE$interest)])
IDE$earnings[is.na(IDE$earnings)] <- mean(IDE$earnings[!is.na(IDE$earnings)])
IDE$interest <- random_impute(IDE$interest)
IDE$earnings <- random_impute(IDE$earnings)
noe1 <- which(ITE$c1=="imputation")
noe2 <- which(ITE$c2=="imputation")
ite_impute1 <- function(sam1){
  sam1[,1][noe1] <- NA
  iimp=mice(sam1,method="norm.nob",m=1,maxit=1,seed=1)
  iimp=complete(iimp)
  return(iimp[,1])
}
ite_impute2 <- function(sam1){
  sam1[,2][noe2] <- NA
  eimp=mice(sam1,method="norm.nob",m=1,maxit=1,seed=1)
  eimp=complete(eimp)
  return(eimp[,2])
}
k = 0
ed1 <- sum(IDE$interest)
ed2 <- sum(IDE$earnings)
ed1+ed2
noom <- c(1,2,3)
while (sd(noom) >= 1) {
  k = k+1
  idemo <- ite_impute1(IDE)
    ed1 = sum(abs(IDE$interest - idemo))
  IDE$interest <- idemo
  edemo <- ite_impute2(IDE)
    ed2 = sum(abs(edemo - IDE$earnings))
  IDE$earnings <- edemo
  print(k)
  noom[k] <- (ed1+ed2)
  print(sd(noom))
}

