##----UTF-8----##
library(psych)
library(corrplot)
setwd("C:/Users/h/Desktop/bigdata_baes/2017《大数据统计基础》考试题")
footset <- read.csv("英超数据.csv",header = T)
#对数据进行预览，了解记录量、变量数量和变量名，以及变量类型
head(footset)
any(is.na(footset))
dim(footset)
names(footset)
str(footset)
summary(footset)

#变量数量：34
#变量名称与变量类型、取值范围 
 #*球员基本信息：
 #"球员"           "年龄"           "球队"          
 #"号码"           "位置"           "出场"          
 #"首发"           "出场时间"       
  #基本竞技成绩指标
 #"进球"          
 #"助攻"           "传球"           "过人"          
 #"抢断"           
 #犯规类指标：
 #"越位"           "犯规"          
 #"红牌"           "黄牌"           
 #进攻性数据指标：
 #"射门"          
 #"射正"           "射门成功率"     "头球进球"      
 #"左脚进球"       "右脚进球"       "直接任意球进球"
 #"点球"           "赢得点球机会"   
 #其他竞技数据指标：
 #"拦截"          
 #"解围"           "头球解围"       "后场解围"      
 #"头球争顶成功"   "头球争顶失败"   "乌龙球"        
 #"下一年进球" 
#相关性分析
any(is.na(footset))#是否存在缺失值
nearZeroVar(footset[,-c(1,3,5)])#是否存在低方差预测变量
apply(footset[,-c(1,3,5)],2, sd)#去除头顶球
qty_set <- footset[,-c(1,3,5,29,32,34)]
qtycor <- cor(qty_set)
corrplot(qtycor,order = "hclust",tl.cex = 0.7)
qty_set <- qty_set[,!(names(qty_set)
                      %in% c('号码','红牌','射门成功率','乌龙球','年龄'))]
library(GGally)
base <- footset[,c(5,6,7,8,9,10,11,12,13)] 
ggscatmat(base,columns = 2:9,color = "位置")

sqty_set <- scale(qty_set)
foot_pca <-princomp(sqty_set,cor = T)
summary(foot_pca)
plot(foot_pca,type="lines",main = "碎石图")
cbind(round(foot_pca$loadings[,1:6],3))
foot_pca$loadings[,1] <- foot_pca$loadings[,1]*-1
cbind(round(foot_pca$loadings[,1:6],3))
scores <- round(foot_pca$scores[,1:6],2)
head(scores)
footset[,35:40] <- foot_pca$scores[,1:6]
#对应分析
names(footset)[35:40] <- c("score1","score2","score3","score4","score5","score6")
ggplot(data = footset,aes(x=score1,y=score2,colour=footset$球队))+
  geom_point(size=2)+
  xlab("第一主成分得分")+ylab("第二主成分得分")+
  facet_wrap(~footset$球队,ncol = 4) 
pre_goal <- footset[,34:40]
names(pre_goal) <- c('Y','X1','X2','X3','X4','X5','X6')
lm_fit=lm(Y~.,data = pre_goal)
summary(lm_fit)
#逐步回归
lm_step <- step(lm_fit,direction = 'both',trace = 0)
summary(lm_step)
summary(lm_step)$call
