# 三维散点图
# 一言以蔽之，三个变量的交互关系
#
# Yuehan Yang. 2015/3/23 yyh@cufe.edu.cn

# e.g 英里数、车重、排量交互作用
# install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
# 简版（一看就没有爱的……缺省版）
scatterplot3d(wt, disp, mpg, 
              main="Basic 3D Scatterplot")
# 修图版（萌版）
scatterplot3d(wt, disp, mpg, 
              pch=16, 
              highlight.3d=TRUE,
              type="h",
              main="3D Scatterplot with Vertical Lines")
# 同样，该图如何修出来的，作为今天做作业的同学的思考题

# 增加一个回归面
s3d <-scatterplot3d(wt, disp, mpg, 
                    pch=16, 
                    highlight.3d=TRUE,
                    type="h", 
                    main="3D Scatter Plot with Verical Lines and Regression Plane")
fit <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit)
# 简单介绍：平面代表预测值，图中的点是实际值，
# 平面到点的垂直距离表示残差值
# 1. 点在平面之上，预测值被低估
# 2. 反之，被高估
detach(mtcars)

# 继续装X版本
# 旋转三维散点图

install.packages("rgl")
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=5)

# 以及
rgl.open()
library(splines)
library(RcmdrMisc)
library(car)
library(sandwich)
library(Rcmdr)
attach(mtcars)

scatter3d(mpg ~ wt | cyl, ellipsoid=T,surface=F, revolutions=3, data=mtcars)


if(interactive() && require(rgl) && require(mgcv)){
  # scatter3d(prestige ~ income + education, data=Duncan)
  # Sys.sleep(1) # wait 5 seconds
  # scatter3d(prestige ~ income + education | type, data=Duncan)
  # Sys.sleep(1)
  scatter3d(prestige ~ income + education | type, surface=FALSE, 
            ellipsoid=TRUE, revolutions=3, data=Duncan)
  #  scatter3d(prestige ~ income + education, fit=c("linear", "additive"),
  #           data=Prestige)
  # Sys.sleep(1)
  # scatter3d(prestige ~ income + education | type, 
  #          radius=(1 + women)^(1/3), data=Prestige)
}
