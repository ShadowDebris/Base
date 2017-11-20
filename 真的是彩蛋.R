library(rgl)
library(cluster)

#建立函数
eggs <- function(cen, a = 1,b = 1,c = 1,n = 65, ...){
  f <- function(s,t){ 
    cbind(   a * cos(t)*cos(s) + cen[1],
             b *        sin(s) + cen[2],
             c * sin(t)*cos(s) + cen[3])
  }
  persp3d(f,slim = c(-pi/2,pi/2), tlim  = c(0, 2*pi),  n = n, add = T,...)
}
open3d()#打开图形界面
set.seed(123)#生成一个菜蛋
n <- 1
for (i in 1:n){
  cen <- 3*runif(3)
  a <- 6
  b <- 6
  c <- 7
  elpf <- eggs(cen,a=a,b=b,c=c,col=rainbow)
}

#不够还可以加
open3d()
set.seed(100)
n <- 10
for (i in 1:n){
  cen <-400*runif(3)
  a <- runif(1,1,10)
  b <- runif(1,1,10)
  c <- runif(1,1,10)
  j <- runif(1,1,10)
  elpf <- eggs(cen,a=a*j,b=b*j,c=c*j,col=rainbow,alpha=0.4)
}


