#原始的10折交叉验证函数
cv <- function(x,y,folds = 10, maxlambda=1,minlambda=0){
  n <- dim(x)[1];p <- dim(x)[2]
  q <- sample(n,n)
  x <- x[q,];y <- y[q]
  nlambda <- seq(minlambda,maxlambda,length.out=100)
  err <- rep(0,100)
  for(i in 1:100){
    k <- n/folds 
    result1=rep(NULL,10)
    for(j in 1:folds){
      ytest <- y[((j-1)*k+1):(j*k)]
      xtest <- x[((j-1)*k+1):(j*k),]
      yfit <- y[-(((j-1)*k+1):(j*k))]
      xfit <- x[-(((j-1)*k+1):(j*k)),]
      result <- lars(xfit,yfit)
      yhat <- predict(result,xtest,s = nlambda[i],
                      mode = "lambda",type = "fit")$fit
      result1[j]<- mean((ytest - yhat)^2)
    }
    err[i] <- mean(result1)
  }
  k <- which.min(err);er <- min(err)
  lambda <- nlambda[k]
  return(list(lambda = lambda,mse = err))
}
#用apply函数优化后的交叉验证
mse<-function(x,y) mean((y-x)^2)
cvfolds=function(i,x,y,lambda,k){
  i<-(k*(i-1)+1):(k*i)
  result<-lars(x[-i,],y[-i])
  yhat<-predict(result,x[i,],s=lambda,
                mode="lambda",tpye="fit")$fit
  result1<-apply(yhat,2,mse,y[i])
  return(result1)
}
#k折交叉验证(可以整除时)
cv<-function(x,y,folds,maxlambda=1,minlambda=0){
  n<-dim(x)[1];p<-dim(x)[2]
  q<-sample(n,n)
  x<-x[q,];y<-y[q]
  nlambda<-seq(minlambda,maxlambda,length.out = 100)
  k1<-n/folds 
  xx<-matrix(1:folds,folds)
  result2<-apply(xx,1,cvfolds,x,y,nlambda,k1)#第一次循环是取前20个样本作为测试集，其余作为训练集，计算不同lambda下的误差，返回值是一个含10个元素的列表，每个列表有100个误差值
  result2<-unlist(result2) 
  result2<-matrix(result2,100,folds)#将列表打散后转化成一个100行10列的矩阵
  error<-apply(result2,1,mean)#返回一个长度为100的向量，也就是每个lambda值对应的模型的误差
  inx <- which.min(error)
  lambda <- nlambda[inx]
  return(list(lambda = lambda,mse = error))
}

#k折交叉验证(不能整除时)
cvfolds1=function(i,x,y,lambda,n){
  i<-((n%/%i)*(i-1)+1):n
  result<-lars(x[-i,],y[-i])
  yhat<-predict(result,x[i,],s=lambda,
                mode="lambda",tpye="fit")$fit
  result1<-apply(yhat,2,mse,y[i])
  return(result1)
}
cv<-function(x,y,folds,maxlambda=1,minlambda=0){
  n<-dim(x)[1];p<-dim(x)[2]
  q<-sample(n,n)
  x<-x[q,];y<-y[q]
  nlambda<-seq(minlambda,maxlambda,length.out = 100)
  if (n%%folds==0){
    k1<-n/folds 
    xx<-matrix(1:folds,folds)
    result2<-apply(xx,1,cvfolds,x,y,nlambda,k1)
    result2<-unlist(result2) 
    result2<-matrix(result2,100,folds)
  }else{
    k1<-n%/%folds 
    xx<-matrix(1:(folds-1),(folds-1))
    result2<-apply(xx,1,cvfolds,x,y,nlambda,k1)
    result2<-unlist(result2) 
    result2<-matrix(result2,100,folds-1)
    result3<-as.matrix(cvfolds1(folds,x,y,nlambda,n))
    result2<-cbind(result2,result3)#一个100行10列的矩阵
  }
  error<-apply(result2,1,mean)#得到一个长度为100的向量
  inx <- which.min(error)
  lambda <- nlambda[inx]
  return(list(lambda = lambda,mse = error))
}



