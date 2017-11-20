densplot<-function(){
  x = seq(from= -5, to = 5,length = 101)
  plot(x,dnorm(x,0,1),col="red",xlim=c(-5,5),ylim=c(0,1),type='l',
       ylab='density',xlab='',
       main="The Normal Density Distribution")
  lines(x,dnorm(x,0,0.5),col="green")
  lines(x,dnorm(x,0,2),col="blue")
  lines(x,dnorm(x,-2,1),col="orange")
  legend("topright",legend=paste("m=",
                                 c(0,0,0,-2)," sd=", c(1,0.5,2,1)),
         lwd=1, col=c("red", "green","blue","orange"))
}

