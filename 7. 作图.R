# 作图
# 请参考随机变量脚本
#
# Yuehan Yang. 2014/7/20 yyh@cufe.edu.cn

x <- 1:50# 有没有人问我这个函数命令是什么？
#
?runif
# 50个来自0~2 之间的均匀分布的随机数
y <- runif(50,9,10) 
#
plot(x,y) # 这是最简单无脑的作图
#
plot(x,y,main ="Main title", sub="subtitle",
     xlab="x-label",ylab="y-label")
# main, sub, slab, ylab 分别对应主标题，副标题（底部），X轴/Y轴名称。
#
# 更多的
text(0.6,0.6,"text at (0.6,0.6)")
abline(h=.6,v=.6)
# 放置需要的点或线
#
# 这一节只是给出一个简单的作图例子，下节课会进行复杂的画图函数介绍。
# mtext 函数
# for (side in 1:4) mtext (-1:4,side=side,at=.7,line=-1:4)
# mtext(paste("side",1:4), side=1:4, line=-1, font=2)
