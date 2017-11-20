# 气泡图
# 什么是气泡图(bobble plot)
# 创建二维散点图，再用点的大小来表示第三个变量的值
# 
# Yuehan Yang. 2015/3/23 yyh@cufe.edu.cn

# 提前给出symbols的可选项：
# 圆圈、方形、星形、温度计、箱线
# 以下是圆圈图的例子
# （关于圆圈，可以考虑圆圈半径、或面积等等选择）


attach(mtcars)
r <- sqrt(disp/pi)
symbols(wt, mpg, r, inches=0.30, fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)
par(opar)
# 
# 最后PS，该图不适合用来统计分析，
# 作为一个数学大脑，分辨体积、面积都很困难
# 但，
# 你觉得一个PPT里，是直方图专业，还是泡泡图更吸引人注意？
# 商业应用中，该图应用非常广
#
# 曾经有一张泡泡图放在我面前，我是拒绝的……