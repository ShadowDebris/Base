# 直方图
#
# Yuehan Yang. 2015/3/16 yyh@cufe.edu.cn


# 直方图
# 数据来源——R内嵌数据集mtcars
# 美国Motor Trend收集的1973年到1974年中共32辆
# 汽车的11个指标：油耗blabla

par(mfrow = c(2, 2))
# plot(mtcars$mpg)
hist(mtcars$mpg)

hist(mtcars$mpg, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Colored histogram with 12 bins")

# 增加密度曲线
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)

# 直方图与正态曲线
x <- mtcars$mpg
h <- hist(x, breaks = 12, col = "red", 
          xlab = "Miles Per Gallon", 
          main = "Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
box()

# restore original graphic parameters
par(opar)

