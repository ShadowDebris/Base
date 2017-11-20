# 对象属性
#  所有对象都有两个内在属性：类型和长度
#      类型是对象元素的基本种类，
#      共有四种：数值型、字符型、
#                复数型（暂不作讨论）、逻辑型（False或True）
#      长度指的是对象中元素的数目。
#  对象的类型和长度分别通过函数 mode() 和 length() 获得
#
# Yuehan Yang. 2014/7/20 yyh@cufe.edu.cn


x <-1
# 检测：mode(x), length(x)
#
A <- "Gomphotherium"
#
compar <- TRUE
#
z <- 1i
# 测验：mode(A); mode(compar); mode(z)
#
# 函数 ls() 用于列出对象名
#
# 我们在上一个脚本里已经介绍了数值向量的赋值，那么接下来
#
# 字符向量：
# 它是一个字符串的向量，元素用引号来指定和输出
vector_A <- c("Huey", "Dewey", "Louie")
# 或单引号(但不经常使用)
vector_A <- c('Huey', 'Dewey', 'Louie')
#
# 逻辑向量
# 顾名思义，取值 True(T) 或 False(F)
vector_B <- c(T,T,F,T)
#
# 逻辑判断常用在函数调用中，e.g.
vector_x <- c(10.4, 5.6, 3.1 ,6.4, 21.7)
vector_x > 10
#
# 混搭型
vector_xx <- c(pi, "abc")
vector_xxx <- c(FALSE, 3)
