# 变量赋值
# 最基本的命令：输入一个对象的名字来显示该对象的内容
# 优点：便于储存信息
# 常用变量名称：a-z, A-Z, (.), (_)
# 备注：区分大小写；
#       变量名不能包含空格；可以更改；
#       可以由字母、点号开头，但不能由数字开头
#             （点号开头的变量名是特殊的，尽量避免）；
#       数字后不能紧跟点号；
#       某些单字母变量名已被系统使用，命名时也应避免，
# e.g. c, q, t, C, D, F, I, T, diff, df, pt;
#       
# Yuehan Yang. 2014/7/18 yyh@cufe.edu.cn





x1 = 2 # matlab 赋值法
#
x2 <- 4 # R 常用赋值模式，<- 表示赋值运算符(assignment operator)
# 
20 -> x3 # 反向也是可以的
# 
assign("x4",30) # 其余方法
#
x5 <- x1+x2 # 赋值后进行运算



 
 
# 脚本运行小技巧：
 # 1. 快捷键： Ctrl+R
 # 2. Source on Save + save
 # 3. source("test2.R")  
  # 预先设置路径 e.g. setwd("C:/Users/yyh/Desktop/first class")
