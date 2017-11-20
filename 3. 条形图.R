# 条形图
#
# Yuehan Yang. 2015/3/16 yyh@cufe.edu.cn


# Simple bar plot
# 一类患有风湿性关节炎新疗法研究结果


# pause after each graph
par(ask = TRUE)

# save original graphic settings
opar <- par(no.readonly = TRUE)

# 读入程序包
library(grid)
library(vcd)

#将数据赋值
counts <- table(Arthritis$Improved)
counts


# 简单条形图
barplot(counts, main = "Simple Bar Plot", xlab = "Improvement", 
        ylab = "Frequency")

# 水平条形图
barplot(counts, main = "Horizontal Bar Plot", xlab = "Frequency", 
        ylab = "Improvement", horiz = TRUE)


# 列联表
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

# 堆砌条形图
barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts))

# 分组条形图
barplot(counts, main = "Grouped Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts), 
        beside = TRUE)

