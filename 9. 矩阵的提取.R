# 总结与练习2
#
# Yuehan Yang. 2014/7/18 yyh@cufe.edu.cn

scores <- c(100,99,98)
#
scores[2] # 检索
#
scores[2] = 100 # 修改
#
scores2 = scores[-2] # 删除第二项元素
#
scores[c(2,3,2,1,2)] # 更复杂的检索方式
#
scores[-c(1,3)] # 更复杂的删除方式


#注：有没有注意到最后的检索和删除并没有更改原目标 scores ?
