setwd("E:/学习资料/研究生学习/统计计算/彩蛋")
#install.packages("tm")
#install.packages("tmcn")
library(tmcn)
library(wordcloud)
library(wordcloud2)

hlzj <-readLines("个性签名1.txt",encoding ="UTF-8")  
length(hlzj)
hlzjTemp <- gsub("[0-9０１２３４５６７８９ < > ~ / \ \" ? . 。= -.◣]","",hlzj)  
insertWords("女孩纸")
hlzjTemp <- segmentCN(hlzjTemp)  
hlzjTemp[1:2]  
stopwords<- unlist(read.table("stopword.txt",stringsAsFactors=F,quote = ""))  
stopwords[50:100]  
removeStopWords <- function(x,stopwords) {  
  temp <- character(0)  
  index <- 1  
  xLen <- length(x)  
  while (index <= xLen) {  
    if (length(stopwords[stopwords==x[index]]) <1)  
      temp<- c(temp,x[index])  
    index <- index +1  
  }  
  temp  
} 
hlzjTemp2 <-lapply(hlzjTemp,removeStopWords,stopwords)  
hlzjTemp2[1:2]  

words <- lapply(hlzjTemp2,strsplit," ")  
wordsNum <- table(unlist(words))  
wordsNum <- sort(wordsNum) #排序  
wordsData <- data.frame(words =names(wordsNum), freq = wordsNum)  
weibo.top150 <- tail(wordsData,100) #取前100个词
weibo.top150=weibo.top150[,c(1,3)]
wordcloud(weibo.top150$words,weibo.top150$freq) 



