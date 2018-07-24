#clustering
setwd("C:/Users/Wesle/Documents/R")
rm(list=ls())
library(foreign)
maincluster<-read.csv("mainfoodcor.csv")
names<-names(maincluster[-1])
maincluster<-data.matrix(maincluster[-1])
row.names(maincluster)<-names
#转化成matrix

# install.packages("flexclust")
library(flexclust)

# names<-c("a","b","c")
# as.dist(matrix(1:9,nrow = 3,ncol = 3,dimnames=list(names,names)))#取的是下三角矩阵的值

#取相反数,负数越大，说明越是属于替代品， 距离越近 因此，应该加1
d<-as.dist(maincluster+1)
d.single<-hclust(d,method="average")
plot(d.single,hang=-1,cex=0.8,main="Average-linkages Clustering for 10 Mainfood")

d.single<-hclust(d,method="complete")
plot(d.single,hang=-1,cex=0.8,main="Complete-linkages Clustering for 10 Mainfood")


d.single<-hclust(d,method="single")
plot(d.single,hang=-1,cex=0.8,main="Single-linkages Clustering for 10 Mainfood")


#可以做一个比较全面的聚类
#貌似聚类没有必要？因为我最终需要找替代品，实际上相关系数就可以解决


