#clustering
setwd("C:/Users/Wesle/Documents/R")
rm(list=ls())
library(foreign)
maincluster<-read.csv("mainfoodcor.csv")
names<-names(maincluster[-1])
maincluster<-data.matrix(maincluster[-1])
row.names(maincluster)<-names
#ת����matrix

# install.packages("flexclust")
library(flexclust)

# names<-c("a","b","c")
# as.dist(matrix(1:9,nrow = 3,ncol = 3,dimnames=list(names,names)))#ȡ���������Ǿ����ֵ

#ȡ�෴��,����Խ��˵��Խ���������Ʒ�� ����Խ�� ��ˣ�Ӧ�ü�1
d<-as.dist(maincluster+1)
d.single<-hclust(d,method="average")
plot(d.single,hang=-1,cex=0.8,main="Average-linkages Clustering for 10 Mainfood")

d.single<-hclust(d,method="complete")
plot(d.single,hang=-1,cex=0.8,main="Complete-linkages Clustering for 10 Mainfood")


d.single<-hclust(d,method="single")
plot(d.single,hang=-1,cex=0.8,main="Single-linkages Clustering for 10 Mainfood")


#������һ���Ƚ�ȫ��ľ���
#ò�ƾ���û�б�Ҫ����Ϊ��������Ҫ�����Ʒ��ʵ�������ϵ���Ϳ��Խ��

