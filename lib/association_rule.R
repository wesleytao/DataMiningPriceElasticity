# Association rules
# 2016/4/3 
#author:wesley tao 
# data import: association_rule.csv
# data export:target
# wd: R
#encoding UTF-8

rm(list=ls())
library(foreign)
setwd("C:/Users/Wesle/Documents/R")
association_rule<-read.csv('association_rule.csv')

#step 1横坐标 calenda date store code 纵坐标ID，  转化成newdata

drop<-names(association_rule)%in%"X"
mydata<-association_rule[!drop]#删除X
rm(drop)

temp<-melt(mydata,id=c("calendar_date","store_code","ID"))
newdata<-dcast(temp,calendar_date+store_code~ID)
rm(temp,mydata)
newdata$calendar_date<-as.Date(newdata$calendar_date,"%m/%d/%y")#转换日期格式
newdata_sort<-newdata[order(newdata$store_code,newdata$calendar_date),]#按照日期排序
rm(newdata)

# View(newdata_sort)#发现店与店之间有差异，即有些item——id只在这家店卖
#按照店来拆分
library(sqldf)
SHA227<-sqldf("select * from newdata_sort where store_code='SHA227' ")
SHA227<-SHA227[-2]#把storecode删了
SHA312<-sqldf("select * from newdata_sort where store_code='SHA312'")
SHA312<-SHA312[-2]

# SHA227$calendar_date<-as.Date(SHA227$calendar_date,origin="1970-1-1")
# SHA312$calendar_date<-as.Date(SHA312$calendar_date,origin="1970-1-1")



#project 看销量的相对变动  拆分成两家店 227 and 312

#SHA227
#NA 替换成0,并且做差分
temp<-SHA227[-1]
temp[is.na(temp)]<-0#将NA全部替换成0
myvars<-names(temp)%in%"calendar_date_1"
temp<-temp[!myvars] #基本没有变化
SHA227_edit<-cbind(SHA227$calendar_date,temp)
rm(temp,myvars)

diff_SHA227<-apply(SHA227_edit[-1],2,diff)#按照列做差分
diff_SHA227[diff_SHA227>0]<-TRUE
diff_SHA227[diff_SHA227<=0]<-F
rm(SHA227_edit)
#diff_SHA227 是bool矩阵
library(arules)
SHA227_diff<-as(diff_SHA227,"transactions")
rm(diff_SHA227,myvars)

summary(SHA227_diff)
# 看店铺227的关联规则
rulesdiff.227<-apriori(SHA227_diff,parameter=list(minlen=2,maxlen=3))
summary(rulesdiff.227)
rulesdiff.227<-sort(rulesdiff.227,decreasing = T,by="support")#按照lift 排序
inspect(rulesdiff.227[1:10])




#SHA312
#NA 替换成0,并且做差分
temp<-SHA312[-1]
temp[is.na(temp)]<-0#将NA全部替换成0
myvars<-names(temp)%in%"calendar_date_1"
temp<-temp[!myvars] #基本没有变化
SHA312_edit<-cbind(SHA312$calendar_date,temp)
rm(temp,myvars)

diff_SHA312<-apply(SHA312_edit[-1],2,diff)#按照列做差分
diff_SHA312[diff_SHA312>0]<-TRUE
diff_SHA312[diff_SHA312<=0]<-F
rm(SHA312_edit)
#diff_SHA312 是bool矩阵
library(arules)
SHA312_diff<-as(diff_SHA312,"transactions")
rm(diff_SHA312,myvars)

summary(SHA312_diff)
# 看店铺312的关联规则
rulesdiff.312<-apriori(SHA312_diff,parameter=list(minlen=2,maxlen=3))
summary(rulesdiff.312)
rulesdiff.312<-sort(rulesdiff.312,decreasing = T,by="confidence")#按照lift 排序
inspect(rulesdiff.312[1:10])

myvars<-c(5977,6106,5992,11399,5969,5306,5976,6107,5968,12324)
myvars<-as.character(myvars)

#编程实现去找这些


x1<-as(rulesdiff.227[1],"data.frame")
for (i in 1:10){
  a<-subset(rulesdiff.227,lhs%pin%myvars[i])
  c<-as(a,"data.frame")
  x1<-rbind(x1,c)
  print(x1)
  
}

plot(rulesdiff.227,method="graph",control = list(type="items"))
plot(rulesdiff.312,method="graph")



missed<-subset(rulesdiff.312,lhs%pin%myvars[1])
missed_data<-as(missed,"data.frame")
x1<-rbind(x1,missed_data)

write.csv(x1,"target_10main_rules.csv")
# 
# inspect(rulesdiff.227)
# inspect(rulesdiff.312)

# 
subset(rulesdiff.227,lhs%pin%myvars[1])
# subset(rulesdiff.227,lhs%pin%myvars[2])#1
# subset(rulesdiff.227,lhs%pin%myvars[3])#2
# subset(rulesdiff.227,lhs%pin%myvars[4])#5
# subset(rulesdiff.227,lhs%pin%myvars[5])#3
# subset(rulesdiff.227,lhs%pin%myvars[6])#2
# subset(rulesdiff.227,lhs%pin%myvars[7])
# subset(rulesdiff.227,lhs%pin%myvars[8])
# subset(rulesdiff.227,lhs%pin%myvars[9])
# subset(rulesdiff.227,lhs%pin%myvars[10])
# 

# subset(rulesdiff.312,lhs%pin%myvars[2])#0
# subset(rulesdiff.312,lhs%pin%myvars[3])#
# subset(rulesdiff.312,lhs%pin%myvars[4])#
# subset(rulesdiff.312,lhs%pin%myvars[5])#
# subset(rulesdiff.312,lhs%pin%myvars[6])#
# subset(rulesdiff.312,lhs%pin%myvars[7])
# subset(rulesdiff.312,lhs%pin%myvars[8])
# subset(rulesdiff.312,lhs%pin%myvars[9])
# subset(rulesdiff.312,lhs%pin%myvars[10])
# 
# 
# # rules.diff227  doesn't contain any targeted rules
# # x2<-as(rulesdiff.227[1],"data.frame")
# # for (i in 1:10){
# #   a<-subset(rulesdiff.312,lhs%pin%myvars[i])
# #   c<-as(a,"data.frame")
# #   x2<-rbind(x2,c)
# #   print("hahah")
# #   print(x2)
# # }





