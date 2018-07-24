# correlationship
# 2017/4/8 
# prepare for clusting  
#in_put daily_sell.csv 
#out_put correlation_category.csv

rm(list=ls())
library(foreign)
daily_sell<-read.csv("daily_sell.csv")#该分数据中没有剔除套餐类产品
summary(daily_sell)
#发现很多产品不少都没有卖，所以还是需要剔除一些数据
length(names(daily_sell[c(-1,-2)]))# 产品数 180款

#分组_字符串匹配
myvars<-names(daily_sell[c(-1,-2)])

Dessert<-grep("Dessert",myvars,ignore.case = F,fixed=TRUE)#
Drink<-grep("Drink",myvars,ignore.case = F,fixed=TRUE)#
Main<-grep("Main",myvars,ignore.case = F,fixed=TRUE)
Snack<-grep("Snack",myvars,ignore.case = F,fixed=TRUE)#
Non_food<-grep("Non",myvars,ignore.case = F,fixed=TRUE)#
Side_item<-grep("Side",myvars,ignore.case = F,fixed=TRUE)#


#销售天数统计
bool<-is.na(daily_sell[c(-1,-2)])
count<-apply(bool,2,sum)
days_of_sell<-length(daily_sell$X)-count


#列出相关系数矩阵
correlation<-cor(daily_sell[c(-1,-2)],use="pairwise.complete.obs",method="pearson")# 因为存在大量NA值，所以选择一对一对地看相关系数 即pairwise.complete.obs
# dim(correlation)#180 180

main_cor<-correlation[Main,Main]
snack_cor<-correlation[Snack,Snack]
nonfood_cor<-correlation[Non_food,Non_food]
drink_cor<-correlation[Drink,Drink]
dessert_cor<-correlation[Dessert,Dessert]
side_item_cor<-correlation[Side_item,Side_item]


#画马赛克图
library(corrgram)
#对负值感兴趣 出现的负值越多越好,同时必须NA比较少
bool<-(main_cor<0)
bool[is.na(bool)]<-FALSE
main_select<-sort(apply(bool,2,sum),T)
top_20<-names(main_select[1:20])

# NA最少的20个
bool<-is.na(main_cor)
main_select<-sort(apply(bool,2,sum),F)
top_20_minna<-names(main_select[1:20])

#取当中的交集
main_select<-intersect(top_20,top_20_minna)
pics<-main_cor[main_select,main_select]

setwd("C:/Users/Wesle/Documents/R")
write.csv(pics,"mainfoodcor.csv")
corrgram(pics,order=T,lower.panel=panel.shade,upper.panel = NULL,text.panel = panel.txt,main="Corrgram of top 10 Main Food")




