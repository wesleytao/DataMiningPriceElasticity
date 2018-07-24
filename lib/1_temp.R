setwd("C:/Users/Wesle/Documents/R")
library(foreign)
library(sqldf)

#Step1 从中dta取出数据存入origin里面

origin<-read.dta("KFC_for_12.dta")
summary(origin)


#从中选取出，day_id item_id 价，量金额 原价，总分组，细分组，分组名称，日期存入mydata

mydata<-sqldf("select day_id,store_code,item_id,sell_id,unit_sold,alc_price,sell_price,amount,combo_flag,category,sub_category,class,sub_class,category_desc,calendar_date
                        from origin",row.names=TRUE)


#修改时间,将日期字符串修改为时间
timeformat<-"%m/%d/%y"
mydata$calendar_date<-as.Date(mydata$calendar_date,timeformat)



#Step2 制作一张物品种类表，详细记录物品的名称，所属种类，存入CSV文件中

###发现没有other 分类的产品，同时other产品里面的sub_category 42 一部分属于other 一部分属于dessert，剔除other产品

table(mydata$category,mydata$sub_category)
mydata<-subset(mydata, category!=13)



category<-sqldf("select category,sub_category,category_desc,count(distinct(item_id))as item_number
		     from mydata 
		     group by sub_category
		     order by category")
category
write.csv(category,"category.csv")





#物品详细记录180种产品的 item_id 的列表

item<-sqldf("select category,sub_category,category_desc,item_id,alc_price
		 from mydata
		 group by item_id
		 order by category,sub_category")
item


#想要把item里面的item_id，以字符串连接的格式放到category里面
a<-""
for (i in 1:length(category$category)){ c<-""
	for(j in 1:length(item$category)){
		if (item$sub_category[j]==category$sub_category[i])
				
				c<-paste(c,item$item_id[j],sep=",")

}
a<-c(a,c)
}
a

b<-c(a,"sdfsetr")


#剔除第一个空值
item_all<-subset(a,a!="")

item_all
write.csv(item_all,"item_all.csv")







#Step 3 找出每一个category里面销量最高的那个的ID####
#3.1求出每种物品的总销量
sum_item_sold<-sqldf("select sum(unit_sold)as sum_sold, item_id,category,category_desc
				from mydata
				group by item_id")


#求出每个种类下总销量最高的id
Max_sum_in_category_id<-sqldf("select item_id,category,category_desc 
					from sum_item_sold
					where sum_sold in (select max(sum_sold) 
									from sum_item_sold 
									group by category)
")

write.csv(Max_sum_in_category_id,"max_sum_item_id.csv")


####################################

#3.2求出这个id的销量，价格，原价，时间,每天的销量，不考虑卖出的方式#######

best_seller<-sqldf("select item_id,category,category_desc,unit_sold,alc_price,amount,calendar_date
				from mydata 
				where item_id in (select item_id
							from Max_sum_in_category_id)
				")



write.csv(Max_sum_all_in,"Max_sum_item_sold.csv")


##3.3 需要合并每日的销量
best_seller_daily<-sqldf("select item_id,calendar_date,sum(unit_sold) as daily_sell from best_seller group by calendar_date,item_id")


fix(best_seller)
write.csv(best_seller,"best_seller.csv")
write.csv(best_seller_daily,"best_seller_daily.csv")


###3.4 转换成纵坐标为date，横坐标为item_id,值为销量的表
library(reshape2)
library(plyr)

temp<-melt(best_seller_daily,id=c("calendar_date","item_id"))
best_seller_daily<-dcast(temp,calendar_date~item_id)

write.csv(best_seller_daily,"daily_sell_of_6_item.csv")


###4 绘出销量的折线图12345
#4.1 将变量名以字符串的形式取出来，并且添加 ID
names(best_seller_daily)
for (i in 2:7){
names(best_seller_daily)[i]<-paste("ID",names(best_seller_daily)[i],sep="")

}

#4.2 尝试画一张图,将日期转化为字符型变量出现了问题,后来发现是因为重名的原因
summary(best_seller_daily)
timeformat<-"%Y-%m-%d"
best_seller_daily$calendar_date<-as.Date(best_seller_daily$calendar_date,timeformat)


#4.3 利用ggplot2制作一张月销量折线图 挑选ID5373
library(ggplot2)

attach(best_seller_daily)
plot(clendar_date2,ID5373,type="b")





#5 看不同卖出方式的原价是否不同

Item_6040<-sqldf("select calendar_date,item_id,alc_price,sell_id from origin where item_id='6040'")
#发现有许多原价为0元的，不明白情况？以及价格变动
attach(Item_6040)
table(sell_id,alc_price)
summary(Item_6040)
detach(Item_6040)

#看每一天卖出方式不同,价格原价是否不同
Item_6040<-sqldf("select * from origin where item_id='6040' order by calendar_date")
newdata<-Item_6040[1:100,]
attach(newdata)
newdata2<-newdata[order(calendar_date,store_code,sell_id,alc_price),]

detach(newdata)
#定下 （店，sell-id 来看原价还有卖价 和日期）,有问题 无法用subset的原因可能是Item_id 为list对象
close_look<-sqldf("select calendar_date,sell_id,store_code,alc_price,sell_price,combo_flag,item_code,unit_sold from Item_6040 where store_code='SHA227'  order by calendar_date")


close_look<-close_look[order(calendar_date,sell_id),]

#再从中挑选出原价大于10元的
close_look<-sqldf("select * from close_look where alc_price>20")
#如果确定是套餐价格的话，我们可以在当天找到很多价格是69的,看一天的卖出的东西,看不出来套餐的对应关系，而且如果是套餐的话
#那么应该价格都很贵，所以套餐的化应该是单价而非套餐价格 
#那么为什么有的商品卖的非常非常贵？只可能是数量不同，以及外卖还是折扣券

check_combo<-sqldf("select * from mydata where calendar_date='10/13/13' and store_code='SHA227' and combo_flag='2' order by item_id")

#但是我看到了 应该以store +day 形成的作为关联规则？？？

