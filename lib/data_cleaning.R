#数据清洗 data cleaning
# data improt: KFC_for_12.dta
#data export: 1. daily_sell.csv 2.  各个产品的销售天数统计.csv 3. association_rule.csv


rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")

library(sqldf)
library(foreign)

origin<-read.dta("KFC_for_12.dta")
origin$calendar_date<-as.Date(origin$calendar_date,"%m/%d/%y")
# summary(origin)

#step1 寻找主码
#tips总共多少条
sqldf("select count(*)from origin")#304879
sqldf("select count(distinct(calendar_date))from origin")#728 day
start_date<-sqldf("select distinct(calendar_date) from origin")


#tips看聚集函数的先后顺序有没有影响

aggregate1<-sqldf("select day_id,store_code,item_id,sell_id,count(*) from origin group by store_code,day_id,item_id,sell_id having count(*)>1")

# 加入alcprice

aggregate2<-sqldf("select unit_sold,count(*) from origin group by store_code,day_id,item_id,sell_id,alc_price,combo_flag,sub_class,unit_sold,sell_price having count(*)>2")

#看每天每家店卖出了多少种产品以及其id
item_daily_id<-sqldf("select all calendar_date,store_code, item_id from origin group by calendar_date,store_code,item_id")

check<-c("unit_sold","sell_price")
describe(origin[check])

#step 2 利用分组来查看每个产品id的销量和价格的描述性统计数据

myvars<-c("unit_sold","sell_price","alc_price")
describeBy(origin[myvars],list(am=origin$item_id))

too_small<-sqldf("select distinct item_id,count(*)as n,category_desc  from origin group by item_id having n<50")
write.csv(too_small,"销售记录小于50的产品id.csv")

#剔除销售记录小于50的产品
delete_item<-too_small$item_id

check<-sqldf("select calendar_date,unit_sold,item_id,category_desc from origin where item_id in (select item_id from too_small) order by item_id,calendar_date")

#非常重要：需要构造一个函数来输入 一组日期值 返回 开始日期 与结束日期,不需要构造
check_1<-check
#发现在sql语句中的max 和min 是把日期变量当作数值进行处理的
date_check<-sqldf("select item_id,min(calendar_date)as start_date, max(calendar_date)as end_date  from check_1 group by item_id")

date_check$start_date<-as.Date(date_check$start_date,origin="1970-1-1")
date_check$end_date<-as.Date(date_check$end_date,origin="1970-1-1")
date_check



# 看来还是要用 melt 与dcast
library(reshape2)
# 生成一个calendar_date_2
check_1$calendar_date_2<-check_1$calendar_date
temp<-melt(check_1,id=c("calendar_date","item_id","category_desc"))
#以上结果日期为数值
check_date<-dcast(temp,item_id+category_desc~variable)
#以上结果出现错误


#利用split+apply 函数进行测算起止时间

sp<-split(check_1,check_1[c("item_id","category_desc")],drop=TRUE)
start_date<-lapply(sp,FUN=function(x)min(x$calendar_date))
end_date<-lapply(sp,FUN=function(x)max(x$calendar_date))
result<-cbind(start_date,end_date)
#15857 是6076.drink 日期是2013-06-01
as.Date(15857,origin="1970-1-1")
#发现其实只要修改as.date就可以了 sql还是非常强大的




#step3 算所有产品的起止时间，销售日期记录天数（卖了多少天）
check_all_item_date<-sqldf("select item_id,category_desc,min(calendar_date)as start_date,max(calendar_date)as end_date from origin group by item_id")
date_check<-check_all_item_date
date_check$date_count<-(date_check$end_date-date_check$start_date+1)

date_check$start_date<-as.Date(date_check$start_date,origin="1970-1-1")
date_check$end_date<-as.Date(date_check$end_date,origin="1970-1-1")

date_check

write.csv(date_check,"date_check_for_180items.csv")

#3.1新旧产品上线时间表
#分类对上线天数进行重新赋值 连续变量改为类别变量,销售天数全满为 best_seller "1-2 years" "0.5-1year" "1-6 months" "below 1 month" "less than 5 days"

day_check<-date_check

day_check$category_day[day_check$date_count>=700]<-"all_the_time"
day_check$category_day[day_check$date_count<700 & day_check$date_count>=365]<-"1-2_years"
day_check$category_day[day_check$date_count>=180 & day_check$date_count<365]<-"0.5-1_years"
day_check$category_day[day_check$date_count<180 & day_check$date_count>=30]<-"1-6_months"
day_check$category_day[day_check$date_count<30 & day_check$date_count>=5]<-"less_than_30_days"
day_check$category_day[day_check$date_count<5]<-"less_than_5_days"

day_check<-sqldf("select category_day,count(*)as number from day_check group by category_day")

#3.2 需要剔除all_the_time
date_check_delete<-sqldf("select * from date_check where date_count<700 and date_count>5")



write.csv(date_check_delete,"126款产品的销售日期统计，与销售天数.csv")


#step 4 制作每款产品的日销量
#4.1 剔除销售时间小于30天 的14款产品
drop_id<-sqldf("select item_id,category_desc from date_check where date_count<30")
daily_sell_origin<-sqldf("select * from origin where item_id not in (select item_id from drop_id)")

#4.2 所有产品的销量按照天与产品id 来进行合并
daily_sell<-sqldf("select category_desc, item_id, calendar_date,sum(unit_sold)as daily_sold from origin group by item_id,calendar_date ")
#为啥有些产品销量为0？
count_zeros<-sqldf("select* from origin where unit_sold <=0 ")
#应该把这些也剔除出去 不应该也许？


#4.3 现在是比较麻烦的，需要将数据进行行和列的转变，纵坐标为calendar_date 横坐标为item_id 将categorydesc 放在前面然后daily_sell放在后面
#一般来说这样子操作是有一些粗糙的，因为有部分天数会出现bulk purchasing的情况
library(tidyr)	
daily_sell<-unite(daily_sell,"new_id",category_desc,item_id,sep="ID")
temp<-melt(daily_sell,id=c("calendar_date","new_id"))
check<-dcast(temp,calendar_date~new_id)

write.csv(check,"daily_sell.csv")

# 5 弄一个适合搞关联规则的原始数据：TID 是date store code, calendar_date  事务集是 item_id item_id 是category_desc+ID
library(sqldf)
# 5.1 按照天与store——code合并
summary(origin)
origin_edit<-sqldf("select sum(unit_sold)as daily_sold,store_code,item_id,category_desc,calendar_date from origin group by calendar_date,item_id")
summary(origin_edit)
# 5.2 剔除销量为负值的数据
temp<-subset(origin_edit,daily_sold>=0)
summary(temp)
#5.3 将category—_desc stic to item_id 用unite 函数（）unite(data, col, ..., sep = "_", remove = TRUE)
library(tidyr)	
temp_new<-unite(temp,"ID",category_desc,item_id,sep="_ID",remove=TRUE)
View(temp_new)
association_rule<-sqldf("select calendar_date,store_code,ID,daily_sold from temp_new")
write.csv(association_rule,"association_rule.csv")

