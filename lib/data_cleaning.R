#������ϴ data cleaning
# data improt: KFC_for_12.dta
#data export: 1. daily_sell.csv 2.  ������Ʒ����������ͳ��.csv 3. association_rule.csv


rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")

library(sqldf)
library(foreign)

origin<-read.dta("KFC_for_12.dta")
origin$calendar_date<-as.Date(origin$calendar_date,"%m/%d/%y")
# summary(origin)

#step1 Ѱ������
#tips�ܹ�������
sqldf("select count(*)from origin")#304879
sqldf("select count(distinct(calendar_date))from origin")#728 day
start_date<-sqldf("select distinct(calendar_date) from origin")


#tips���ۼ��������Ⱥ�˳����û��Ӱ��

aggregate1<-sqldf("select day_id,store_code,item_id,sell_id,count(*) from origin group by store_code,day_id,item_id,sell_id having count(*)>1")

# ����alcprice

aggregate2<-sqldf("select unit_sold,count(*) from origin group by store_code,day_id,item_id,sell_id,alc_price,combo_flag,sub_class,unit_sold,sell_price having count(*)>2")

#��ÿ��ÿ�ҵ������˶����ֲ�Ʒ�Լ���id
item_daily_id<-sqldf("select all calendar_date,store_code, item_id from origin group by calendar_date,store_code,item_id")

check<-c("unit_sold","sell_price")
describe(origin[check])

#step 2 ���÷������鿴ÿ����Ʒid�������ͼ۸��������ͳ������

myvars<-c("unit_sold","sell_price","alc_price")
describeBy(origin[myvars],list(am=origin$item_id))

too_small<-sqldf("select distinct item_id,count(*)as n,category_desc  from origin group by item_id having n<50")
write.csv(too_small,"���ۼ�¼С��50�Ĳ�Ʒid.csv")

#�޳����ۼ�¼С��50�Ĳ�Ʒ
delete_item<-too_small$item_id

check<-sqldf("select calendar_date,unit_sold,item_id,category_desc from origin where item_id in (select item_id from too_small) order by item_id,calendar_date")

#�ǳ���Ҫ����Ҫ����һ������������ һ������ֵ ���� ��ʼ���� ���������,����Ҫ����
check_1<-check
#������sql����е�max ��min �ǰ����ڱ���������ֵ���д�����
date_check<-sqldf("select item_id,min(calendar_date)as start_date, max(calendar_date)as end_date  from check_1 group by item_id")

date_check$start_date<-as.Date(date_check$start_date,origin="1970-1-1")
date_check$end_date<-as.Date(date_check$end_date,origin="1970-1-1")
date_check



# ��������Ҫ�� melt ��dcast
library(reshape2)
# ����һ��calendar_date_2
check_1$calendar_date_2<-check_1$calendar_date
temp<-melt(check_1,id=c("calendar_date","item_id","category_desc"))
#���Ͻ������Ϊ��ֵ
check_date<-dcast(temp,item_id+category_desc~variable)
#���Ͻ�����ִ���


#����split+apply �������в�����ֹʱ��

sp<-split(check_1,check_1[c("item_id","category_desc")],drop=TRUE)
start_date<-lapply(sp,FUN=function(x)min(x$calendar_date))
end_date<-lapply(sp,FUN=function(x)max(x$calendar_date))
result<-cbind(start_date,end_date)
#15857 ��6076.drink ������2013-06-01
as.Date(15857,origin="1970-1-1")
#������ʵֻҪ�޸�as.date�Ϳ����� sql���Ƿǳ�ǿ���




#step3 �����в�Ʒ����ֹʱ�䣬�������ڼ�¼���������˶����죩
check_all_item_date<-sqldf("select item_id,category_desc,min(calendar_date)as start_date,max(calendar_date)as end_date from origin group by item_id")
date_check<-check_all_item_date
date_check$date_count<-(date_check$end_date-date_check$start_date+1)

date_check$start_date<-as.Date(date_check$start_date,origin="1970-1-1")
date_check$end_date<-as.Date(date_check$end_date,origin="1970-1-1")

date_check

write.csv(date_check,"date_check_for_180items.csv")

#3.1�¾ɲ�Ʒ����ʱ���
#��������������������¸�ֵ ����������Ϊ������,��������ȫ��Ϊ best_seller "1-2 years" "0.5-1year" "1-6 months" "below 1 month" "less than 5 days"

day_check<-date_check

day_check$category_day[day_check$date_count>=700]<-"all_the_time"
day_check$category_day[day_check$date_count<700 & day_check$date_count>=365]<-"1-2_years"
day_check$category_day[day_check$date_count>=180 & day_check$date_count<365]<-"0.5-1_years"
day_check$category_day[day_check$date_count<180 & day_check$date_count>=30]<-"1-6_months"
day_check$category_day[day_check$date_count<30 & day_check$date_count>=5]<-"less_than_30_days"
day_check$category_day[day_check$date_count<5]<-"less_than_5_days"

day_check<-sqldf("select category_day,count(*)as number from day_check group by category_day")

#3.2 ��Ҫ�޳�all_the_time
date_check_delete<-sqldf("select * from date_check where date_count<700 and date_count>5")



write.csv(date_check_delete,"126���Ʒ����������ͳ�ƣ�����������.csv")


#step 4 ����ÿ���Ʒ��������
#4.1 �޳�����ʱ��С��30�� ��14���Ʒ
drop_id<-sqldf("select item_id,category_desc from date_check where date_count<30")
daily_sell_origin<-sqldf("select * from origin where item_id not in (select item_id from drop_id)")

#4.2 ���в�Ʒ���������������Ʒid �����кϲ�
daily_sell<-sqldf("select category_desc, item_id, calendar_date,sum(unit_sold)as daily_sold from origin group by item_id,calendar_date ")
#Ϊɶ��Щ��Ʒ����Ϊ0��
count_zeros<-sqldf("select* from origin where unit_sold <=0 ")
#Ӧ�ð���ЩҲ�޳���ȥ ��Ӧ��Ҳ����


#4.3 �����ǱȽ��鷳�ģ���Ҫ�����ݽ����к��е�ת�䣬������Ϊcalendar_date ������Ϊitem_id ��categorydesc ����ǰ��Ȼ��daily_sell���ں���
#һ����˵�����Ӳ�������һЩ�ֲڵģ���Ϊ�в������������bulk purchasing�����
library(tidyr)	
daily_sell<-unite(daily_sell,"new_id",category_desc,item_id,sep="ID")
temp<-melt(daily_sell,id=c("calendar_date","new_id"))
check<-dcast(temp,calendar_date~new_id)

write.csv(check,"daily_sell.csv")

# 5 Ūһ���ʺϸ���������ԭʼ���ݣ�TID ��date store code, calendar_date  ������ item_id item_id ��category_desc+ID
library(sqldf)
# 5.1 ��������store����code�ϲ�
summary(origin)
origin_edit<-sqldf("select sum(unit_sold)as daily_sold,store_code,item_id,category_desc,calendar_date from origin group by calendar_date,item_id")
summary(origin_edit)
# 5.2 �޳�����Ϊ��ֵ������
temp<-subset(origin_edit,daily_sold>=0)
summary(temp)
#5.3 ��category��_desc stic to item_id ��unite ��������unite(data, col, ..., sep = "_", remove = TRUE)
library(tidyr)	
temp_new<-unite(temp,"ID",category_desc,item_id,sep="_ID",remove=TRUE)
View(temp_new)
association_rule<-sqldf("select calendar_date,store_code,ID,daily_sold from temp_new")
write.csv(association_rule,"association_rule.csv")
