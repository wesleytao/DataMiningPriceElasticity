setwd("C:/Users/Wesle/Documents/R")
library(foreign)
library(sqldf)

#Step1 ����dtaȡ�����ݴ���origin����

origin<-read.dta("KFC_for_12.dta")
summary(origin)


#����ѡȡ����day_id item_id �ۣ������ ԭ�ۣ��ܷ��飬ϸ���飬�������ƣ����ڴ���mydata

mydata<-sqldf("select day_id,store_code,item_id,sell_id,unit_sold,alc_price,sell_price,amount,combo_flag,category,sub_category,class,sub_class,category_desc,calendar_date
                        from origin",row.names=TRUE)


#�޸�ʱ��,�������ַ����޸�Ϊʱ��
timeformat<-"%m/%d/%y"
mydata$calendar_date<-as.Date(mydata$calendar_date,timeformat)



#Step2 ����һ����Ʒ���������ϸ��¼��Ʒ�����ƣ��������࣬����CSV�ļ���

###����û��other ����Ĳ�Ʒ��ͬʱother��Ʒ�����sub_category 42 һ��������other һ��������dessert���޳�other��Ʒ

table(mydata$category,mydata$sub_category)
mydata<-subset(mydata, category!=13)



category<-sqldf("select category,sub_category,category_desc,count(distinct(item_id))as item_number
		     from mydata 
		     group by sub_category
		     order by category")
category
write.csv(category,"category.csv")





#��Ʒ��ϸ��¼180�ֲ�Ʒ�� item_id ���б�

item<-sqldf("select category,sub_category,category_desc,item_id,alc_price
		 from mydata
		 group by item_id
		 order by category,sub_category")
item


#��Ҫ��item�����item_id�����ַ������ӵĸ�ʽ�ŵ�category����
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


#�޳���һ����ֵ
item_all<-subset(a,a!="")

item_all
write.csv(item_all,"item_all.csv")







#Step 3 �ҳ�ÿһ��category����������ߵ��Ǹ���ID####
#3.1���ÿ����Ʒ��������
sum_item_sold<-sqldf("select sum(unit_sold)as sum_sold, item_id,category,category_desc
				from mydata
				group by item_id")


#���ÿ����������������ߵ�id
Max_sum_in_category_id<-sqldf("select item_id,category,category_desc 
					from sum_item_sold
					where sum_sold in (select max(sum_sold) 
									from sum_item_sold 
									group by category)
")

write.csv(Max_sum_in_category_id,"max_sum_item_id.csv")


####################################

#3.2������id���������۸�ԭ�ۣ�ʱ��,ÿ��������������������ķ�ʽ#######

best_seller<-sqldf("select item_id,category,category_desc,unit_sold,alc_price,amount,calendar_date
				from mydata 
				where item_id in (select item_id
							from Max_sum_in_category_id)
				")



write.csv(Max_sum_all_in,"Max_sum_item_sold.csv")


##3.3 ��Ҫ�ϲ�ÿ�յ�����
best_seller_daily<-sqldf("select item_id,calendar_date,sum(unit_sold) as daily_sell from best_seller group by calendar_date,item_id")


fix(best_seller)
write.csv(best_seller,"best_seller.csv")
write.csv(best_seller_daily,"best_seller_daily.csv")


###3.4 ת����������Ϊdate��������Ϊitem_id,ֵΪ�����ı�
library(reshape2)
library(plyr)

temp<-melt(best_seller_daily,id=c("calendar_date","item_id"))
best_seller_daily<-dcast(temp,calendar_date~item_id)

write.csv(best_seller_daily,"daily_sell_of_6_item.csv")


###4 �������������ͼ12345
#4.1 �����������ַ�������ʽȡ�������������� ID
names(best_seller_daily)
for (i in 2:7){
names(best_seller_daily)[i]<-paste("ID",names(best_seller_daily)[i],sep="")

}

#4.2 ���Ի�һ��ͼ,������ת��Ϊ�ַ��ͱ�������������,������������Ϊ������ԭ��
summary(best_seller_daily)
timeformat<-"%Y-%m-%d"
best_seller_daily$calendar_date<-as.Date(best_seller_daily$calendar_date,timeformat)


#4.3 ����ggplot2����һ������������ͼ ��ѡID5373
library(ggplot2)

attach(best_seller_daily)
plot(clendar_date2,ID5373,type="b")





#5 ����ͬ������ʽ��ԭ���Ƿ�ͬ

Item_6040<-sqldf("select calendar_date,item_id,alc_price,sell_id from origin where item_id='6040'")
#����������ԭ��Ϊ0Ԫ�ģ�������������Լ��۸�䶯
attach(Item_6040)
table(sell_id,alc_price)
summary(Item_6040)
detach(Item_6040)

#��ÿһ��������ʽ��ͬ,�۸�ԭ���Ƿ�ͬ
Item_6040<-sqldf("select * from origin where item_id='6040' order by calendar_date")
newdata<-Item_6040[1:100,]
attach(newdata)
newdata2<-newdata[order(calendar_date,store_code,sell_id,alc_price),]

detach(newdata)
#���� ���꣬sell-id ����ԭ�ۻ������� �����ڣ�,������ �޷���subset��ԭ�������Item_id Ϊlist����
close_look<-sqldf("select calendar_date,sell_id,store_code,alc_price,sell_price,combo_flag,item_code,unit_sold from Item_6040 where store_code='SHA227'  order by calendar_date")


close_look<-close_look[order(calendar_date,sell_id),]

#�ٴ�����ѡ��ԭ�۴���10Ԫ��
close_look<-sqldf("select * from close_look where alc_price>20")
#���ȷ�����ײͼ۸�Ļ������ǿ����ڵ����ҵ��ܶ�۸���69��,��һ��������Ķ���,���������ײ͵Ķ�Ӧ��ϵ������������ײ͵Ļ�
#��ôӦ�ü۸񶼺ܹ������ײ͵Ļ�Ӧ���ǵ��۶����ײͼ۸� 
#��ôΪʲô�е���Ʒ���ķǳ��ǳ���ֻ������������ͬ���Լ����������ۿ�ȯ

check_combo<-sqldf("select * from mydata where calendar_date='10/13/13' and store_code='SHA227' and combo_flag='2' order by item_id")

#�����ҿ����� Ӧ����store +day �γɵ���Ϊ�������򣿣���
