# drawing the pics
install.packages("ggplot2")
library(ggplot2)
product<-c("Dessert","Drink","Snack","Side Item","Main","Non-Food")
numbers<-c(3+3+7+1,21+4+6+11,3+10+5+6,7+8+3+2,16+29+2+8+2+2+4,9+2+2)

df<-data.frame(type=product,nums=numbers)
p <- ggplot(data = df, mapping = aes(x = 'Content', y = nums, fill = type)) + geom_bar(stat = 'identity', position = 'stack', width = 1)  
#
label_value <- paste('(', round(df$nums/sum(df$nums) * 100, 1), '%)', sep = '')  
label_value  
label <- paste(df$type, label_value, sep = '')  
label  
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + scale_fill_discrete(labels = label)




# type <- c('A','B','C','D','E','F','G')  
# nums <- c(10,23,8,33,12,40,60)  
# df <- data.frame(type = type, nums = nums)  

# #绘制条形图  
# p <- ggplot(data = df, mapping = aes(x = 'Content', y = nums, fill = type)) + geom_bar(stat = 'identity', position = 'stack', width = 1)  
# p  
# 
# label_value <- paste('(', round(df$nums/sum(df$nums) * 100, 1), '%)', sep = '')  
# label_value  
# label <- paste(df$type, label_value, sep = '')  
# label  
# p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + theme(legend.position = "none") + geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]), x = sum(df$nums)/150, label = label))   