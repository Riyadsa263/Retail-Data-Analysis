#Required library 
library(ggplot2) 
library(wordcloud) 
library(calendR) 
library(dplyr)
library(RColorBrewer) 
library(tidyverse)
library(packcircles) 
#Reading files 
meesho2=read.csv("meesho2.csv") 
mobile=read.csv("mobile.csv") 
#Lollipop chart for meesho data 
s=count(meesho2,`Customer.State`) 
png(file="RLollipop_chart.jpg") 
ggplot(s,aes(x=`Customer.State`,y=n))+geom_segment(aes(x=`Customer.S
tate`,xend=`Customer.State`,y=0,yend=n))+
geom_point(color=rainbow(length(s$n)),size=4)+ 
theme(axis.text.x=element_text(angle=90)) 
dev.off() 
#Calendar plot for items bought on meesho in the month August 2022
c=count(meesho2,`Order.Date`) 
png(file="RCalender.jpg") 
calendR(year=2022,month=8,special.days=c$n,weeknames=c("Sun","Mon",
"Tue","Wed","Thu","Sat"),special.col="purple",gradient=TRUE,
legend.pos="right",legend.title="Delivery Frequency")
dev.off()
#Donut plot for delivery status
del=count(meesho2,`Reason.for.Credit.Entry`) 
df=data.frame(var1=c(del$`Reason.for.Credit.Entry`),Freq=c(del$n)) 
df$fraction=df$Freq/sum(df$Freq) df$ymax=cumsum(df$fraction) 
df$ymin=c(0,head(df$ymax,n=-1)) 
png(file="RDonut_plot.jpg") 
ggplot(df,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=var1))
+geom_rect()+ coord_polar("y") + xlim(c(2,4))
dev.off()
#Histogram plot for prices of dresses
h=count(meesho2,`Supplier.Discounted.Price`) 
price=h$`Supplier.Discounted.Price` 
png(file="RHistogram_plot.jpg") 
hist(price,col=brewer.pal(n=length(h),name="RdPu"),main="Price 
Histogram") 
dev.off() 
#Circular packaged plot for different sizes of dresses bought
png(file="RCircular_packaging.jpg") 
size=count(meesho2,Size) 
x = runif(min(size$n), max(size$n)) 
y = runif(min(size$n), max(size$n)) 
packing=circleProgressiveLayout(size$n, sizetype='area') 
size$packing 
dat.gg= circleLayoutVertices(packing, npoints=50) ggplot() + 
geom_polygon(data = dat.gg, aes(x,y,group=id, fill=factor(id)) )+ 
geom_text(data = size, aes(x,y,label = Size)) + theme(legend.title 
= element_blank(),legend.position = "right") + 
scale_fill_discrete(labels=c(size$Size))+ coord_equal() 
dev.off() 
#Word cloud for companies of mobile 
m2=mobile$Manufacturer 
png(file="RWordcloud.jpg") 
wordcloud(words=m2,min.freq=1,random.order=FALSE,random.color=TRUE,
colors=brewer.pal(8,"D ark2"))
dev.off() 
#Circular barplot for Companies of mobile 
m3=count(mobile,Manufacturer) 
png(file="RCircular_bar.jpg") 
ggplot(m3,aes(x=Manufacturer,y=n,fill=Manufacturer))+geom_bar(stat=
"identity",fill="darkred")+co ord_polar(start=0)+ theme_minimal() 
dev.off( ) 
#Bubble plot for sizes of clothes bought 
m=count(mobile,Year) 
a=m$`Year` 
b=m$`n` 
color <- brewer.pal(n = length(a), name = "PuOr") 
size=c(m$n) 
png(file="RBubble_chart.jpg") 
ggplot(data=m,aes(a,b,size=size))+geom_point(alpha=0.7,color=
rainbow(length(b)))+ theme(axis.text.x=element_text(angle=90)) + 
scale_size_continuous(range = c(8, 13)) 
dev.off() 
#Stacked bar plot for types of Mobile Phones
png(file="RStacked_plot.jpg") 
ggplot(mobile,aes(x = Year, fill = Form.factor)) + geom_bar(width = 
0.4) + theme_classic() + theme( plot.title = element_text(family = 
"Times New Roman", hjust ="bold"), axis.text = element_text(family = 
"Times New Roman",face ="bold"), axis.title = element_text(family = 
"Times New Roman", face = "bold"), legend.title = element_blank(), 
legend.text = element_text(family = "Times New Roman") ) + 
labs(title = "Types of Phones", x = NULL, y = "Mobile count")
dev.off()
