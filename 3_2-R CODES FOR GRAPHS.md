# 1.ggplot

```r
library(ggplot2)

##barchart with facets
ggplot(dimbar9, aes(reorder(angle,value), value)) + 
geom_bar(stat = "identity", width= 0.7, fill="#b5c4b1") + 
facet_wrap(~ tag, ncol = 5) + coord_flip() +theme_gray()+ 
ggtitle("Topic angles distribution by segments") + 
xlab("topic angles") +ylab("value") + theme_set(theme_bw()) +theme(panel.grid.major=element_line(colour=NA))+
theme(plot.title = element_text(hjust = 0.5))

##bubbule chart
bubble1<-read.csv("~/Desktop/bubble1.csv",header=T,as.is=T)##要做气泡的数据 
ggplot(bubble1, aes(p_rate, n_rate, color=factor(cc))) + 
geom_point(aes(shape=factor(tag),size=total),stat = "identity" ) + 
facet_wrap( ~ tag2, ncol = 1) +
theme_gray()+ 
ggtitle("Sentiment distribution by topic angles") + 
xlab("positive rate") +ylab("negative rate") + 
theme_set(theme_bw()) +
theme(panel.grid.major=element_line(colour=NA))+
theme(plot.title = element_text(hjust = 0.5))+ 
geom_text(aes(label=sentiment),size=3,hjust = 0, nudge_x = 0.02,check_overlap = TRUE)+
##geom_label(aes(fill = factor(cc)), colour = "white", fontface = "bold")+
xlim(0,0.8)+ylim(0,0.5) +
theme(legend.position = "none")
```
# 2.bar racer animation 

```r
##animation 
install.packages('gifski')
install.packages('devtools')
devtools::install_github("jl5000/barRacer")
library(barRacer)
dimbar3<-read.csv("~/Desktop/dimbar.csv",header=T,as.is=T)##要做条形图的数据 
bar_chart_race(df = dimbar3, # 数据集 
cat_col = author_type, # 分类变量 
val_col = value, # 数值变量 
time_col = tag2, #时间变量 (没有时间 用integar 代替) 
max_bars = 15, # 默认是10，显示10个bar 
duration = 30, # 默认是20s时间 
fps = 20, # 默认是10 
width = 900, # 
height = 1200, # 
title = "topics among different groups ") # 绘制
##gganimate::anim_save(filename = "topics2.gif") 
gganimate::anim_save(filename = "topics.mp4")
```
# 3.maps (world, china, region)

```r
library(ggplot2)
library(maptools)
library(geosphere)
library(plyr)

#Sales data import and pro-processing
MBsales<-read.csv("./data/map_R_circle.csv",sep=';',header=T)
MBsales2016<-MBsales[MBsales$Year==2016,]
Csales2016<-MBsales[MBsales$Model=="C CLASS SEDAN"& MBsales$Year==2016 & MBsales$CBU.PbP=="PBP",]
Csales2016<-data.frame(Csales2016)
Sales<-ddply(Csales2016,.(City),summarize,Sales=sum(Total.Year))
Sales<-Sales[order(-Sales$Sales),]
Sales$id<-as.character(c(1:nrow(Sales)))
Sales$City<-gsub("'","",Sales$City)
Sales$City<-gsub("Nanning City","NANNING",Sales$City)s
Sales$City<-gsub("Fu2zhou","FUZHOU",Sales$City)
Sales$City<-gsub("Liuzhou City","Liuzhou",Sales$City)
Sales$City<-gsub("Foshan Nanhai","Foshan",Sales$City)
Sales$City<-gsub("Foshan Shunde","Foshan",Sales$City)
Sales$City<-toupper(Sales$City)
#China CityGeocode data import
Citygeocode<-read.csv("./data/city_geocode_lookup.csv",sep=';',header=T)
Citygeocode$City<-toupper(Citygeocode$City)
#selected<-toupper(c("Beijing", "Shanghai", "Guangzhou",?"Foshan", "Xi¡¯an", "Chengdu", "Suzhou", "Dalian"))
#selected<-Citygeocode[Citygeocode$City %in% selected,]

Sales<-merge(Sales,Citygeocode,by.x='City',by.y='City',all.x=TRUE)

BBACcode=c(116.3,39.9)
Sales<-Sales[complete.cases(Sales),]
routes = gcIntermediate(BBACcode,Sales[,c('Lon', 'Lat')], 300, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)

fortify.SpatialLinesDataFrame = function(model, data, ...) {ldply(model@lines, fortify)}
fortifiedroutes = fortify.SpatialLinesDataFrame(routes) 

greatcircles = merge(fortifiedroutes, Sales, all.x=T, by="id")

ChinaProvince<-readShapePoly("./data/province.shp")
Chinamap <- fortify(ChinaProvince)

theme_map <- list(theme(panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
panel.border = element_blank(),
axis.line = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank(),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
panel.background = element_blank(),
plot.background = element_blank()))

p1<-ggplot(Sales)+
geom_path(aes(x = long, y = lat, group = id),size=0.15,data=Chinamap)+
geom_line(aes(long,lat,group=id),data=greatcircles,color='grey',alpha=0.25,size=0.15)+
geom_point(aes(Lon,Lat,group=id,alpha=Sales,size=Sales),color="red")+scale_size(range = c(0, 4))+
dgeom_text(aes(Lon,Lat,label=City),data=Sales[1:5,],hjust =-0.4,check_overlap = TRUE,size=2.5)+
scale_alpha_continuous(range = c(0.25, 0.8))+coord_map()+ylim(14,55)+theme_map
ggsave("./outputs/China map great cicle.png", p1, height=4.8, width=9.5)
```
# 4.sankey diagram

# 5.heatmap

# 6.word cloud

```r
library(wordcloud2)
library(RColorBrewer)
df6<-read.csv("~/Desktop/cloud.csv",header=T,as.is=T)
wordcloud2(df6,shape= "circle", rotateRatio =0, color="lightblue") 
wordcloud2(df6,shape="circle", size = 1, minSize = 0, gridSize =  0, 

fontFamily = NULL, fontWeight = 'normal',

color = 'random-dark', backgroundColor = "white", maxRotation = pi/4, rotateRatio = 0.4,  shape = 'circle', ellipticity = 0.65, widgetsize = NULL) 
```
# some useful vingettes 

