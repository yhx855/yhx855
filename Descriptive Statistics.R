rm(list=ls())

#读取包
library(ggplot2)
library(tmcn)
library(magrittr)
library(stringr)
library(ggpubr)
library(gridExtra)

#设置系统语言为中文
Sys.setlocale(category = "LC_ALL", locale = "Chinese")


#绘制环状图ggpubr包里的函数,不建议用ggplot或者pie画饼图，画出来不好看
df <- data.frame(
  group = c("英语", "日语", "其他语言"),
  value = c(140, 15, 13))
#计算比例
df$percent_value = round(df$value/sum(df$value) * 100)
#设置标签
labs <- paste0(df$group, " (", df$percent_value, "%)")  
#绘图
ggdonutchart(df, "value",
             label = labs,
             fill = "group",
             color = "white",
             palette = c("#FFE1E0", "#CAEDEB", "#FFB236"))  

#散点图准备数据
point <- data.frame(douban[,2],douban[,11],douban[,15])
#更改变量名
names(point)<-c("grade","director","screenwriter")
#绘制箱线图
fig1 <-ggplot(data = point, mapping = aes(x = screenwriter, y = grade)) + 
  geom_point() + geom_rug(position = 'jitter', size = 0.1)+ geom_smooth()
fig1 <- fig1 + xlab("编剧历史评分")#设置x轴名称
fig1 <- fig1 + ylab("评分") #设置y轴名称
png("历史评分.png", width=3000, height=3000, res=500, bg = "transparent")
fig1
dev.off()

#准备评分数据
grade <- as.numeric(as.matrix(douban[,2]))
#绘制评分直方图，有时候用hist比ggplot2画的好看
png("评分.png", width=5000, height=3000, res=500, bg = "transparent")
hist(grade, col = "#F5BC62",main="",xlab="分数",ylab = "频数")
dev.off()
dat <- data.frame(table(round(log(na.omit(Price)),1)))
#任务中的数据不是四舍五入，但具体怎么做的我也不清楚
#绘图
fig1 <- ggplot(dat, aes(x = Var1, y = Freq))#基函数
fig1 <- fig1 + geom_col(width = 1, fill = "lightgoldenrod")#绘制柱状图
fig1 <- fig1 + xlab("产品价格(对数变化)")#设置x轴名称
fig1 <- fig1 + ylab("频数") #设置y轴名称
fig1 <- fig1 + theme(panel.grid.major =element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))#设置背景
fig1 <- fig1 + scale_x_discrete(breaks=seq(0, 12, 2))#不清楚怎么把坐标轴改成任务里那样
fig1


#箱线图+地毯图+抖点图
dat <- read.csv("z.csv", header = T, encoding = 'UTF-8')
#更改名称
names(dat)<- c("x1","x2","x3","x4")
#绘制箱线图
fig3 <- ggplot(dat, aes(x=x1, y=x2)) 
fig3 <- fig3 +  geom_boxplot(aes(fill=x1), color="black",width = 0.5, notch=T) + #绘制箱线图
  geom_point(position=position_jitter(0.2), color="black", alpha=.5) + #绘制抖点图
  geom_rug(color="black") + #绘制地毯图
  guides(fill=guide_legend(title=NULL)) #去除图例标题
fig3 <- fig3 + scale_fill_manual(values=c("#F1E3A7","#BAD1B0"))#修改颜色
fig3 <- fig3 + xlab("")     #修改x轴名称
fig3 <- fig3 + ylab("上映时间") #修改y轴名称 
fig3 <- fig3 + theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(),axis.line = element_line(colour = "black"))
fig3 <- fig3 + theme(legend.position="none")
png("上映.png", width=2000, height=2000, res=500, bg = "transparent")
fig3
dev.off()


#词云图
#提取水果
catalog_sg <- data.table(catalog)
catalog_sg <- catalog_sg[second=="水果"] # 等价于用逻辑值筛选
#提取苹果
catalog_pg <- data.frame(catalog_sg[third=="苹果"])[,1] # 等价于用逻辑值筛选
#将数据格式转化为数据框
catalog_sg <- data.frame(catalog_sg)[,1]
#分词
catalog_pg <- table(segment(catalog_pg, engine))
catalog_sg <- table(segment(catalog_sg, engine))
#降序排序，并提取出现次数最多的前100个词语
catalog_pg <- sort(catalog_pg, decreasing = T)[1:100] 
catalog_sg <- sort(catalog_sg, decreasing = T)[1:100] 

#绘制词云
wordcloud2(catalog_sg, color = 'random-dark',fontFamily = "微软雅黑",
           minRotation = -pi/6, maxRotation = -pi/6,
           rotateRatio = 1)


#折线图
x202101 <- read.csv("202101.csv", header = T, encoding = 'UTF-8')
datefactorlevel <- read.csv("date.csv", header = T, encoding = 'UTF-8')
dat2 <-  as.data.frame(table(x202101[,7]))
names(dat2) <- c("date","freq")
dat2 <- dat2[order(as.Date(dat2$dat, format='%m/%d/%Y')),] 
date <- as.character(datefactorlevel)

#a<-factor(dat2[,1],levels = date[1:31])

a<-factor(dat2[,1],levels = c("1/1/2021",
                              "1/2/2021",
                              "1/3/2021",
                              "1/4/2021",
                              "1/5/2021",
                              "1/6/2021",
                              "1/7/2021",
                              "1/8/2021",
                              "1/9/2021",
                              "1/10/2021",
                              "1/11/2021",
                              "1/12/2021",
                              "1/13/2021",
                              "1/14/2021",
                              "1/15/2021",
                              "1/16/2021",
                              "1/17/2021",
                              "1/18/2021",
                              "1/19/2021",
                              "1/20/2021",
                              "1/21/2021",
                              "1/25/2021",
                              "1/26/2021",
                              "1/27/2021",
                              "1/28/2021",
                              "1/29/2021",
                              "1/30/2021",
                              "1/31/2021"))
b<-dat2[,2]
dat3 <-data.frame(a,b)

p3 <- ggplot(dat3, aes(x = a, y = b, group = 1))
p3 <- p3 + geom_line() + theme_set(theme_gray())+
  scale_linetype_manual(values=c(1, 2))
p3 <- p3 + xlab("Date") +
  theme(axis.title.x = element_text(size = 16,
                                    color = "black", face = "bold",
                                    vjust = 0.5, hjust = 0.5))
p3 <- p3 + ylab("Freq") +
  theme(axis.title.y = element_text(size = 16,
                                    color = "black", face = "bold",
                                    vjust = 0.5, hjust = 0.5))
#p3 <- p3 + scale_x_discrete(labels=c("2002", "", "2003","","2004","","2005", "","2006", "","2007","","2008", "","2009","", "2010","","2011", "","2012", "","2013","","2014", "","2015","", "2016","","2017","", "2018","", "2019",""))
p3 <- p3 + theme(axis.text.x = element_text(size = 10,
                                            color = "black", face = "bold",
                                            vjust = 1, hjust = 1, angle = 45))
p3 <- p3 + theme(axis.text.y = element_text(size = 15,
                                            color = "black", face = "bold",
                                            vjust = 0.5, hjust = 0.5))
p3 <- p3 + theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(),axis.line = element_line(colour = "black"))
p3 <- p3 + theme(legend.title=element_blank()) +
  theme(legend.position=c(0.90,0.075)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.width=unit(.5,"inches"))+
  theme(legend.background = element_blank()) +
  theme(legend.text = element_text(colour="black", size = 16, face = "bold"))
p3
png("Fig 3.png", width=5000, height=3000, res=500, bg = "transparent")
p3
dev.off()
