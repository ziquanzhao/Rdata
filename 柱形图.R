install.packages("agricolae")
library(agricolae)
library(ggplot2)
library(openxlsx)
library(gridExtra)
rm(list=ls())
mycolor <- c('#E95D53','#7488B5','#ECB37B','#F5F39F','#FBDCC0','#D3ACAE','#D1D198','#18489C')

mydata <- read.table('number.txt',header = T)
mydata$ID <- as.factor(mydata$ID)
colnames(mydata) <- c('SampleName','Number')
aov.mean <- aggregate(mydata$Number,by=list(mydata$SampleName),FUN=mean)
aov.sd <- aggregate(mydata$Number,by=list(mydata$SampleName),FUN=sd)

fit <- aov(mydata$Number~mydata$SampleName)
out <- HSD.test(fit,"mydata$SampleName",alpha = 0.001)

out_group <- out$groups

aov.mean$Group.1 <- factor(aov.mean$Group.1,levels = c('Col-0','EV','OE1','OE2','OE3','OE4','OE5'))
mean <- aov.mean[order(aov.mean[,1]),]
aov.sd$Group.1 <- factor(aov.sd$Group.1,levels = c('Col-0','EV','OE1','OE2','OE3','OE4','OE5'))
sd <- aov.sd[order(aov.sd[,1]),]

finaldata <- data.frame(mean$x,out_group,sd)


pdf(file = 'Seed number.pdf',width = 8,height = 5)

ggplot(data = finaldata,aes(x = Group.1,y = mean.x))+
  geom_bar(stat="identity",position="dodge",width = 0.6,fill = c('#E95D53','#ECB37B','#7488B5','#7488B5','#7488B5','#7488B5','#7488B5'))+
  geom_jitter(data = mydata,aes(x = SampleName,y = Number),width = 0.2,size=1,colour = '#662F00')+
  geom_errorbar(aes(ymin = mean.x - x, ymax = mean.x + x),position = position_dodge(0.9),width=0.2,linewidth=1)+
  geom_text(aes(y=mean.x+1.4*x,label=finaldata$groups),position=position_dodge(0.9),size=5,fontface="bold")+
  labs(y = "% of wild-type value")+
  scale_y_continuous(breaks = seq(0,100,20))+
  theme_bw()+
  theme(legend.position = 'none',legend.title = element_blank(),text = element_text(family = 'serif'),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_text(family = 'serif',size = 21,face = 'bold',vjust = 2))+
  theme(axis.text.x = element_text(family = 'serif',size = 18,face = 'bold',vjust = -1),axis.text.y = element_text(size = 18,face = 'bold'))

dev.off()