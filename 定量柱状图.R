install.packages('ggbreak')

rm(list=ls())
library(agricolae)
library(ggplot2)
library(ggbreak)



windowsFonts(serif = windowsFont("Times New Roman"))

mydata <- read.table('S1-S28-RTQPCR.txt',header = T)
mydata$ID <- as.factor(mydata$ID)
colnames(mydata) <- c('Group','Relative')
aov.mean <- aggregate(mydata$Relative,by=list(mydata$Group),FUN=mean)
aov.sd <- aggregate(mydata$Relative,by=list(mydata$Group),FUN=sd)

aov.mean$Group.1 <- factor(aov.mean$Group.1,levels = c('TL24','TL190','TL226','TL263','TL337','TL346','TL353','TL442','TL450','TL87','TL97','TL110','TL182','TL264','TL344','TL350'))
mean <- aov.mean[order(aov.mean[,1]),]
aov.sd$Group.1 <- factor(aov.sd$Group.1,levels = c(c('TL24','TL190','TL226','TL263','TL337','TL346','TL353','TL442','TL450','TL87','TL97','TL110','TL182','TL264','TL344','TL350')))
sd <- aov.sd[order(aov.sd[,1]),]

finaldata <- data.frame(mean,sd$x)

pdf('S1-S28.pdf',width = 8,height = 5)

ggplot(data = finaldata,aes(x = Group.1,y = x))+
  geom_bar(stat="identity",position="dodge",width = 0.6,fill = '#E95D53')+
  geom_errorbar(aes(ymin = x - sd.x, ymax = x + sd.x),position = position_dodge(0.9),width=0.2,linewidth = 0.35)+
  labs(x= '',y = 'Relative  expression  level')+
  #scale_y_break(c(2.5,2000),space = 0.5,scales = 4)+
  theme_bw()+
  theme(legend.position = 'none',legend.title = element_blank(),text = element_text(family = 'serif'),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())+
  theme(axis.text.x = element_text(family = 'serif',angle = 30,hjust = 1,vjust = 1,size = 16,face = 'bold'),axis.text.y = element_text(family = 'serif',size = 16,face = 'bold'))+
  theme(axis.title.y = element_text(family = 'serif',face = 'bold',size = 16,vjust = 2))

dev.off()

finaldata <- read.table('Xs_RtqPCR.txt',header = T)
finaldata$ID <- as.factor(finaldata$ID)
finaldata$Group <- as.factor(finaldata$Group)

pdf('TRqPCR.pdf',width = 8,height = 5)

ggplot(data = finaldata,aes(x = ID,y = Relative,fill = Group))+
  geom_bar(stat = 'identity',position = 'dodge',width = 0.8)+
  geom_errorbar(aes(ymin=Relative-SD,ymax=Relative+SD),width=0.3,position = position_dodge(0.8),linewidth = 0.35)+
  labs(x= '',y = 'Relative  expression  level',fill = 'Germplasm type')+
  scale_fill_manual(values = c('#E95D53','#ECB37B','#7488B5'))+
  theme_bw()+
  theme(legend.title = element_text(),text = element_text(family = 'serif',face = 'bold'),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())+
  theme(axis.text.x = element_text(family = 'serif',angle = 30,hjust = 1,vjust = 1,size = 14,face = 'bold'),axis.text.y = element_text(family = 'serif',size = 16,face = 'bold'))+
  theme(axis.title.y = element_text(family = 'serif',face = 'bold',size = 16,vjust = 2))

dev.off()