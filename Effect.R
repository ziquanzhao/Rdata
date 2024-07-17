library(ggplot2)
library(ggpubr)

mydata <- read.csv('Effict.csv')
mydata$Model <- as.factor(mydata$Model)


pdf('SFSM-SNP_effict.pdf',width = 9,height = 5)
ggplot(data = mydata[13:523,],aes(x = SNP,y = Effect,colour = Model))+
  geom_point(size = 2)+
  scale_color_manual(values = c('#de3026','#fedd92','#4d73b1','#03E0B6'))+
  xlab(label = 'SNP position')+
  ylab(label = 'Allele Estimated Effect Values')+
  theme_bw()+
  theme(text = element_text(family = 'serif',),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())+
  theme(axis.text.x = element_blank(),axis.text.y = element_text(family = 'serif',size = 15,face = 'bold'))+
  theme(axis.title.y = element_text(family = 'serif',size = 18),axis.title.x = element_text(family = 'serif',size = 18))
dev.off()
