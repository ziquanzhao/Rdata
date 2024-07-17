library(ggplot2)
library(ggpubr)

mydata <- read.table('RT-SFSM-HWG.txt',header = T,sep = '\t')

pdf('RT-SFSM.pdf',width = 7,height = 5)

ggplot(data = mydata,aes(x = SFSM,y = Relative))+
  geom_point(aes(color = Germplasm.type),size = 3)+
  geom_smooth(method = 'lm',formula = y~x,se = T,color = 'black')+
  stat_cor(data = mydata,method = 'pearson')+  #Spearman用于非正态性数据，pearson用于正态性数据
  xlab(label = 'SFSM/g')+
  ylab(label = 'XsAP2 relative expression level')+
  theme_bw()+
  theme(text = element_text(family = 'serif',size = 14),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())+
  theme(axis.text.x = element_text(family = 'serif',hjust = 1,vjust = 1,size = 16,face = 'bold'),axis.text.y = element_text(family = 'serif',size = 16,face = 'bold'))+
  theme(axis.title.y = element_text(family = 'serif',size = 16,face = 'bold'),axis.title.x = element_text(family = 'serif',size = 16))

dev.off()

yield <- data.frame(c(6830,6311,6011,5061,3571,2729,2451))
names(yield) <- "yield"

ks.test(x = yield$yield)
shapiro.test(x = yield$yield)
ks.test(x = yield$yield,y = "pnorm",mean = mean(yield$yield),sd = sd(yield$yield))