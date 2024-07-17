library(ggplot2)
library(ggpubr)
rm(list=ls())
mycolor <- c('#E95D53','#7488B5','#ECB37B','#F5F39F','#FBDCC0','#D3ACAE','#D1D198','#18489C')

mydata <- read.table('TLsample231_gene01.hmp.txt',header = T)#SNP数据集读入
collectGarbage()

SNP <- mydata[mydata$rs=='SLACHESIS_GROUP10_24013014',]#你感兴趣的SNP位点
SNP <- t(SNP[,-c(1,3:11)])

Traitdata <- read.table(file = 'TL222taxa.txt',header = T)#性状数据读入

finaldata <- data.frame(SNP[2:nrow(SNP),],Traitdata$HWG)
colnames(finaldata) <- c('SNP','HWG')
finaldata$SNP <- as.factor(finaldata$SNP)#将SNP突变转换为因子变量
finaldata <- finaldata[finaldata$SNP!='NN',]#有时候这个位点没有测出来，会表示成NN，分析时去掉这个


windowsFonts(serif = windowsFont("Times New Roman"))

pdf(file = 'Chr10_24013014 HWG SNP mutation type.pdf',width = 3,height = 4)

ggplot(data = finaldata,aes(x = SNP,y = HWG,color = SNP))+
  geom_boxplot(notch = F)+
  scale_color_manual(values = mycolor)+#自定义箱线图边框颜色
  scale_fill_manual(values = mycolor)+#自定义箱线图填充色
  labs(x='Chr10_24013014 locus mutation type',y='HWG/g')+#xy轴标题
  geom_jitter(aes(fill=SNP),width =0.2,shape = 20,size=1.3)+#加散点
  stat_boxplot(geom = "errorbar",width=0.15,aes(color=SNP))+#加误差线
  stat_compare_means(comparisons = list(c("--", "-C"),c("-C","CC"),c("--","CC")), method = "t.test")+#加显著性检验
  theme_bw()+
  theme(legend.position = 'none',legend.title = element_blank(),text = element_text(family = 'serif'),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())

dev.off()