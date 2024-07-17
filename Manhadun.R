rm(list = ls())
library(qqman)
library(ggpubr)
setwd("E:/R/曼哈顿")


my_gwasresults <- read.csv('GAPIT.Association.GWAS_Results.GLM.SFSN.csv')
my_gwasresults <- data.frame(my_gwasresults[,1],as.numeric(gsub(pattern = 'LACHESIS_GROUP',replacement = '',my_gwasresults[,2])),my_gwasresults[,3],my_gwasresults[,4])
colnames(my_gwasresults) <- c('SNP','CHR','BP','P')

pdf('GLM-SFSN2022.pdf',width = 10,height = 5)
manhattan(my_gwasresults,main = 'SFSN(GLM model)',cex = 1,suggestiveline = -log10(0.05/length(rownames(my_gwasresults[,1]))),
          col = c('#de3026','#fedd92','#4d73b1'),family='serif')
dev.off()

pdf('GLM-SFSN2022-qq.pdf',width = 5,height = 5)
qq(my_gwasresults$P,cex = 1,col = '#4d73b1',family='serif',pch = 20)
dev.off()

