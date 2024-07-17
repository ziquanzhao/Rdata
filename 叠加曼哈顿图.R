library(CMplot)
rm(list = ls())


my_gwasresults2022 <- read.csv('GAPIT.Association.GWAS_Results.MLM.SFSN2022.csv')
my_gwasresults2022 <- data.frame(my_gwasresults2022[,1],as.numeric(gsub(pattern = 'LACHESIS_GROUP',replacement = '',my_gwasresults2022[,2])),my_gwasresults2022[,3],my_gwasresults2022[,4])

my_gwasresults2020 <- read.csv('GAPIT.Association.GWAS_Results.MLM.SFSN2020.csv')

my_gwasresults <- data.frame(my_gwasresults2022,my_gwasresults2020[,4])
colnames(my_gwasresults) <- c('SNP','Chr','Position','SFSN2022','SFSN2020')

CMplot(my_gwasresults,file.name = 'SFSN-MLM',plot.type = 'c',r = 0.4,cir.,multracks = F,threshold = 0.05/nrow(my_gwasresults),amplify = T,col = matrix(c('#4d73b1','#fedd92'),2,1,byrow = T),points.alpha = 255,multraits = T,file = 'jpg',dpi = 600)