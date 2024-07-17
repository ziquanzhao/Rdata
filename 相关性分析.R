library(xlsx)
library(Hmisc)
library(corrplot)
rm(list = ls())
trait <- read.table(file = 'TL222taxa.txt',header = T)
trait <- trait[,3:5]

trait <- filetest

res2 <- rcorr(as.matrix(trait),type = 'pearson')

mycolor <- c()
for (i in seq(1:200)) {
   mycolor <- append(mycolor,COL2('RdBu',200)[201-i])    
}

mycolor <- mycolor[20:180]

pdf(file = 'corr2022.pdf',height = 5,width = 5,family = 'serif')
corrplot(corr = res2$r, p.mat = res2$P,  #先运行这个
         method = 'circle',col = mycolor,
         type = 'upper',
         tl.cex = 1,tl.col = 'black',
         insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 1,pch.col = 'black')
corrplot(corr = res2$r,   #再运行这个，否则画不全
         type = 'lower',add = T,method = 'number',
         tl.pos = 'l',tl.cex = 1,tl.col = 'black',col = mycolor,
         cl.pos = 'n')
dev.off()

