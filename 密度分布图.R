setwd("E:/R/密度分布图")
library(nortest)

filetest <- read.table(file = '2020.txt', header = T)

lillie.test(filetest$SFSM2020)

filetest <- filetest[!is.na(filetest$SFSM2020),]

pdf('pheotype-SFSM2020.pdf',height = 6,width = 6)
hist(x = filetest[,2],prob = T,main = '',family = 'serif',xlab = 'SFSM2020',border = 'black',lwd = 2)
lines(density(filetest[,2]),lwd = 2,col = 'red')
legend(x=7,y = 0.05,legend = 'KS normality test   p-value = 0.5419 < 0.05',bty = 'n',text.font = 2)
dev.off()

