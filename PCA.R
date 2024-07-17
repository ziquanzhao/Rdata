mydata <- read.table('GAPIT.Genotype.PCA.txt',header = T)


pdf(file = 'PCA123.pdf',width = 7,height = 7)

plot(mydata$PC1,mydata$PC2,type = 'p',xlab = 'PC1(4.99%)',ylab = 'PC2(1.73%)',pch=20,col='#E95D53',cex=1,cex.lab=1,font.lab=2,family='serif')

plot(mydata$PC1,mydata$PC3,type = 'p',xlab = 'PC1(4.99%)',ylab = 'PC3(1.70%)',pch=20,col='#E95D53',cex=1,cex.lab=1,font.lab=2,family='serif')

plot(mydata$PC2,mydata$PC3,type = 'p',xlab = 'PC2(1.73%)',ylab = 'PC3(1.70%)',pch=20,col='#E95D53',cex=1,cex.lab=1,font.lab=2,family='serif')

dev.off()

