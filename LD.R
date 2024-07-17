if (F) {
  '
  使用说明：
  1.此脚本主要针对PopLDdecay软件绘图时的.bin数据进行再度可视化，变得更美观。
  2.需要的数据就是.bin文件，我们只会用这个文件的前两列数据，因此如果你不是.bin文件，也可以自己改造数据，
    只要满足第一列是序列长度（distance），第二列是LD值就可以。
  3.绘图需要的R脚本和.bin文件被期望放在工作目录下。第13行是.bin文件的文件名，注意更改。
  4.第19-24行是设置点和虚线，自定义坐标轴，这个要根据你设置的LD衰减距离进行设置，一般这个数据一定会更改。
  5.第26行图例中，我认为LD衰减距离是最大值一半时的LD值，你可以自己的定义。
  '
}

LD <- read.table('TL230_plot.bin',header = F,stringsAsFactors = F)
mydata <- data.frame(LD$V1,LD$V2)

pdf('LD.pdf',width = 6,height = 6)
plot(mydata$LD.V1/1000,mydata$LD.V2,type = 'l',xaxt='n',yaxt='n',col='blue',lwd=2,xlab = 'Distance(Kb)',ylab = expression(r^2),font.lab=2,family = 'serif')

segments(-15,0.219,20.75,0.219,lty = 2,lwd=2,col = 'red')  #虚线
segments(20.75,0,20.75,0.219,lty = 2,lwd=2,col = 'red')  #虚线
axis(side = 1,at = 20.75,family='serif',col.axis = 'red')  #红色坐标轴
axis(side = 2,at = 0.219,family='serif',col.axis = 'red')  #红色坐标轴
axis(side = 1,at = c(100,200,300,400,500),family='serif')  #x轴主要刻度
axis(side = 2,at = c(0.1,0.3,0.4),family='serif')  #y轴主要刻度

legend('top',legend = expression(paste(r^2,' = ',frac(1,2),' ',r[max]^2,'    ','Distance = 20.75Kb')),
       horiz = 1,bty = 'n',text.font = 1,cex = 0.8) #中上部的标签
dev.off()