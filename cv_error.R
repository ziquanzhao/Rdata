#需要准备好cv_error值，放到当前目录下。文件的名字改成‘cv_error.txt’
#注意当前工作目录要设置到cv_error.R所在目录
#文本文件的数据格式如：
#K2-value cv_error2_value
#K3-value cv_error3_value
#K4-value cv_error4_value



cv_error_data <- read.table('cv_error.txt',header = F,stringsAsFactors = F)

pdf('cv_error-value.pdf')
plot(cv_error_data$V1,cv_error_data$V2,type = 'o',xlab = 'K-value',ylab = 'CV_error-value',
     pch = 20,font.lab=2,family = 'serif',lwd = 0.8,col='black',yaxt='n')
points(3,0.45044,pch=20,cex=1.3,col='red')   #加点
axis(side = 1,at = 3,cex.axis=1,font.axis=1,family = 'serif',col.axis = 'red')   #自定义坐标轴
axis(side = 2,at = c(0.45044),cex.axis=1,font.axis=1,family = 'serif',col.axis = 'red')
axis(side = 2,at = c(0.50,0.55,0.60),cex.axis=1,font.axis=1,family = 'serif',col.axis = 'black')#自定义坐标轴
legend('top',legend = 'K = 3  CV_error_min = 0.45044',bty = 'n',text.font = 1,cex = 0.8)  #标签
segments(0,0.45044,3,0.45044,lty = 2,lwd = 1.5,col = 'red')  #虚线
segments(3,0,3,0.45044,lty = 2,lwd = 1.5,col = 'red')   #虚线
dev.off()
