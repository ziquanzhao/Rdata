#赵子权
#2939818719@qq.com 
#欢迎使用！！

if (F) {
  '
  使用须知：
  1.绘图需要pophelper包，没有的话从github下载安装。第21行取消注释即可。
  2.绘图需要admixture输出的Q文件，建议直接把Q文件和脚本都放在工作目录下。注意不要脑残到给K=1时的Q文件。
    如果没有，手动输入Q文件所在绝对路径。在第24行修改。
  3.绘图需要样本ID文件，从你做admixture时输入文件中找，应该有一个.nosex文件，把这个文件修改以下即可。格式如下：
    sampleID1
    sampleID2
    sampleID3
    你的样本ID文件强烈建议命名为admixture_sampleID.txt。如果不是，修改第26行。记得也放到工作目录下。
  4.图像右侧K值显示，右上部Group的显示，在第36、37行可以设置。
  5.输出图片大小（cm），dpi，位置，名字，图片类型可以在第39设置。
  '
}

#devtools::install_github('royfrancis/pophelper') #安装pophelper包
library(pophelper)

Q_file_list <- list.files('./',pattern = 'Q',full.names = T)   #Q文件所在目录，这是根据文件后缀识别文件，注意不要放其他以.Q结尾的文件。
all_Q_file_data <- readQ(files = Q_file_list)
sampleID <- read.table('admixture_sampleID.txt',header = F,stringsAsFactors = F)  #样本ID

num <- length(Q_file_list)

groupid <- paste('Group',seq(1,num+1,1),sep = '')  #顶部显示Group
K_id <- paste('\nK=',seq(2,num+1,1),'\n',sep = '')  #右侧K值

plotQ(all_Q_file_data[1:num],imgoutput="join",panelspacer = 1,
      barsize = 1,barbordersize = 0,clustercol=c("#1D72F5","#DF0101","#77CE61", "#FF9326","#A945FF","#0089B2","#FDF060","#FFA6B2","#BFF217","#60D5FD","#CC1577","#F2B950","#7FB21D","#EC496F","#326397","#B26314","#027368","#A4A4A4","#610B5E"), #是否串联画图，簇的颜色
      showindlab=T,indlabwithgrplab=T,showgrplab = F,grplab=sampleID,
      showlegend=T,legendpos="right",legendkeysize=40,legendtextsize=25,legendlab=groupid, #顶部Group设置
      showsp = T,sppos="right",splabcol='#517885',splabsize=35,splabface="plain",splab = K_id,spbgcol="#b2dbe9",#右侧条带的所有设置
      useindlab=T,indlabsize=18,indlabheight=20,indlabspacer=10,
      width = 100,height = 12,units = 'cm',outputfilename="admixture_plot",imgtype="pdf",dpi=600,exportpath=getwd())