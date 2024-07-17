if (!require("openxlsx")){
  install.packages("openxlsx")
}
library(openxlsx)

YourCountsData <- read.table('WX-72-counts.txt')
rowname <- YourCountsData$V1
rowname <- rowname[2:nrow(YourCountsData)]
colname <- YourCountsData[1,]
colname <- colname[2:length(colname)]
#读取数据

YourCountsData <- as.data.frame(lapply(YourCountsData[2:nrow(YourCountsData),2:ncol(YourCountsData)],as.numeric))
#将数据都转换为数值型

colnames(YourCountsData) <- colname
#整理好列名

YourCountsData <- data.frame(rowname,YourCountsData)
#整理好行名

YourCountsDataFilter <- YourCountsData[rowSums(YourCountsData[,2:ncol(YourCountsData)])>0,]
#删除在不同样本中表达量都为0的基因

Li <- nrow(YourCountsData)/1000
Ni_to_Li <- YourCountsDataFilter[,2:ncol(YourCountsDataFilter)]/Li

#TPM计算如下
TPM <- t(t(Ni_to_Li)/colSums(Ni_to_Li)*10^6)
TPM_ID_Normal <- data.frame(YourCountsDataFilter[,1],TPM)
TPM_ID_log <- data.frame(YourCountsDataFilter[,1],log2(TPM+1))
sheet <- list("Normal_TPM" = TPM_ID, "log2(x+1)_TPM" = TPM_ID_log)
write.xlsx(sheet, file = 'YourFinalTPM.xlsx')


#FPKM计算如下：
FPKM <- t(t(Ni_to_Li)/colSums(YourCountsDataFilter[,2:ncol(YourCountsDataFilter)])*10^6)
FPKM_ID_Normal <- data.frame(YourCountsDataFilter[,1],FPKM)
FPKM_ID_log <- data.frame(YourCountsDataFilter[,1],log2(FPKM+1))
SHEET <- list("Normal_FPKM" = FPKM_ID_Normal, "log2(x+1)_FPKM" = FPKM_ID_log)
write.xlsx(SHEET,file = 'YourFinalFPKM.xlsx')
