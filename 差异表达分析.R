#用于差异表达分析
#BiocManager::install("DESeq2") 

#用于KEGG分析
#BiocManager::install("clusterProfiler")
#BiocManager::install("org.Hs.eg.db")

for (package in c("DESeq2","clusterProfiler","pathview")) {
  if (!requireNamespace(package, quietly = TRUE))
    BiocManager::install(package)
}


rm(list = ls())

library(DESeq2)
library(openxlsx)
library(ggplot2)

#读取数据，转化分组
dat <- read.xlsx(xlsxFile = "Ganhan_TPM.xlsx",sheet = "Sheet1",rowNames = T,check.names = F)
coldata <- data.frame(condition = factor(rep(c('control', 'treat'), each = 3), levels = c('control', 'treat')))

# 将数据四舍五入为整数，有时候有些基因表达量较低，直接四舍五入会有很大偏差，×1000后再分析
dat <- dat*1000
dat <- round(dat)

#第一步，构建 DESeqDataSet 对象
dds <- DESeqDataSetFromMatrix(countData = dat, colData = coldata, design= ~condition)

#第二步，计算差异倍数并获得 p 值
#备注：parallel = TRUE 可以多线程运行，在数据量较大时建议开启
dds1 <- DESeq(dds, fitType = 'mean', minReplicatesForReplace = 7, parallel = T)

#注意，需将 treat 在前，control 在后，意为 treat 相较于 control 中哪些基因上调/下调
res <- results(dds1, contrast = c('condition', 'treat', 'control'))

#输出表格至本地
res1 <- data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
#baseMean是所有样本平均表达量，log2FoldChange是差异倍数，ifcSE、stat、pvalue不用管，padj是多重比较后的p值，显著性，越小越好
write.csv(x = res1,file = "ganhan_DES.csv")

##筛选差异表达基因
#首先对表格排个序，按 padj 值升序排序，相同 padj 值下继续按 log2FC 降序排序
res1 <- res1[order(res1$padj, res1$log2FoldChange, decreasing = c(FALSE, TRUE)), ]

#log2FC≥1 & padj<0.01 标识 up，代表显著上调的基因
#log2FC≤-1 & padj<0.01 标识 down，代表显著下调的基因
#其余标识 none，代表非差异的基因
res1[which(res1$log2FoldChange >= 1 & res1$padj < 0.01),'sig'] <- 'up'
res1[which(res1$log2FoldChange <= -1 & res1$padj < 0.01),'sig'] <- 'down'
res1[which(abs(res1$log2FoldChange) <= 1 | res1$padj >= 0.01),'sig'] <- 'none'

#输出选择的差异基因总表
res1_select <- subset(res1, sig %in% c('up', 'down'))
write.csv(res1_select, file = 'ganhan_DESeq2.csv')


##ggplot2 差异火山图
#ggplot2默认颜色
#library(scales)
#show_col(hue_pal()(n)) #n为1~6，show_col(hue_pal()(3))

#默认情况下，横轴展示 log2FoldChange，纵轴展示 -log10 转化后的 padj
pdf(file = "DEGs_Huoshan.pdf")
ggplot(data = res1, aes(x = log2FoldChange, y = -log10(padj), color = sig)) +
  geom_point(size = 1) +  #绘制散点图
  scale_color_manual(values = c('#F8766D', 'gray', '#00BA38'), limits = c('up', 'none', 'down')) +  #自定义点的颜色
  labs(x = 'log2 Fold Change', y = '-log10 adjust p-value', title = 'control vs treat', color = '') +  #坐标轴标题
  theme(plot.title = element_text(hjust = 0.5, size = 14), panel.grid = element_blank(), #背景色、网格线、图例等主题修改
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = c(-1, 1), lty = 3, color = 'black') +  #添加阈值线
  geom_hline(yintercept = 2, lty = 3, color = 'black') +
  xlim(-15, 15) + ylim(0, 35)  #定义刻度边界
dev.off()

