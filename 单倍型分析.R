#if(! require(BiocManager)) 
#  install.packages("BiocManager")
#BiocManager::install(c("Biostrings", "GenomicRanges", "muscle", "IRanges", "rtracklayer", "trackViewer"))
#install.packages("geneHapR")
#geneHap不容易安装，如果遇到某些包安装不上，把所有包全部删除，然后一切重新安装；
#或者直接卸载当前版本的R（例如：4.4.0），然后重新安装最新版本的R（4.4.1）


rm(list = ls())
library(geneHapR)
library(maps)

#官网文档https://gitee.com/zhangrenl/genehapr/wikis/Introduction.md

#导入数据
vcf <- import_vcf("MSU7_geno0.1_maf0.05.vcf.gz")
gff <- import_gff("rice.gff3", format = "GFF")
AccINFO <- import_AccINFO("rice-accinfo.txt")
pheno <- import_AccINFO("rice-phenotype.txt")
gene <- read.table("rice-geneID.txt",header = T)

i <- 4
gene_chr <- gene$Chr[i]
gene_start <- gene$Start[i]
gene_end <- gene$End[i]
geneID <- gene$GeneID[i]
hapPrefix <- "Hap"

#过滤vcf数据，只保留落在[start,end]区域的变异位点
vcf_gene_pos <- filter_vcf(vcf = vcf,gff = gff,mode = "POS",start = gene_start,end = gene_end,Chr = gene_chr)

#单倍型分析，我们假设所有种质都是不相关的，因此在单倍型鉴定之前不对基因型进行插补
hapResult <- vcf2hap(vcf = vcf_gene_pos, hapPrefix = hapPrefix, # 单倍型名称的前缀
                     hetero_remove = F, # 是否移除包含杂合位点的SNP
                     na_drop = F)       # 移除包含基因型缺失的SNP

#这一步可以不做，在后续的单倍型表格中增加一行注释，注意values中的标签个数要与SNP个数一致，例如可以加入SNP是否在CDS上的注释
hapResult <- addINFO(hapResult,
                     tag = "Protein",  #标签ID
                     values = c("C->D", "V->R", "G->N",
                                "C->D", "V->R", "G->N",
                                "C->D", "V->R", "G->N","G->N"),  #标签内容
                     replace = TRUE)    #是否替代原有注释标签

#假设某些样本中位点缺失，并且将缺失标记为“DEL”，需要先择是否要删除这些含有缺失位点的样本
#hapResult[hapResult == "DEL"] <- NA
##hapResult <- hapResult[-which(rowSums(is.na(hapResult))>0),]  #is.na(hapResult)会生成一个矩阵，缺失记为TRUE，非缺失值为FALSE，rowSums()对这个矩阵的行求和，那么含有缺失值的行会大于0，得到TRUE，which()根据TRUE找到相应行号，我们再取反，即可得到不含缺失值的行号


hapSummary <- hap_summary(hapResult,hapPrefix = hapPrefix)  #这一步是获取一个简明的结果

#过滤单倍型，如果某种单倍型在群体中的频率小于5个，则移除该单倍型
hapSummary <- filter_hap(hap = hapSummary,rm.mode = "freq",freq.min = 8)

#将单倍型分析结果写入文件
write.hap(x = hapSummary,file = paste(geneID,"HapSummary.txt",sep = "_"))

#如果没有加入注释标签，就用这个代码绘制单倍型位点图，就是不同单倍型的位点信息及频率统计表
plotHapTable(hapSummary,             # 单倍型结果
             hapPrefix = hapPrefix,  # 单倍型名称前缀
             angle = 0,              # 物理位置的角度
             displayIndelSize = 4,   # 图中展示最大的Indel大小
             title = geneID)         # 图片标题

#如果加入注释标签，就用这个代码绘制单倍型位点图，就是不同单倍型的位点信息及频率统计表
plotHapTable(hapSummary,
             hapPrefix = hapPrefix,
             INFO_tag = "Protein",
             displayIndelSize = 4, 
             angle = 0,
             title = geneID)


#绘制一个基因结构图，并把变异位点标上去
displayVarOnGeneModel(gff = gff, hapSummary = hapSummary,Chr = gene_chr,
                      startPOS = gene_start-50,
                      endPOS = gene_end+50,
                      CDS_h = 0.05, fiveUTR_h = 0.025, threeUTR_h = 0.025, # CDS,UTR条块的宽度
                      cex = 0.8) # 变异位点的标记大小

hapSummary[hapSummary == "DEL"] <- "N"               # 替换空值，有些位点基因型缺失
hapnet <- get_hapNet(hapSummary,                  # 单倍型结果
                     AccINFO = AccINFO,           # 包含样本分类信息的数据框(data.frame)
                     groupName = "Subpopulation") # 含有样本分类信息的列名称

plotHapNet(hapnet,                          # 单倍型网络
           scale = "log2",                  # 标准化方法"log10"或"log2"或"none"
           show.mutation = 2,               # 是否展示变异位点数量, 0,1,2,3
           col.link = 2, link.width = 2,    # 单倍型之间连线的颜色和宽度
           main = geneID,                   # 主标题
           pie.lim = c(0.5, 2),             # 饼图的大小，映射范围
           labels = T,                      # 是否在单倍型上添加label
           labels.cex = 0.7,labels.col = 1, # 就是“Hap001”的字体大小,以及颜色
           legend_version = 1,              # 图例形式（0或1）
           legend = c(10,1),                # 图例的坐标
           cex.legend = 0.8)                # 图例中文字的大小

AccINFO$Longitude <- as.numeric(AccINFO$Longitude)
AccINFO$Latitude <- as.numeric(AccINFO$Latitude)
hapDistribution(hapResult,             # 单倍型结果
                AccINFO = AccINFO,     # 含有地理坐标的数据框（data.frame）
                hapNames = c("Hap001","Hap002", "Hap003"),  # 展示的单倍型名称建议不超过3个
                symbol.lim = c(2, 6),  # 圆圈的大小,样本量映射范围
                label.cex = 0.6,       #  样本量编号
                LON.col = "Longitude", # 经纬度所处的列名称
                LAT.col = "Latitude",  # 经纬度所处的列名称
                legend = "bottomleft", # 图例所处的位置
                cex.legend = 0.7,      # 图例大小
                main = geneID)         # 主标题


plot_LDheatmap(hap = hapResult, # 单倍型结果
               add.map = TRUE,     # 是否添加基因模式图
               gff = gff,       # 注释信息
               Chr = Chr,       # 染色体名称
               start = start,   # 基因的起始位置
               end = end)       # 基因的终止位置（更多参数参见帮助文档）

hapVsPheno(hap = hapResult,       # 单倍型分析结果
           pheno = pheno,         # 表型,里面有很多表型数据
           phenoName = names(pheno)[10],  #指定一个表型
           hapPrefix = hapPrefix, # 单倍型名称的前缀
           title = geneID,        # 主标题
           minAcc = 5,            # 参与p值计算所需的最小样本数
           #symnum.args = list(    # 定义显著性标注方式
           #  cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
           #  symbols = c("***", "**", "*", "ns")),
           method = "t.test",
           mergeFigs = F)     # 结果包括两个图，是否融合成一张图

# 逐位点比较变异效应
hapVsPhenoPerSite(hap = hapResult,              # 单倍型分析结果
                  pheno = pheno,                # 表型文件
                  phenoName = "Plant_height") # 表型名称

# 回车继续下一位点
# ESC退出当前命令


#效应值计算
EFF <- siteEFF(hapResult, pheno)
plotEFF(EFF, gff = gff,
        Chr = Chr, start = start, end = end,
        showType = c("five_prime_UTR", "CDS", "three_prime_UTR"), # see help(plotEFF)
        y = "effect",                      # the means of y axis, one of effect or pvalue
        ylab = "effect",                   # label of y axis
        cex = 0.5,                         # Cex
        legend.cex = 0.8,                  # legend size
        main = geneID,                     # main title
        CDS.height = 1,                    # controls the height of CDS, heights of others will be half of that
        markMutants = TRUE,                # mark mutants by short lines
        mutants.col = 1, mutants.type = 1, # parameters for appearance of mutants
        pch = 20)                          # points type