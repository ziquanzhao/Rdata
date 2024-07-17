if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("AnnotationDbi", "impute","GO.db", "preprocessCore"),force = T)
install.packages(c("WGCNA", "stringr", "reshape2"), repos="https://mirrors.tuna.tsinghua.edu.cn/CRAN")
#下载包

rm(list=ls())

mycolor <- c('#ff3030','#ff4747','#ff5e5e','#ff7575','#ff8c8c','#ffa3a3','#ffbaba','#ffd1d1','#ffd0c8','#ffefed','#ffffff','#eaebf5','#e5edf5','#cbdbec','#b1cae2','#97b8d8','#7ca6cf','#6294c5','#4883bb','#2e71b2','#145fa8')

library(WGCNA)
library(openxlsx)
options(stringsAsFactors = F)
enableWGCNAThreads()
# 加载包，打开多线程


FpkmData <- read.xlsx(xlsxFile = 'YourFinalFPKM.xlsx', sheet = 'Normal_FPKM',colNames = T,rowNames = T)
FpkmData <- as.data.frame(t(FpkmData))
#处理数据格式，要求每列为基因，每行为一个样本。并且为数值矩阵。

FpkmData <- FpkmData[,colSums(FpkmData)>=1]
#过滤掉不表达的基因。

gsg <- goodSamplesGenes(FpkmData, verbose = 3)
gsg$allOK
if (!gsg$allOK)
{
  if (sum(!gsg$goodGenes)>0)
    printFlush(paste("Removing genes:", paste(names(FpkmData)[!gsg$goodGenes], collapse = ", ")));
  if (sum(!gsg$goodSamples)>0)
    printFlush(paste("Removing samples:", paste(rownames(FpkmData)[!gsg$goodSamples], collapse = ", ")));
  FpkmData <- FpkmData[gsg$goodSamples, gsg$goodGenes]
}
#这一步是检测样本或者基因是否有过多缺失值，如果gsg$allOK结果为TURE，则证明数据完好。否则就会运行if语句中的内容，删除缺失过多的样本或者基因


sampleTree <- hclust(dist(FpkmData), method = "average")
pdf(file = "Sample clustering to detect outliers.pdf", width = 8, height = 4)
par(cex = 0.6)
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)
abline(h = 55, col = "red") #h=15（默认为15）就是离群分界线，超15的认为离群
#FpkmData <- FpkmData[-x,]
#这一步查看离群样本，并去除离群样本
#如果有离群的样本，通过上一行代码将其删除，其中x代表要删除的行
dev.off()


Traitdata <- read.xlsx('Seedsize.xlsx',sheet = 'Sheet1', colNames = T, rowNames = T)
#读取表型数据
collectGarbage()
#内存垃圾回收，清理内存用的


#在我们继续进行网络构建和模块检测之前，我们想看一下表型数据与样本基因表达量树状图之间的关系。
sampleTree2 <- hclust(dist(FpkmData), method = "average")
traitColors <- numbers2colors(Traitdata, signed = FALSE)
pdf(file = 'Sample dendrogram and trait heatmap.pdf',width = 8,height = 6)
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = names(Traitdata),
                    cex.colorLabels = 0.7,#性状标签大小
                    cex.dendroLabels = 0.6,#样本ID标签大小
                    main = "Sample dendrogram and trait heatmap")
dev.off()
#如图所示，白色表示低值，红色表示高值，灰色表示缺少条目。


powers <- c(c(1:10), seq(from = 12, to=20, by=2))
sft <- pickSoftThreshold(FpkmData, powerVector = powers, verbose = 5)
pdf(file = 'Soft thresholding.pdf', width = 10, height = 5 )
par(mfrow = c(1,2))
cex1 = 0.9
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit signed R^2",type="n",
     main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red")
abline(h=0.85,col="red")
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
dev.off()
#这是软阈值计算，根据图选择合适的软阈值。


#设置软阈值为6
softPower <- 10
adjacency <- adjacency(FpkmData, power = softPower)


#为了减少背景干扰和假阳性，将邻接法转换TOM矩阵
TOM <- TOMsimilarity(adjacency)
dissTOM <- 1-TOM


#使用TOM进行聚类
geneTree <- hclust(as.dist(dissTOM), method = "average")
pdf(file = 'Gene clustering on TOM-based dissimilarity.pdf', width = 15, height = 5)
plot(geneTree, xlab="", sub="", main = "Gene clustering on TOM-based dissimilarity",
     labels = FALSE, hang = 0.04)
dev.off()


#聚类模块并上色
dynamicMods <- cutreeDynamic(dendro = geneTree, distM = dissTOM,
                            deepSplit = 2, pamRespectsDendro = FALSE,
                            minClusterSize = 100)#一个模块中的基因如果数量太少的话，其实意义不大，因此我们规定了模块最小基因数量为30
table(dynamicMods)#返回n多个模块，模块0表示没有聚类上的基因，模块1到模块n则是聚类上的基因
dynamicColors = labels2colors(dynamicMods)
pdf(file = 'Gene dendrogram and module colors.pdf', width = 12, height = 9)
plotDendroAndColors(geneTree, dynamicColors, "Dynamic Tree Cut",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Gene dendrogram and module colors")
dev.off()


#很多模块虽然被分开了，但是他们的表达谱非常相似，因此我们要合并相似的模块，我们选择相关性在0.75以上的模块合并。
MEList <- moduleEigengenes(FpkmData, colors = dynamicColors)
MEs <- MEList$eigengenes
MEDiss <- 1-cor(MEs)
METree <- hclust(as.dist(MEDiss), method = "average")
pdf(file = 'Clustering_of_module_eigengenes.pdf', width = 14, height = 12)
plot(METree, main = "Clustering of module eigengenes",
     xlab = "", sub = "")
MEDissThres = 0.25#选择合并相关性在0.75以上的模块，因此切割阈值就是0.25
abline(h=MEDissThres, col = "red")
merge <- mergeCloseModules(FpkmData, dynamicColors, cutHeight = MEDissThres, verbose = 3)
mergedColors <- merge$colors
mergedMEs <- merge$newMEs
dev.off()


#为了对比合并前和合并后的区别，再次绘制聚类模块图
pdf(file = "Clustering of module eigengenes another.pdf", width = 12, height = 8)
plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
                    c("Dynamic Tree Cut", "Merged dynamic"),
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()


#保存整理模块颜色变量
moduleColors <- mergedColors
colorOrder <- c("grey", standardColors(50))#要求返回多少颜色，可以改,"Grey"颜色说明是为关联到的基因，要排除
moduleLabels <- match(moduleColors, colorOrder)-1
MEs <- mergedMEs


#重新定义MEs颜色标签
nGenes <- ncol(FpkmData)
nSamples <- nrow(FpkmData)
MEs0 <- moduleEigengenes(FpkmData, moduleColors)$eigengenes
MEs <- orderMEs(MEs0)
moduleTraitCor <- cor(MEs, Traitdata, use = "p")
moduleTraitPvalue <- corPvalueStudent(moduleTraitCor, nSamples)


#绘制模块性状相关性热图
pdf(file = 'Module-trait relationships.pdf',width = 10, height = 6)
textMatrix <- paste(signif(moduleTraitCor, 2), "\n(",
                   signif(moduleTraitPvalue, 1), ")", sep = "")
dim(textMatrix) <- dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3))
labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = names(Traitdata),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = TRUE,
               colors = mycolor,
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 1,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))
dev.off()


#计算基因表达模块和性状间的相关性
HWG <- as.data.frame(Traitdata$HWG)
names(HWG) <- "HWG"
modNames <- substring(names(MEs), 3)
geneModuleMembership <- as.data.frame(cor(FpkmData, MEs, use = "p"))
MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))
names(geneModuleMembership) <- paste("MM", modNames, sep="");
names(MMPvalue) <- paste("p.MM", modNames, sep="")
geneTraitSignificance <- as.data.frame(cor(FpkmData, HWG, use = "p"))
GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))
names(geneTraitSignificance) <- paste("GS.", names(HWG), sep="")
names(GSPvalue) <- paste("p.GS.", names(HWG), sep="")
#HWG是你性状的ID，需要自己改,注意有多处，都要修改


#自己查看相关性矩阵后，会发现有些模块和你的性状相关性很高，选择那个模块的颜色作为输入条件，绘制基因性状散点图
#当然，也可以选择不绘图
module <- "greenyellow" #选择你感兴趣的模块颜色
column <- match(module, modNames)
moduleGenes <- moduleColors==module
pdf(file = 'My_instering_model_MEgreenyellow_SeedNumber.pdf', height = 10, width = 10)#注意PDF文件命名和感兴趣的模块颜色要一致
par(mfrow = c(1,1))
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for body weight",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = 'red',pch = 20)
dev.off()


#输出模块结果
names(FpkmData)
names(FpkmData)[moduleColors=="greenyellow"]#选择你感兴趣的模块颜色
annot <- read.csv(file = "YourGeneAnnotation.csv")#这个注释文件其实就要一列即可，这列的表头关键字是“GeneID”，数据就是你的所有基因的ID，注意要和你FPKM值数据中的基因ID保持一致
probes <- names(FpkmData)
probes2annot <- match(probes, annot$GeneID)#GeneID作为你的基因ID表头
sum(is.na(probes2annot))#这是计算由多少个基因ID没有匹配上，正确的返回结果应该是0

#将数据写入文件
geneInfo0 <- data.frame(GeneID = probes,
                       moduleColor = moduleColors,
                       geneTraitSignificance,
                       GSPvalue)
modOrder <- order(-abs(cor(MEs, HWG, use = "p")))#注意修改性状ID
for (mod in 1:ncol(geneModuleMembership))
{
  oldNames <- names(geneInfo0)
  geneInfo0 <- data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]],
                         MMPvalue[, modOrder[mod]]);
  names(geneInfo0)<- c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
                       paste("p.MM.", modNames[modOrder[mod]], sep=""))
}
geneOrder <- order(geneInfo0$moduleColor, -abs(geneInfo0$GS.HWG))#修改性状ID
geneInfo <- geneInfo0[geneOrder, ]
write.csv(geneInfo, file = "YourFinalCorrelationResult.csv")#最后的关联结果保存


#基因表达网络可视化，做一个不同基因间表达相关性的热图，在这个图中，同一模块中的基因往往具有较高相关性，这是正常的
#但是我们还想看看不同模块之间的基因表达是否具有较强的相关性。
nSelect <- 400#几万个基因绘制热图需要消耗大量时间和算力，因此我们在每个模块中随机挑选400个基因做相关性分析。
set.seed(10)
select <- sample(nGenes, size = nSelect)
selectTOM <- dissTOM[select, select]
selectTree <- hclust(as.dist(selectTOM), method = "average")
selectColors <- moduleColors[select]
pdf(file = 'Network heatmap plot selected genes.pdf',width = 18,height = 18)
plotDiss <- selectTOM^7
diag(plotDiss) <- NA
TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot selected genes")
dev.off()


#可视化特征基因的表达网络，把性状作为一个模块，加入基因模块之间的相关性分析和聚类中，这样和性状模块聚到一起的且
#相关性较高的那个模块就是我们所想要的关键模块
# 重新计算模块的特征基因
MEs <- moduleEigengenes(FpkmData, moduleColors)$eigengenes
# 将性状模块加入基因模块中
HWG <- as.data.frame(Traitdata$HWG)
names(HWG) <- "HWG"
MET <- orderMEs(cbind(MEs, HWG))#此步注意修改性状ID
#下面开始绘图
pdf(file = 'Eigengene dendrogram and adjacency heatmap.pdf',width = 5,height = 7.5)
par(cex = 0.7)
plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle = 0)
dev.off()

#通过TOM值筛选关键基因
#通过上步我们已经确定了我们最感兴趣的模块，现在我们要绘制一下这个模块中的基因的节点网络图
#这样首先可以简单确定一下基因之间的关系（可以猜测一下不同基因间上下游关系或者互作关系）
#其次可以确定这个模块中那些基因是关键节点基因，这个基因就是我们的性状候选基因。
#TOM值（模块调控系表中的weight值）大于阈值（默认是0.15)的两个基因才认为是相关的，然后计算每个基因的连接度。即先筛选有足够强度的关系，然后计算连接度。
#导出VisANT网络文件
annot <- read.csv(file = "YourGeneAnnotation.csv")
module <- "darkgrey"
probes <- names(FpkmData)
inModule <- (moduleColors==module)
modProbes <- probes[inModule]
modTOM <- TOM[inModule, inModule]
dimnames(modTOM) <- list(modProbes, modProbes)
nTop <- 30#仅输出该模块中前30个顶级中枢基因，可以修改
IMConn <- softConnectivity(FpkmData[, modProbes])
top <- (rank(-IMConn) <= nTop)
vis <- exportNetworkToVisANT(modTOM[top, top],
                            file = paste("VisANT-", module, "-top30.txt", sep=""),
                            weighted = TRUE,
                            threshold = 0.1,#阈值根据需要而定
                            probeToGene = data.frame(annot$GeneID, annot$GeneSymbol) )

#导出Cytoscape网络文件
annot = read.csv(file = "YourGeneAnnotation.csv")
module <- "darkgrey"
probes <- names(FpkmData)
inModule = is.finite(match(moduleColors, module))
modProbes = probes[inModule]
modGenes = annot$GeneSymbol[match(modProbes, annot$GeneID)]
modTOM = TOM[inModule, inModule]
dimnames(modTOM) = list(modProbes, modProbes)
cyt = exportNetworkToCytoscape(modTOM,
                               edgeFile = paste("CytoscapeInput-edges-", paste(module, collapse="-"), ".txt", sep=""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(module, collapse="-"), ".txt", sep=""),
                               weighted = TRUE,
                               threshold = 0.02,#阈值根据需要而定
                               nodeNames = modProbes,
                               altNodeNames = modGenes,
                               nodeAttr = moduleColors[inModule])



#通过计算kME值来筛选关键基因（官方）
datKME <- signedKME(FpkmData, MEs, outputColumnName="kME_MM.")
module <- "darkgrey"
column <- match(module, modNames)
moduleGenes = moduleColors==module
darkturquoise_module <- as.data.frame(dimnames(data.frame(FpkmData))[[2]][moduleGenes])
names(darkturquoise_module) <- "GeneID"
darkturquoise_KME <- as.data.frame(datKME[moduleGenes,column]) 
names(darkturquoise_KME) <- "KME"
rownames(darkturquoise_KME) <- darkturquoise_module$GeneID
FilterGenes <- abs(darkturquoise_KME$KME) > 0.5#kME阈值
darkturquoise_hub = subset(darkturquoise_KME, abs(darkturquoise_KME$KME)>0.5)#kME阈值
write.csv(darkturquoise_hub, "Hubgene_KME_darkturquoise.csv")

#如何筛选候选基因：
#1.TOM值筛选，直接最好模块的前30个顶级中枢基因，结果在VisANT-darkturquoise-top30.txt文件中，threshold这个阈值默认是0.5
#2.|GS|>0.2,|MM|>0.8，最常用，将该基因模块身份MM相对于基因显著性GS做散点图，选择右上角MM和GS均高的基因进一步分析。
#   基因显著性值（Gene significance,GS)因变量水平的相关系数。衡量基因与表型性状的关联程度，GS越高，说明与表型越相关，越具有生物学意义。GS可以为正值或负值（正相关或负相关）
#   就是看YourFinalCorrelationResult.csv中的GS.HWG和MM.[color],注意p值要显著
#3.计算kME值，依据|kME|≥0.7筛选.