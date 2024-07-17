rm(list = ls())

library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)

GeneID <- "LOC_Os02g49700"

FileName <- paste("D:/SoftwareData/Rdata/RiceHap/ProcessedHapFile/",GeneID,"-HAP.txt",seq = "")
FileName <- str_replace_all(FileName,"\\s+","")

mydata <- read.table(file = FileName,header = T,sep = "\t")
mydata[is.na(mydata)] <- 0


YearName <- colnames(mydata)[2:10]
PlotList <- list()

for (Name in YearName) {
  plot_data <- aggregate(mydata[[Name]], by=list(type = mydata$HAP), sum) #[[]]可以访问动态列名，$只能访问静态列名
  myplot <- ggplot(plot_data,aes(x = "", y = x, fill = type))+
    geom_bar(stat = "identity", width = 1)+
    coord_polar(theta = "y",start = 0)+
    labs(title = Name,x = "",fill = "Hap")+
    scale_fill_manual(values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3","#B8B8B8"))+
    theme_void()+
    theme(legend.position="right",legend.title = element_text(size = 8))+
    theme(plot.title = element_text(color="black", size=12, face="plain",hjust = 0.5,vjust = 0.5))
  PlotList[[Name]] <- myplot
}

plot_grid(plotlist = PlotList,
          align = "h", 
          ncol = 3,
          #labels = c("a","b","c")
          #label_size = 14,
          #label_fontfamily = "serif",
          label_fontface = "bold")

ggsave(filename = str_replace_all(paste("D:/SoftwareData/Rdata/RiceHap/ProcessedHapFile/",GeneID,".pdf",seq = ""),"\\s+",""),
       device = "pdf",units = "in",width = 8.79,height = 5.55)



rm(list = ls())
AllFileName <- list.files(path = "D:/SoftwareData/Rdata/数据框转置并输出/",pattern = "\\-HAP.txt")
for (GeneIDFile in AllFileName) {
  
  GeneID <- strsplit(GeneIDFile,split = "-HAP")[[1]][1]
  
  FileName <- paste("D:/SoftwareData/Rdata/数据框转置并输出/",GeneID,"-HAP.txt",seq = "")
  FileName <- str_replace_all(FileName,"\\s+","")
  
  mydata <- read.table(file = FileName,header = T,sep = "\t")
  mydata[is.na(mydata)] <- 0
  
  YearName <- colnames(mydata)[2:10]
  PlotList <- list()
  
  for (Name in YearName) {
    plot_data <- aggregate(mydata[[Name]], by=list(type = mydata$HAP), sum) #[[]]可以访问动态列名，$只能访问静态列名
    myplot <- ggplot(plot_data,aes(x = "", y = x, fill = type))+
      geom_bar(stat = "identity", width = 1)+
      coord_polar(theta = "y",start = 0)+
      labs(title = Name,x = "",fill = "Hap")+
      scale_fill_manual(values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3","#B8B8B8"))+
      theme_void()+
      theme(legend.position="right",legend.title = element_text(size = 8))+
      theme(plot.title = element_text(color="black", size=12, face="plain",hjust = 0.5,vjust = 0.5))
    PlotList[[Name]] <- myplot
  }
  
  plot_grid(plotlist = PlotList,
            align = "h", 
            ncol = 3,
            #labels = c("a","b","c")
            #label_size = 14,
            #label_fontfamily = "serif",
            label_fontface = "bold")
  
  ggsave(filename = str_replace_all(paste("D:/SoftwareData/Rdata/循环饼图/",GeneID,".pdf",seq = ""),"\\s+",""),
         device = "pdf",units = "in",width = 8.79,height = 5.55)
}



result <- data %>% group_by(HAP) %>% summarise(merged_1950s = paste(na.omit(1950s), collapse = "; "))