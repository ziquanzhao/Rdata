install.packages("gggenes")
library(gggenes)
library(ggplot2)

mydata <- read.table('gene.txt',header = T)

pdf('structure.pdf',width = 10,height = 4)
ggplot(mydata, aes(xmin = start, xmax = end, y = molecule, fill = gene,label = gene, forward = direction)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()
dev.off()