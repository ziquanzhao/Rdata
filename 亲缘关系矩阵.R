library(pheatmap)

kingship <- read.table('allchr_kinship.txt',header = F,stringsAsFactors = F)
kingshiprowname <- kingship[,1]
kingship <- kingship[1:164,2:165]
row.names(kingship) <- kingshiprowname
kingship <- as.matrix(kingship)
kingship <- apply(kingship,2,as.numeric)

pheatmap(mat = kingship,
         treeheight_row = 30,treeheight_col = 40,
         show_colnames = T,fontsize_col = 3,
         labels_col = kingshiprowname,
         filename = 'kingship.pdf')