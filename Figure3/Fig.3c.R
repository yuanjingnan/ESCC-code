


library(grDevices, lib.loc = "/usr/local/Cellar/r/4.2.1_3/lib/R/library")
library(dplyr)
library(pheatmap)
library(ggplot2)

dir <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3c/"


############ Fig3c ############ 

bk <- c(seq(-2,-0.1,by=0.01),seq(0,2,by=0.01))
color = c(colorRampPalette(colors = c("#148259","white","#6d245d"))(length(bk)))


gene12_input <- read.table(file = paste(dir,"Fig3c.txt",sep = ""),header = T,sep = "\t" )

pdf(paste(dir,"12gene.pdf",sep = ""),width=4, height=3)
pheatmap::pheatmap(gene12_input,
                   scale = "row",
                   cluster_cols=F,
                   cluster_rows = F,
                   clustering_method="ward.D",
                   border_color=NA,
                   color=color)
dev.off()
############ Fig3c ############ 
