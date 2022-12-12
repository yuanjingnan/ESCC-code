


library(grDevices, lib.loc = "/usr/local/Cellar/r/4.2.1_3/lib/R/library")
library(dplyr)
library(pheatmap)
library(ggplot2)

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3c/"


############ Fig3c ############ 
load(paste(dir,"data_12gene.rda",sep = ""),verbose = T)#data_12gene

cli_anno <- read.table(file = paste(dir,"Subtype_Pre_Post.txt",sep = ""),header = T,sep = "\t" )
cli_anno <- cli_anno %>% dplyr::select(Mandard,RNA_Tumor_Sample_Barcode,Subtype,Response)
cli_anno$Mandard <- factor(cli_anno$Mandard, levels = c("TRG1/2","TRG3","TRG4"))
cli_anno$Response <- factor(cli_anno$Response, levels = c("Well responders","Poor responders"))
rownames(cli_anno) <- cli_anno$RNA_Tumor_Sample_Barcode
cli_anno$RNA_Tumor_Sample_Barcode <- factor(cli_anno$RNA_Tumor_Sample_Barcode,levels = cli_anno$RNA_Tumor_Sample_Barcode)
order_T1 <- c("ESCC-P13-T1", "ESCC-P16-T1", "ESCC-P06-T1", "ESCC-P08-T1", "ESCC-P09-T1", "ESCC-P05-T1", "ESCC-P01-T1", "ESCC-P10-T1", "ESCC-P12-T1", "ESCC-P07-T1", "ESCC-P04-T1", "ESCC-P21-T1", "ESCC-P20-T1")

bk <- c(seq(-2,-0.1,by=0.01),seq(0,2,by=0.01))
color = c(colorRampPalette(colors = c("#148259","white","#6d245d"))(length(bk)))



pdf(paste(dir,"12gene.pdf",sep = ""),width=4, height=3)
pheatmap::pheatmap(data_12gene[,order_T1],
                   scale = "row",
                   cluster_cols=F,
                   cluster_rows = F,
                   clustering_method="ward.D",
                   border_color=NA,
                   color=color)
dev.off()
############ Fig3c ############ 
