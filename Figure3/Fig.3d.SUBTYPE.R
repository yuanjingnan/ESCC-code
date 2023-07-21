


dir <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3d/SUBTYPE/"

########### fig3d  ###########


bk <- c(seq(-2,-0.1,by=0.01),seq(0,2,by=0.01))
color = c(colorRampPalette(colors = c("#155F83FF","white"))(length(bk)/2),colorRampPalette(colors = c("white","#800000FF"))(length(bk)/2))


heatmap_input <- read.table(file = paste(dir,"Fig.3d1.txt",sep = ""),header = T,sep = "\t" )
colnames(heatmap_input) <- gsub("\\.", "-", colnames(heatmap_input))
cli_anno <- read.table(file = paste(dir,"Fig.3d2.txt",sep = ""),header = T,sep = "\t" )

P <- pheatmap::pheatmap(heatmap_input,
                        scale = "row",
                        labels_col = sample_order_label,
                        cluster_cols=F,
                        breaks=bk,
                        cluster_rows = F,
                        border_color="white",
                        color=color,
                        fontsize_row = 8,
                        annotation_colors=list(
                          Response=c('Well responders'='#3C5488FF','Poor responders'='#E64B35FF'),
                          MPR= c('YES'='#D49A9E','NO'='#E3E3E3'),
                          `TME`= c("IE"="#E31A1C","FE"="#FDBF6F","TP"="#1F78B4"),
                          `PD.L1.expression`= c('YES'='#D49A9E','NO'='#E3E3E3'),
                          `Fribroblasts.fraction`= c('YES'='#D49A9E','NO'='#E3E3E3'),
                          `IFN.EMT.score`= c('YES'='#D49A9E','NO'='#E3E3E3')),
                        annotation_col = cli_anno)

pdf(paste(dir,"Subtype.pdf",sep = ""),width=6, height=3.5)
print(P)
dev.off()

########### fig3d  ###########