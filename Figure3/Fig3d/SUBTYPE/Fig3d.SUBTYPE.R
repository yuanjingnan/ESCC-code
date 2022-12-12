


dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3d/SUBTYPE/"

########### fig3d  ###########


cli_anno <- read.table(file = paste(dir,"heatmap.cli.txt",sep = ""),header = T,sep = "\t" )
rownames(cli_anno) <- cli_anno$RNA_Tumor_Sample_Barcode
colnames(cli_anno) <- c("RNA_Tumor_Sample_Barcode","patients","MPR","TME","PD-L1 expression","CAF fraction","IFN/EMT score","Response")
cli_anno <- cli_anno %>% dplyr::select(`IFN/EMT score`,`CAF fraction`,`PD-L1 expression`,MPR,Response)

bk <- c(seq(-2,-0.1,by=0.01),seq(0,2,by=0.01))
color = c(colorRampPalette(colors = c("#155F83FF","white"))(length(bk)/2),colorRampPalette(colors = c("white","#800000FF"))(length(bk)/2))

sample_order <- c("ESCC-P13-T1","ESCC-P16-T1","ESCC-P06-T1","ESCC-P08-T1","ESCC-P09-T1",
                  "ESCC-P01-T1","ESCC-P05-T1","ESCC-P22-T1","ESCC-P03-T1","ESCC-P17-T1", "ESCC-P20-T1","ESCC-P07-T1",
                  "ESCC-P12-T1","ESCC-P04-T1","ESCC-P10-T1","ESCC-P21-T1")

sample_order_label <- c("P13","P16","P06","P08", "P09","P01","P05","P22",
                        "P03","P17","P11","P20","P07","P12","P04","P10","P21") 

gesa_list_0.05_select <- c("HALLMARK_ALLOGRAFT_REJECTION","HALLMARK_INTERFERON_ALPHA_RESPONSE",
                           "HALLMARK_INTERFERON_GAMMA_RESPONSE","HALLMARK_E2F_TARGETS",
                           "HALLMARK_MYC_TARGETS_V1","HALLMARK_DNA_REPAIR","HALLMARK_MTORC1_SIGNALING",
                           "HALLMARK_HYPOXIA","HALLMARK_MYOGENESIS","HALLMARK_TGF_BETA_SIGNALING",
                           "HALLMARK_ANGIOGENESIS","HALLMARK_EPITHELIAL_MESENCHYMAL_TRANSITION",
                           "HALLMARK_KRAS_SIGNALING_UP")

a1 <- read.table(file = "/Users/yuanjingnan/ESCC_2020/analysis_final/Subtype/GSEA.hallamrk.txt",header = T,sep = "\t") 
HALLMARK_ID <- a1$pathway
names(HALLMARK_ID) <- a1$HALLMARK



load(file = paste(dir,"Hallmark_ESCC_data.rda",sep = ""),verbose = T)#Hallmark_ESCC_data
data <- Hallmark_ESCC_data$exp

P <- pheatmap::pheatmap(data[gesa_list_0.05_select,sample_order],
                        scale = "row",
                        labels_row= as.character(HALLMARK_ID[gesa_list_0.05_select]),
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
                          `PD-L1 expression`= c('YES'='#D49A9E','NO'='#E3E3E3'),
                          `CAF fraction`= c('YES'='#D49A9E','NO'='#E3E3E3'),
                          `IFN/EMT score`= c('YES'='#D49A9E','NO'='#E3E3E3')),
                        annotation_col = cli_anno)

pdf(paste(dir,"Subtype.pdf",sep = ""),width=6, height=3.5)
print(P)
dev.off()

########### fig3d  ###########