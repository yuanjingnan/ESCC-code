library(ggplot2)
dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3d/ECOTYPER/"
Major_Immune_cell_data <- function(cf_exp,cli,cf_list_major) {
  a1 <- read.table(file = cf_exp,header = T,sep = "\t") 
  rownames(a1) <- a1$RNA_Tumor_Sample_Barcode
  a2 <- a1[cli$RNA_Tumor_Sample_Barcode,] %>% left_join(cli,by="RNA_Tumor_Sample_Barcode") 
  a3 <- a2
  data_all <- data.frame(Response=character(),RNA_Tumor_Sample_Barcode=character(), 
                         Type=character(), Cell_type=as.numeric(character()), 
                         Cell_type_ID=character(),stringsAsFactors=FALSE) 
  for (v in cf_list_major) {
    col <- c("Response","RNA_Tumor_Sample_Barcode","Type",v)
    data1 <- a3 %>% dplyr::select(col) %>% mutate(Cell_type_ID=v)
    colnames(data1) <- c("Response","RNA_Tumor_Sample_Barcode","Type","Cell_type","Cell_type_ID")
    data_all <- rbind(data_all,data1)
  }
  data_all
}
load(paste(dir,"list_ESCC_Pre.rda",sep = ""),verbose = T)#list_ESCC_Pre
list_ESCC_Pre_1 <- list_ESCC_Pre %>% dplyr::filter(!patients %in% c("ESCC-P11","ESCC-P02"))

RNA_ECOTYPER <- paste(dir,"ESCC.ECOTYPER.txt",sep = "")
cf_list_eco_without_Epi<- c("Fibroblasts","Endothelial.cells","PMNs","Monocytes.and.Macrophages","Mast.cells","Dendritic.cells",
                            "NK.cells","B.cells","PCs",
                            "CD4.T.cells","CD8.T.cells")

sample_order <- c("ESCC-P13-T1","ESCC-P16-T1","ESCC-P06-T1","ESCC-P08-T1","ESCC-P09-T1","ESCC-P01-T1","ESCC-P05-T1","ESCC-P22-T1","ESCC-P03-T1","ESCC-P17-T1","ESCC-P20-T1","ESCC-P07-T1","ESCC-P12-T1","ESCC-P04-T1","ESCC-P10-T1","ESCC-P21-T1")

P_order <- c("P13","P16","P06","P08","P09","P01","P05","P22","P03","P17","P11","P20","P07","P12","P04", "P10","P21") 

data_all <- Major_Immune_cell_data(RNA_ECOTYPER,list_ESCC_Pre_1,cf_list_eco_without_Epi) 
title  <- "Pre-Neo"

data_all$RNA_Tumor_Sample_Barcode <- factor(data_all$RNA_Tumor_Sample_Barcode, levels = sample_order)
data_all$Cell_type_ID <- factor(data_all$Cell_type_ID, levels = cf_list_eco_without_Epi)
data_all <- data_all %>% arrange(factor(RNA_Tumor_Sample_Barcode, levels = sample_order))
data_all <- data_all %>% separate(RNA_Tumor_Sample_Barcode,into=c("ESCC","P","T1"),"-")
data_all$P <- factor(data_all$P, levels = P_order)
cell_type_col <- c("#ec671a","#f28f1b","#b4b3b3","#97cb7d","#4ca735","#3a8e39",
                            "#554f9f","#be79b0","#8f71b0","#3797d4","#08518b")
                            




P <- ggplot(data_all,aes(x = P,Cell_type,fill=Cell_type_ID))+
  geom_bar(stat="identity",position="fill",width=0.8,colour="white",size=0)+
  labs(title = title,x="",y="Proportion (%)")+
  scale_fill_manual(name="Cell type",breaks=cf_list_eco_without_Epi,labels=cf_list_eco_without_Epi,values=cell_type_col)+
  theme_bw()+
  theme(plot.title=element_text(size=30),axis.title.y = element_text(size=20),
        axis.text.x=element_text(size=20,angle = 90, vjust = 0.5, hjust=1),#normal
        axis.title.x = element_text(size=20),
        axis.text.y=element_text(size=20),legend.text = element_text(size=20),legend.title = element_text(size=20),
        axis.ticks.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(),panel.border = element_blank(),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'))


pdf(paste(dir,"Major_Immune_cell.pdf",sep = ""),width=10, height=5)
print(P)
dev.off()


