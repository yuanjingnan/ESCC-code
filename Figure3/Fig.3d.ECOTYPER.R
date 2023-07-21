library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
dir <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3d/ECOTYPER/"

cf_list_eco_without_Epi<- c("Fibroblasts","Endothelial.cells","PMNs","Monocytes.and.Macrophages","Mast.cells","Dendritic.cells",
                            "NK.cells","B.cells","PCs",
                            "CD4.T.cells","CD8.T.cells")

sample_order <- c("ESCC-P13-T1","ESCC-P16-T1","ESCC-P06-T1","ESCC-P08-T1","ESCC-P09-T1","ESCC-P11-T1","ESCC-P01-T1",
                  "ESCC-P05-T1","ESCC-P22-T1","ESCC-P03-T1","ESCC-P17-T1","ESCC-P20-T1","ESCC-P07-T1",
                  "ESCC-P12-T1","ESCC-P04-T1","ESCC-P10-T1","ESCC-P21-T1")

P_order <- c("P13","P16","P06","P08","P09","P11","P01","P05","P22","P03","P17","P11","P20","P07","P12","P04", "P10","P21") 

cell_type_col <- c("#ec671a","#f28f1b","#b4b3b3","#97cb7d","#4ca735","#3a8e39",
                            "#554f9f","#be79b0","#8f71b0","#3797d4","#08518b")
                            
write.table(data_all,file = paste(dir,"Fig3d3.txt",sep = ""),sep = "\t" )
data_all <- read.table(file = paste(dir,"Fig3d3.txt",sep = ""),header = T,sep = "\t" )


P <- ggplot(data_all,aes(x = P,Cell_type,fill=Cell_type_ID))+
  geom_bar(stat="identity",position="fill",width=0.8,colour="white",size=0)+
  labs(title = title,x="",y="Proportion (%)")+
  scale_fill_manual(name="Cell type",breaks=cf_list_eco_without_Epi,
                    labels=cf_list_eco_without_Epi,values=cell_type_col)+
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


