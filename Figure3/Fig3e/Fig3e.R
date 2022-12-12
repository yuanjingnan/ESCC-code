
library(ggpubr)
library(ggplot2)
dir <-"/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3e/"

cli_anno <- read.table(file = paste(dir,"Subtype_cli.txt",sep = ""),header = T,sep = "\t" )

nba = read.table(file = paste(dir,"CIBERSORTx_Results.txt",sep = ""),header = T,sep = "\t")  %>% mutate(RNA_Tumor_Sample_Barcode=Mixture) %>% left_join(cli_anno,by="RNA_Tumor_Sample_Barcode")



pdf(paste(dir,"TILs.pdf",sep = ""),width=4, height=5)
P3 <- ggplot(nba, aes(Subtype_IE, as.numeric(TILs_relative),color=Subtype_IE))+
  geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
  geom_point(aes(color=Subtype_IE), size=2, position = position_jitter(w=0.05,h= 0))+
  geom_signif(comparisons = list(c("IE","non-IE")),step_increase = 0.1,map_signif_level=F,
              test="wilcox.test",color='black',textsize = 5,show.legend = T)+
  labs(title="",x="",y="TILs",size=25)+
  theme_bw()+
  theme(plot.title=element_text(size=30, hjust=0,vjust=0.5),
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.text.x=element_text(color = "black", size=20),
        axis.ticks.x=element_blank(),
        strip.text = element_text(size = 20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        
        panel.border = element_blank(),
        panel.grid.major=element_line(colour=NA),
        panel.grid=element_blank(),
        axis.line = element_line(size=1, colour = "black"))+
  scale_color_manual(breaks = c("IE","non-IE"), values=c("#1f6b9d", "#df8447"))

print(P3)


dev.off()









