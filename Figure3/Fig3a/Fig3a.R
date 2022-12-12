

library(dplyr)
library(tidyr)

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3a/"
load(file =paste(dir,"list_ESCC.rda",sep = ""),verbose = T)
list_ESCC_Pre <- list_ESCC %>% dplyr::filter(samples_id=="T1")


WES_summary <- read.table(file =paste(dir,"WES.summary.txt",sep = ""),header = T,sep = "\t")%>% 
  left_join(list_ESCC_Pre[,c("RNA_Tumor_Sample_Barcode","Response")],by="RNA_Tumor_Sample_Barcode") %>%drop_na()


#tmb.nonsys PD.L1.expression

#PD-L1 expression_residual tumor_.pdf

WES_summary_input <- WES_summary[,c("tmb.nonsys","neo.num","PD.L1.expression","residual_tumor","Response")]
colnames(WES_summary_input) <- c("TMB","TNB","PD-L1 expression","Residual tumor","Response")

P1 <- ggstatsplot::ggscatterstats(
  data = WES_summary_input,marginal=F,
  y = `PD-L1 expression`,bf.message=F,results.subtitle = T,
  x = `Residual tumor`,
  ylab = "PD-L1 expression",
  xlab = "Residual tumor",
  title = paste("A correlation between PD-L1 expression and Residual tumor"),
  point.args = list(show.legend = T,aes(colour =Response)),
  marginal.type = "histogram",
  method="lm",ggtheme=theme_gray())+ 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+ 
  scale_color_manual(breaks = c("Well responders","Poor responders"), values=c("#377EB8", "#E41A1C"))



pdf(paste(dir,"PDL1_Residual_tumor.pdf",sep = ""),width=4.5, height=3)
print(P1)
dev.off()
