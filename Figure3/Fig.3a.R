

library(dplyr)
library(tidyr)

dir <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3a/"
WES_summary_input <- read.table(file =paste(dir,"Fig3a.txt",sep = ""),header = T,sep = "\t")
colnames(WES_summary_input) <- c("PD-L1 expression","Residual tumor","Response")

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
