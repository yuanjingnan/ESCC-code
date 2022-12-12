
library(ggplot2)
dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3h/"

nba = read.table(file = paste(dir,"subtype.txt",sep = ""),header = T,sep = "\t")




pdf(paste(dir,"figure3h.pdf",sep = ""),width=7, height=5)
ggplot(nba) +
  aes(x = Response, fill = factor(Subtype_IE)) +
  geom_bar(position = "fill")+
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
  scale_fill_manual(breaks = c("IE","non_IE"), values=c("#216695", "#d68146"))
dev.off()
