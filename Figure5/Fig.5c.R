 
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)
library(ComplexHeatmap)


pair.T <- read.csv('pair.t.csv')

p.pair.t <- ggpaired(pair.T,x="variable", y="value",color="black",fill='#868686',palette='jco',line.color = 'black')+ 
  geom_point(aes(color=response,fill=response))+
  stat_compare_means(size=5)+
  xlab("Tumor Tissue")+ylab("T Cell Fraction")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.grid=element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15))
ggsave(p.pair.t,filename = 'Tcell.fraction.T.boxpair.pdf',device = 'pdf',width = 3,height = 5)
  
pair.B <-  read.csv('pair.b.csv')
p.pair.b <- ggpaired(pair.B,x="variable", y="value",color="black",fill='#868686',palette='jco',line.color = 'black')+ 
  geom_point(aes(color=response,fill=response))+
  stat_compare_means(size=5)+
  xlab("Tumor Tissue")+ylab("T Cell Fraction")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.grid=element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15))
ggsave(p.pair.b,filename = 'Tcell.fraction.B.boxpair.pdf',device = 'pdf',width = 3,height = 5)

