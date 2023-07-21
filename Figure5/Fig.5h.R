setwd('D:/BGI/ESCC/Figure/TCR/Neo/')
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggridges)
library(ggpubr)
library(scales)
library(reshape2)

stat <- read.csv('TCR_Neo_stat3.9_11.csv')
stat <- stat %>% mutate(clin4=case_when(clin4=='WELL'~'Well responders',clin4=='POOR'~'Poor responders'))
my_theme <- theme(panel.grid=element_blank(),
                  legend.position = "top",
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size=13),
                  panel.background = element_rect(fill = "transparent",colour = NA),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  plot.margin = unit(c(1,1,1,1),"cm"))





##
###Spe ITC-spe neo  VS rds
splot <- select(stat,Spe_Neo,Spe_ITC,rsd)
b <- melt(splot,id.vars = 'rsd')
p1 <- cor.test(stat$Spe_ITC,stat$Spe_Neo)$p.value
r1 <- cor.test(stat$Spe_ITC,stat$Spe_Neo)$estimate
pdf('Spe_ITC.spe_Neo.smooth.rsd.point.pdf',7,5)
p <- ggplot(b,aes(rsd,value,color=variable,fill=variable))+
    geom_smooth(se=TRUE,method = 'lm')+
    geom_point(size=3,shape=1,stroke = 1.5)+
    theme_bw()+my_theme+
    scale_color_manual(values = c('Spe_Neo'="#FFBA24",'Spe_ITC'="#5A89E0"))+
    scale_fill_manual(values = c('Spe_Neo'="#FFBA24",'Spe_ITC'="#5A89E0"))+
    annotate("text",x=0.6,y=2500,label=paste0("P = ",round(p1,2)))+
    annotate("text",x=0.6,y=2200,label=paste0("R = ",round(r1,2)))+
    labs(x='Residual tumor (%)',y='Numbers')
print(p)
dev.off()

