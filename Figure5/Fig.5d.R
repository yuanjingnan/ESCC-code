setwd("D:/BGI/ESCC/Figure/TCR/XYS")
library(ggpubr)
data <- read.csv('Fig.5d.input.csv')
data$Group <- 'CloneSpace'
data[grep("T1T3$",data$tag),'Group'] <- 'CloneType'
data$tag <- gsub('_space','',data$tag)
box.width <- 0.3
info <- read.csv('D:/BGI/ESCC/Figure/TCR/Figure6/Group.info.csv')
data <- merge(data,info[,c('cname','cluster.tme')],all.x = T,by='cname')
#all
data$clin4 <- factor(data$clin4,levels=c('WELL','POOR'))

  ##==function==##
  plot.mean <- function(x) {
    m <- mean(x)
    c(y = m, ymin = m, ymax = m)
  }
  

p_dot <- 
  ggdotplot(data,x="Group", y="Percent",facet.by = 'clin4',add='mean_sd',position=position_dodge(0.9),add.params = list(group='tag',color='black',width=0.5,size=0.3),color='tag',binwidth = 0.05,fill=NA,size = 0.6,error.plot = "errorbar")+
  stat_summary(fun.data=plot.mean,geom="errorbar",aes(group=tag), position=position_dodge(0.9),color="black", size=0.5)+
  scale_fill_manual(values = c('T1_inT1T3'='#DF8F44FF','T3_inT1T3'='#79AF97FF'))+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size=10),
        plot.margin = unit(c(0,0,0,0),"mm"))+
  stat_compare_means(size=3,aes(group=tag,label = paste0("P = ", ..p.format..)))
  
ggsave(p_dot,filename = 'Fig4D.pdf',device = 'pdf',units = 'mm',width = 60,height = 70)


