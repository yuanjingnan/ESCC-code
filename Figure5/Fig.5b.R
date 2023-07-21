setwd('F:/BGI/ESCC/Figure/TCR/Figure6/Fig.A')

library(ggplot2)
library(dplyr)
library(ggpubr)

data.a <- read.csv('Fig.5b.input.csv') %>% filter(cluster.tme != "")
data.a <- data.a[data.a$cluster.tme !='']
#data.a <- data.a[data.a$id %in% a$Var1,]
item <- c("clonality","richness")
#tumor
data.a.1 <- data.a[data.a$origin == 'Tissue',]
a  <- data.frame(table(data.a.1$id)) %>% filter(Freq==3) %>% select(Var1)
data.a.1 <- data.a.1[data.a.1$id %in% a$Var1,]
data.a.1$type <- factor(data.a.1$type,levels=c('T1',"T3","NP"))
my_compare <- list(c('T1','T3'),c("T1","NP"),c("T3","NP"))

p <-ggviolin(data.a.1,x="type", y=i,width =1,,fill="type",color="black",
             add= "boxplot",
             add.params = list(fill = "cluster.tme",group="cluster.tme",width=0.2,size=0.5),
             palette = 'jco')+
    stat_compare_means(aes(group=cluster.tme),label='p.format',comparisons = my_compare1)+stat_compare_means(comparisons = my_compare,label = 'p.format')+
    #scale_fill_manual(values = c('Well responders'='#3a72b7','Poor responders'='#e2221e'))+
    xlab("")+ylab(i)+
    theme(plot.margin = unit(c(1,1,1,1),"lines"),
          panel.grid=element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size=18),
    )

  p1 <- ggboxplot(data.a.1,x="cluster.tme", y=i,facet.by = 'type',width =1,fill="cluster.tme",color="black",palette = 'jco')+stat_compare_means(comparisons = my_compare1)
  ggsave(p1,filename = paste0('Fig.A.cluster.facet.richness.pdf'),device = 'pdf',width =6,height = 5)
  

