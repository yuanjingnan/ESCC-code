setwd('D:/BGI/ESCC/Figure/IF')
library(ggplot2)
library(cowplot)
library(ggsci)
library(dplyr)
#set theme
my_theme <- theme(panel.grid=element_blank(),
                  legend.position = "none",
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title = element_text(size=18),
                  panel.background = element_rect(fill = "transparent",colour = NA),
                  plot.background = element_rect(fill = "transparent",colour = NA))

ris <- read.csv('clinical.csv') %>% select(sample,response)
all.data <- read.csv('Fig.4b.all_ecotyper_raw_data.M12.csv')
num <- table(all.data$sample)
all.data <- all.data[all.data$sample %in% names(num[num==2]),]
all.data <- merge(all.data,ris)
data <- all.data

#line

select.cell <- c('CD4.T.cells','CD8.T.cells','B.cells','Macrophages.M1','Macrophages.M2','Fibroblasts')

plot.line <- data.frame(matrix(nrow=0,ncol=4))
colnames(plot.line) <- c('T1','T3','Group','Cell')
for (res in unique(ris$response)){
  data1 <- dplyr::filter(data,response==res)
  t1.m <- data.frame(apply(data1[data1$Time=='T1',select.cell],2,median))
  colnames(t1.m) <- 'T1'
  t3.m <- data.frame(apply(data1[data1$Time=='T3',select.cell],2,median))
  colnames(t3.m) <- 'T3'
  tmp <- cbind(t1.m,t3.m)
  tmp$Group <- res
  tmp$cell <- row.names(tmp)
  plot.line <- rbind(plot.line,tmp)
}



for (i in seq(unique(plot.line$cell))){
  name <- unique(plot.line$cell)[i]
  p <- ggplot(plot.line[plot.line$cell==name,],aes(color='Group',fill='Group'))+
    geom_point(aes(1,y=T1*100,color='Group',fill='Group'),size=2.5)+
    geom_point(aes(2,y=T3*100,color='Group',fill='Group'),size=2.5)+
    geom_segment(aes(x=1,y=T1*100,xend=2,yend=T3*100,color='Group'))+
    theme_bw()+
    my_theme+
    scale_x_continuous(limits = c(0.75,2.25),breaks = c(1,2),labels=c('Pre','Post'))+
    scale_fill_manual(values = c("Well responders"='#3C5488',"Poor responders"='#E64B35'))+
    labs(title =paste0(name,"(%)"),x="",y="")
  
  assign(paste0('p',i),p) 
}  
p <- plot_grid(p1,p2,p3,p4,p5,p6,nrow=2)

ggsave(p,filename = 'Fig.4b.CIBER.plot.well.poor.pdf',width=12,height = 5)
