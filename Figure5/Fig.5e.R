library(customLayout)
library(gridExtra)
library(cowplot)
library(ggsci)
library(dplyr)
library(ggplot2)
setwd('D:/BGI/ESCC/Figure/TCR/Figure6/Fig.D')
data.orig <- read.csv('Fig.5e.input') # %>% filter(! cname %in% c('CCX','FDY','ZAL','ZHS','ZY'))


#plot
my_theme <- theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank(),
                  legend.position = 'none',
                  axis.text.x = element_text(size = 15,colour = 'black'),
                  axis.text.y = element_text(size = 12,colour = 'black'),
                  axis.title = element_text(size=16))
my_color <- scale_color_manual(values = c('T1only'='#916064','T1T3'='#d7c393','T3only'='#78a1b1'))
scaleFUN <- function(x) sprintf("%.1f", x)
my_geom_errorbar <- geom_errorbar(aes(ymin=Mean-SEM,ymax=Mean+SEM),
                                  size=0.3,width=0,
                                  position=position_dodge(0.1),
                                  alpha=1)

my_geom_line <- geom_line(size=1,position=position_dodge(0.1))
my_geom_point <- geom_point(position=position_dodge(0.1),size=4,shape=21,fill='white')
#ALL sample
data <- na.omit(data.orig)
mean <- apply(data[,2:19],2,function(x){mean(x)})                                      
sem <- apply(data[,2:19],2,function(x){sd(x)/sqrt(length(x))})  
plot <- data.frame(Type=colnames(data[,2:19]),Mean=mean,SEM=sem) %>% 
  mutate(Time=case_when(grepl("WA",Type)~1,
                        grepl("WB",Type)~2,
                        grepl("WC",Type)~3),
         Group=case_when(grepl("T1only",Type)~"T1only",
                        grepl("T1T3",Type)~"T1T3",
                        grepl("T3only",Type)~"T3only")
  )

p.legend <- ggplot(plot[!grepl('space',plot$Type),],aes(x=Time,y=Mean,color=Group,shape=Group))+
  my_geom_line+
  #my_geom_errorbar+
  my_geom_point+
  labs(y='Percent change of clonetype',x="")+
  theme_bw()+
  my_color+
  scale_x_continuous(breaks = c(1,2,3),labels=c('Baseline','W3','W6'))+
  scale_y_continuous(labels=scaleFUN)+
  theme(legend.position = 'top',plot.margin = unit(c(0,0,0,0),"cm"))
ggsave(p.legend,filename = 'PercentChange.legend.pdf')

p.all.Clonetype  
  ggplot(plot[!grepl('space',plot$Type),],aes(x=Time,y=Mean,color=Group))+ #,shape=Group
  my_geom_line+
  my_geom_errorbar+
  my_geom_point+
  labs(y='Percent change of clonetype',x="")+
  theme_bw()+
  my_color+
  scale_x_continuous(breaks = c(1,2,3),labels=c('Baseline','W3','W6'))+
  scale_y_continuous(labels=scaleFUN)+
  my_theme+
  geom_hline(yintercept =0,linetype = 'dashed',color='grey')


p.all.ClonalSpace <- ggplot(plot[grepl('space',plot$Type),],aes(x=Time,y=Mean,color=Group,shape=Group))+
  my_geom_line+
 # my_geom_errorbar+
  my_geom_point+
  labs(y='Percent change of clonal space',x="")+
  theme_bw()+
  my_color+
  scale_x_continuous(breaks = c(1,2,3),labels=c('Baseline','W3','W6'))+
  scale_y_continuous(labels=scaleFUN)+
  my_theme+
  geom_hline(yintercept =0,linetype = 'dashed',color='grey')


#well poor responders
data.orig <- na.omit(data.orig)

for (i in c(unique(data.orig$clin4),unique(data.orig$clin4))){
  data <- filter(data.orig,clin4==i|cluster.tme==i)
  mean <- apply(data[,2:19],2,function(x){mean(x)})                                      
  sem <- apply(data[,2:19],2,function(x){sd(x)/sqrt(length(x))})
  plot <- data.frame(Type=colnames(data[,2:19]),Mean=mean,SEM=sem) %>% 
    mutate(Time=case_when(grepl("WA",Type)~1,
                          grepl("WB",Type)~2,
                          grepl("WC",Type)~3),
           Group=case_when(grepl("T1only",Type)~"T1only",
                           grepl("T1T3",Type)~"T1T3",
                           grepl("T3only",Type)~"T3only")
    )
  

  p <- ggplot(plot[!grepl('space',plot$Type),],aes(x=Time,y=Mean,color=Group))+
    my_geom_line+
    #my_geom_errorbar+
    my_geom_point+
    labs(y=i,x='',title="")+
    theme_bw()+
    my_color+
    scale_x_continuous(breaks = c(1,2,3),labels=c('Baseline','W3','W6'))+
    scale_y_continuous(labels=scaleFUN)+
    my_theme+
    theme(plot.margin = unit(c(0,1,0,1),"cm"))+
    geom_hline(yintercept =0,linetype = 'dashed',color='grey')
  
  assign(paste0('p.',i,".Clonetype"),p)
  
  p <- ggplot(plot[grepl('space',plot$Type),],aes(x=Time,y=Mean,color=Group,shape=Group))+
    my_geom_line+
   # my_geom_errorbar+
    my_geom_point+
    labs(y=i,x='',title="")+
    scale_x_continuous(breaks = c(1,2,3),labels=c('Baseline','W3','W6'))+
    scale_y_continuous(labels=scaleFUN)+
    theme_bw()+
    my_color+
    my_theme+
    theme(plot.margin = unit(c(0,1,0,1),"cm"))+
    geom_hline(yintercept =0,linetype = 'dashed',color='grey')
  assign(paste0('p.',i,".ClonalSpace"),p)
  
}




p <- plot_grid(plot_grid(p.all.Clonetype,p.all.ClonalSpace,ncol = 1),plot_grid(p.Well.Clonetype,p.Poor.Clonetype,p.Well.ClonalSpace,p.Poor.ClonalSpace,ncol=1),ncol=2)
print(p)
