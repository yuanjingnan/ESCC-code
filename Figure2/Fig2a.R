setwd("D:/BGI/ESCC/Figure/Figure2/")
library(ggpubr)
library(reshape2)
library(dplyr)
library(patchwork)
library(RColorBrewer)
cli <- read.csv('Fig.2a.clinical.input.csv',check.names = F)
cli[is.na(cli$PD_L1),'PD_L1'] <- 'Unknown'
#cols <- c('I'='#79c77a','IIa'='#80cebc','IIb'='#3fb7c5','II'='#1792c1','IIIa'='#fe9a24','IIIb'='#ed710c','IVa'='#e41316','SD'='#b79833','PD'='#7a6720','PR'='#fdf6d1','0'='#fff5ec','1'='#fce57c','2'='#faa853','4'='#e75d32','5'='#cc263b','6'='#8d002d','male'='#68c5a5','female'='#fb9364','never'='#30982a','former'='#d61018','No infiltration'='#d3dfef','Muscularis mucosae'='#9db3d9','Submucosa'='#44bfb6','Muscularis propria'='#3c63b6','Adventitia'='#192544','Full thickness'='#e64241')
cols <- c("IIa"= '#80cec1','IIb'='#369790','IIIb'='#01665e','IVa'='#003c30','SD'='#f5e387','PD'='#9b321f','PR'='#5b803c','No'='#c4acaf','Yes'='#6d3b6f','No infiltration'='#E2DC9F','Muscularis mucosae'='#ffe090','Submucosa'='#fdae63','Muscularis propria'='#f56d43','Adventitia'='#d73027','Full thickness'='#a50126')

if (max(cli$residual)<1){cli$residual <- cli$residual*100}
cli$reduce <- 100-cli$residual
cli$sample <- gsub('ESCC-','',cli$sample)
cli <- cli[order(cli$reduce,decreasing = F),]
cli$sample <- factor(cli$sample,levels = cli$sample)
cli$PD_L1 <- factor(cli$PD_L1,levels=c('>10',"<10","Unknown"))
plot.ris <- select(cli,sample,reduce,PD_L1)

p_ris <- ggplot(plot.ris,aes(x=sample,y=reduce,color=PD_L1,fill=PD_L1))+
  geom_bar(width = 0.6,stat='identity')+
  scale_x_discrete(position = "top")+
  scale_y_reverse()+
  theme_classic()+
  theme(axis.text.x.top = element_text(angle = 45, vjust=0, hjust=0))+
  xlab("")+
  ylab('Rresidual tumor (%)')+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        plot.margin=unit(c(2,2,2,2),"cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22))+
  geom_hline(yintercept=90,linetype="dashed",color="black")+
  scale_color_manual(values = c("<10"='#533e80','>10'='#98c069','Unknown'='#b4b5b4'),
                     label=c("<10"="PD-L1—",">10"="PD-L1+","Unknown"="Unknown"))+
  scale_fill_manual(values = c("<10"='#533e80','>10'='#98c069','Unknown'='#b4b5b4'),
                    label=c("<10"="PD-L1—",">10"="PD-L1+","Unknown"="Unknown"))



heat <- melt(cli[,c('sample','Tumor infiltration','Vessel carcinoma embolus','LN metastasis.status','Clinical disease stage','Alcohol/Smoking status','RECIST')],id='sample')
heat$sample <- gsub('ESCC-','',heat$sample)
heat$sample <- factor(heat$sample,levels=cli$sample)
my_level <- c('Tumor infiltration','Vessel carcinoma embolus','LN metastasis.status','Clinical disease stage','RECIST','Alcohol/Smoking status')

heat$variable <- factor(heat$variable,levels = my_level[length(my_level):1] )

p_bar <- ggplot(heat,aes(x=sample,y=variable))+
  geom_tile(aes(fill=value),color='black',size=1)+
  scale_x_discrete("",expand = c(0,0))+
  scale_y_discrete("",expand = c(0,0))+
  theme(#axis.text.x = element_blank(),
        axis.text.y=element_text(size=18),
        axis.ticks = element_blank(),
        legend.position = 'none',
        plot.margin=unit(c(2,2,2,2),"cm"))+
  scale_fill_manual(values = cols)

ggplot(plot.ris,aes(x=sample,y=reduce,color=PD_L1,fill=PD_L1))+geom_bar(width = 0.6,stat='identity')+scale_x_discrete(position = "top")+scale_y_reverse()+theme_classic()+theme(axis.text.x.top = element_text(angle = 45, vjust=0, hjust=0))+xlab("")+ylab('Rresidual tumor (%)')+theme(legend.position = "right",axis.text.x = element_text(size = 15),plot.margin=unit(c(2,2,2,2),"cm"),axis.text.y = element_text(size = 15),axis.title.y = element_text(size = 20))+geom_hline(yintercept=90,linetype="dashed",color="grey")+scale_color_manual(values = c("<10"='#533e80','>10'='#98c069','Unknown'='#b4b5b4'),label=c("<10"="PD-L1—",">10"="PD-L1+","Unknown"="Unknown"))+scale_fill_manual(values = c("<10"='#533e80','>10'='#98c069','Unknown'='#b4b5b4'),label=c("<10"="PD-L1—",">10"="PD-L1+","Unknown"="Unknown"))
ggsave("residual.legend.bar.pdf",device = "pdf",units = "cm",height = 20,width = 10)
ggplot(heat,aes(x=sample,y=variable))+geom_tile(aes(fill=value),color='black',size=0.5)+scale_x_discrete("",expand = c(0,0))+scale_y_discrete("",expand = c(0,0))+theme(axis.text.x = element_blank(),axis.text.y=element_text(size=10),axis.ticks = element_blank(),legend.position = 'right',plot.margin=unit(c(2,2,2,2),"cm"))+scale_fill_manual(values = cols)
ggsave("topbar.legend.pdf",device = "pdf",units = "cm",height = 20,width = 10)

p_bar+p_ris+plot_layout(widths=c(1,1),heights = c(1,2.5),ncol=1)
ggsave("combine.pdf",device = "pdf",width = 30,height = 30,units = "cm")
