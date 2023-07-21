library(swimplot)
library(ggplot2)
library(gdata)
library(readxl)
library(ggpubr)
setwd("D:/BGI/ESCC/Figure/Figure1/event")
Arm <- read_excel("Fig1c.nationtrt.xlsx")
Arm <- as.data.frame(Arm)

AE <- read_excel("Fig1c.nationa.xlsx")
AE <- as.data.frame(AE)

arm_plot <- swimmer_plot(df=Arm,
                         id='id',
                         end='End_trt',
                         name_fill='Treatment',
                         id_order='Treatment',
                         col=1,alpha=0.75,width=0.5)
                         
arm_plot

AE_plot <- arm_plot + 
  swimmer_points(df_points=AE,
                 id='id',time='time',
                 name_shape = 'Event',
                 size=2,fill='white',col='black')
AE_plot
ggsave(AE_plot,filename = 'swimmer.pdf',device = 'pdf',width = 7,height = 10)

##=======================
box <- read.csv('Fig1c.adjuvant.csv')

df.summary <- box %>%
  group_by(Group) %>%
  summarise(
    sd = sd(End_trt, na.rm = TRUE),
    len = mean(End_trt)
  )

my_theme <- theme(panel.grid=element_blank(),
                  legend.position = "none",
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 7),
                  axis.title = element_text(size=7),
                  panel.background = element_rect(fill = "transparent",colour = NA),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  plot.margin = unit(c(0,0,0,0),"cm"))

pb <- ggplot(box,aes(x=Group,y=End_trt,fill=Group))+geom_boxplot(color='black',width=0.5)+
  scale_fill_manual(values =  c("#00AFBB", "#E7B800"))+
  theme_bw()+my_theme+
  stat_compare_means(size=5,aes(label = paste0("P = ", ..p.format..)))
ggsave(pb,filename = 'box1.pdf',device = 'pdf',width = 5,height = 5)
