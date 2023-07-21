plot_middle <- read.csv('plot_middle.csv')
plot_itc <- read.csv('plot_itc.csv')
plot_neo <- read.csv('plot_neo.csv')
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(reshape2)
library(tidyr)
library(cowplot)

my_theme <- theme(panel.grid=element_blank(),
  legend.position = "none",
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  axis.title = element_text(size=15),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA))


p <- ggplot(plot_middle,
       aes(x = x, stratum = stratum, alluvium = alluvium,
           fill = stratum, label = stratum)) +
		   theme_bw()+my_theme+
  scale_fill_manual(values=c("expansion"="#6dc5a9","contraction"="#fc936a","T1only"="#93a5ce","Overlap"="#e890c6","T3only"="#f9d22c")) +
  geom_flow(width=1/8) +
  geom_stratum(alpha = .9,width=1/4) +
  #geom_text(stat = "stratum", size = 3)+
  labs(x=i,y="")+
  scale_y_continuous(limit=c(0,nmax))



####===============================================


g1 <- ggplot(data = plot_itc,
             aes(x = Group, y = Freq, alluvium = spe_group)) +
  theme_bw() +my_theme+
  scale_fill_manual(values=c("spe_contraction"="#fc936a","nospe_contraction"="#fc936a1a","spe_expansion"="#6dc5a9","nospe_expasion"="#6dc5a91a")) +
  geom_alluvium(aes(fill = spe_group),
                alpha = .75, 
                width = 1/4) +
  geom_stratum(aes(stratum = spe_group,fill = spe_group), 
               width = 1/4) +
  scale_y_continuous(limit=c(0,nmax))+
  annotate("text",x=1,y=num_max,label=num_max,hjust = 0.5, vjust = -1)+
  annotate("text",x=2,y=num_max1,label=num_max1,hjust = 0.5, vjust = -1)
			   


g2 <- ggplot(data = plot_neo,
             aes(x = Group, y = Freq, alluvium = neo_group)) +
  theme_bw() +my_theme+
  scale_fill_manual(values=c("Neo_T1only"="#93a5ce","noNeo_T1only"="#93a5ce1a","Neo_Overlap"="#e890c6","noNeo_Overlap"="#e890c61a","Neo_T3only"="#f9d22c","noNeo_T3only"="#f9d22c1a")) +
  geom_alluvium(aes(fill = neo_group),
                alpha = .75, 
                width = 1/4) +
  geom_stratum(aes(stratum = neo_group,fill = neo_group), 
               width = 1/4)+
  scale_y_continuous(limit=c(0,nmax))+
  annotate("text",x=1,y=num_max1,label=num_max1,hjust = 0.5, vjust = -1)+	   
  annotate("text",x=2,y=num_max*20,label=num_max,hjust = 0.5, vjust = -1)

			 
gg <- plot_grid(g1,p,g2,nrow=1)	   

ggg <- plot_grid(plotlist=plist,ncol=1)
pdf(paste0("Sankey.pdf"),15,15)
print(ggg)
dev.off()
