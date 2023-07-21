library(dplyr)
library(ggplot2)
library(ggstatsplot)

tcr_T3 <- read.csv('Fig.5a.csv')
p.rsd <- ggstatsplot::ggscatterstats(
  data = tcr_T3,
  x = rsd,
  y = richness,
  xlab = "Tumor residual",
  ylab = "Richness",
  title = "",marginal = F,
  point.args = list(show.legend = F,size=3,aes(colour =Response,shape=Response)),
  method="lm",ggtheme=theme_bw()+theme(panel.grid = element_blank())
)+scale_color_manual(values = c('WELL'="#3C5488FF",'POOR'="#E64B35FF"))

ggsave(p.rsd,filename = 'Corr.RSD.Richness.pdf',height = 5,width = 5)
