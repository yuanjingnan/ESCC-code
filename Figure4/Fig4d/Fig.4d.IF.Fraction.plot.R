setwd('D:/BGI/ESCC/Figure/IF')
library(dplyr)
library(reshape2)
library(ggplot2)
library(circlize)
library(ComplexHeatmap)

heat <- read.csv('Fig.4d.IF.fraction.csv',row.names = 1)
heat <- heat[c('CD20','CD8','CD4','CD68','CD163'),c("P03.pre","P03.post","P13.pre","P13.post")]*100

p <- 
  Heatmap(heat,show_column_names=TRUE,cluster_rows = FALSE,
        rect_gp = gpar(col = "black", lwd = 1),
        show_row_names = TRUE,
        cluster_columns = FALSE,show_column_dend = FALSE,
        show_row_dend = FALSE,
        row_names_gp = gpar(fontsize = 10, fontface = "bold"),
        name = "IF Positive cell proportion(%)",
        col=colorRamp2(c(0,3,6,9),c('#abd9e9','#ffffbf','#fdae61','#d73027')),
        show_heatmap_legend = T,heatmap_height = unit(8,"cm"),heatmap_width = unit(8,"cm"))

pdf('Fig.4d.IF.Fraction.heat.pdf',height =7,width = 5)
print(p)
dev.off()


