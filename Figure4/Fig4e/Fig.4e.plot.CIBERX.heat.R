library(dplyr)
library(reshape2)
library(ggplot2)
library(circlize)
library(ComplexHeatmap)
library(cowplot)
sub.cell <- c("B.cells.naive","Macrophages.M1","Macrophages.M2","T.cells.CD4","T.cells.CD8")
sub.sample <- c('P13','P03')

file <-'Fig.4e.CIBERX.ESCC.txt'
select.cell <- sub.cell
select.sample <- sub.sample


data <- read.table(file,header=T,sep='\t') %>%
  select(Mixture,all_of(select.cell)) %>%
  mutate(Time=case_when(grepl('T1',Mixture)~'T1',grepl('T3',Mixture)~'T3')) 

data$Mixture <- gsub("ESCC-(P[0-9][0-9]).*",'\\1',data$Mixture)

data <- filter(data,Mixture %in% select.sample)

a <- table(data$Mixture)
data <- data[data$Mixture %in% names(a[a==2]),]

#plot <- filter(data,Mixture %in% c('P16','P03','P13','P22') ) 
ris <- read.csv('clinical.csv') %>% 
  rename("Mixture"="sample")

plot <- data
plot <- merge(plot,ris[,c('Mixture','residual.percentage','response')]) %>%
                arrange(residual.percentage)
#write.csv(plot,row.names = F,quote=F,'CIBERX.input.csv')


plot1 <- data.frame(Cell=select.cell)  


for (i in unique(plot$Mixture)){
  tmp <- filter(plot,Mixture==i) %>% t() %>% as.data.frame()
  tmp$Cell <- row.names(tmp)
  colnames(tmp)[which(tmp['Time',]=='T1')] <- paste0(i,'.pre')
  colnames(tmp)[which(tmp['Time',]=='T3')] <- paste0(i,'.post')
  tmp <- tmp[-1,]
  tmp <- tmp[-nrow(tmp),]
  tmp[,1] <- as.numeric(tmp[,1])
  tmp[,2] <- as.numeric(tmp[,2])
  
  tmp[,paste0(i,'.FC')] <- log10(tmp[,grepl('post',colnames(tmp))]/tmp[,grepl('pre',colnames(tmp))])
  plot1 <- merge(plot1,tmp,by='Cell')
  
  }

row.names(plot1) <- plot1$Cell
#heat <- plot1[,c("P13.pre","P16.pre","P03.pre","P22.pre","P13.post","P16.post","P03.post","P22.post")]
heat <- plot1[,grep('FC',colnames(plot1),invert = T)]
heat <- cbind(plot1[,grep('pre',colnames(plot1))],plot1[,grep('post',colnames(plot1))])
heat$Cell <-NULL
heat <- heat*100
colnames(heat) <- gsub('.pre','',colnames(heat))


col_fun1 <- colorRamp2(c(0,5,10,15),c('#abd9e9','#ffffbf','#fdae61','#d73027'))
p <- 
  Heatmap(as.matrix(heat),show_column_names=TRUE,cluster_rows = FALSE,
          rect_gp = gpar(col = "black", lwd = 0.3),
          show_row_names = TRUE,
          cluster_columns = FALSE,show_column_dend = FALSE,
          show_row_dend = FALSE,column_names_rot = 90,
          row_names_gp = gpar(fontsize = 6.5, fontface = "bold"),
          name = "RNA-Seq cell proportion(%)",heatmap_legend_param = list(direction = "horizontal"),
          col=col_fun1,
          row_names_side = 'left',
          show_heatmap_legend = T,heatmap_height = unit(1*nrow(heat),"cm"),heatmap_width = unit(1.5*ncol(heat),"cm"))

pdf('Fig.4e.CIBERX.CellFraction.2Sample.heat.pdf')
draw(p,heatmap_legend_side = "bottom")
dev.off()



