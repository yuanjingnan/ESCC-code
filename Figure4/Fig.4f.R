library(estimate)


estimate_data_select <- function(w.T1,j) {
    col <- c("Response","RNA_Tumor_Sample_Barcode","Type")
    w.T1.1<- w.T1 %>% dplyr::select(col,j) 
    colnames(w.T1.1) <- c(col,"cell_type")
    w.T1.1
  }
estimate_data_plot <- function(data,j,list_com) {
    data1 <- data %>% dplyr::select(Response,Type,cell_type)
    data1$Type <- factor(data1$Type, levels = c("Pre-Neo","Post-Neo"))
    data1$Response <- factor(data1$Response, levels = c("Well responders","Poor responders"))
    
    
    data2 <- data1 %>% mutate(single_type=case_when(
      Response=="Well responders"& Type=="Pre-Neo" ~ "Well_Pre",
      Response=="Poor responders"& Type=="Pre-Neo" ~ "Poor_Pre",
      Response=="Well responders"& Type=="Post-Neo" ~ "Well_Post",
      Response=="Poor responders"& Type=="Post-Neo" ~ "Poor_Post"
    ))
    list <- list_com
    list <- combn(unique(data2$single_type), 2, simplify = F)
    data2$single_type <- factor(data2$single_type, levels = c("Well_Pre","Poor_Pre","Well_Post","Poor_Post"))
    x_labe <- c("        Pre-NAIT","","        Post-NAIT","")
    title <- j
    P1<-ggplot(data2, aes(single_type, as.numeric(cell_type),color=Response))+
      geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
      geom_point(aes(color=Response), size=2, position = position_jitter(w=0.05,h= 0))+
      geom_signif(comparisons = list,step_increase = 0.1,map_signif_level=F,test="wilcox.test",color='black',textsize = 5,show.legend = T)+
      #scale_y_continuous(limits=range)+
      labs(title=j,x="",y="",size=25)+
      theme_bw()+
      theme(plot.title=element_text(size=30, hjust=0,vjust=0.5),
            axis.title = element_text(size=20),
            axis.text.y = element_text(size=20),
            axis.text.x=element_text(color = "black", size=20),
            axis.ticks.x=element_blank(),
            strip.text = element_text(size = 20),
            legend.text=element_text(size=20),legend.title=element_text(size=20),
            
            panel.border = element_blank(),
            panel.grid.major=element_line(colour=NA),
            panel.grid=element_blank(),
            axis.line = element_line(size=1, colour = "black"))+
      scale_x_discrete(labels= x_labe)+ 
      scale_color_manual(breaks = c("Well responders","Poor responders"), values=c("#377EB8", "#E41A1C"))
    P1
  }

ESTIMATE_data <- read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4f/Fig4f.txt",sep = "\t",header=T)

list_com <- NULL

estimate.list <- c("StromalScore","ImmuneScore","ESTIMATEScore","TumorPurity")
pdf(paste("/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4f/Fig4f.pdf",sep = ""),width=8, height=8)
for (i in estimate.list) {
  P <- estimate_data_select(ESTIMATE_data,i) %>% estimate_data_plot(.,i,list_com)
  print(P)
}
dev.off()
