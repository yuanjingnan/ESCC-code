
signture_zscore_mean <- function(RNA_full_exp,cli,v_title,genelist,list_com) {
  signture_zscore_mean_data <- function(RNA_full_exp,cli,genelist,v_title) {
    signture_profile <- function(RNA_full_exp,cli,genelist) {
      read_expression <- function(RNA_exp,cli) {
        a0 <- read.table(file = RNA_exp,header = T,sep = "\t")#24533    68
        convert_ENSEMBL_ENTREZID <- clusterProfiler::bitr(a0$ensemble_id, fromType="ENSEMBL", toType=c("SYMBOL"),OrgDb="org.Hs.eg.db", drop = TRUE)
        colnames(convert_ENSEMBL_ENTREZID) <- c("ensemble_id","SYMBOL")
        a00 <- a0 %>% left_join(convert_ENSEMBL_ENTREZID,by="ensemble_id")
        
        a0 <- a00 %>% group_by(SYMBOL) %>% summarise_each(funs(mean=mean(., na.rm=TRUE))) %>% as.data.frame()
        MT_symbol <- c("MTRNR1","MTRNR2","MTTA","MTTC","MTTD","MTTE","MTTF","MTTG","MTTH","MTTI","MERRF","MTTK","MTTL1","MTTL2","MTTM","MTTN","MTTP","MTTQ","MTTR","MTTS1","MTTS2","MTTT","LIMM","MTTV","MTTW","MTTY")
        
        cli$Tumor_Sample_Barcode <- cli$RNA_Tumor_Sample_Barcode
        
        MT_data <- as.numeric(as.vector(rownames(a0[a0$SYMBOL %in% MT_symbol,])))
        a01 <- data.frame()
        if(length(MT_data)==0){
          a01 <- a0
        }else{
          a01 <- a0[-c(MT_data),]
        }
        a02 <- a01[!is.na(a01$SYMBOL),]
        rownames(a02) <- a02$SYMBOL
        id_tmp <- data.frame(id=colnames(a02)) %>% separate(id,into=c("RNA_Tumor_Sample_Barcode","_mean"),sep = "_mean" )
        colnames(a02) <- id_tmp$RNA_Tumor_Sample_Barcode
        colnames(a02) <- gsub('\\.', '-', colnames(a02))
        cli_RNA_list <- cli$RNA_Tumor_Sample_Barcode[cli$RNA_Tumor_Sample_Barcode %in% colnames(a02)]
        #n00 <- a02[complete.cases(a02), ]#24519    
        n000 <- as.data.frame(a02) %>% dplyr::select(cli_RNA_list)
        n <- n000[ave(rownames(n000), rownames(n000), FUN=length)==1,]
        n
      }
      data <- read_expression(RNA_full_exp,cli) %>% as.matrix()
      genelist <- intersect(rownames(data),genelist)
      data <- data[genelist,]
      data
    }
    zscore_mean <- function(data,v_title) {
      a1 <- pheatmap:::scale_rows(data) %>% apply(.,2,mean)
      a2 <- data.frame(zscore_mean=a1) 
      colnames(a2)[which(colnames(a2)=="zscore_mean")]  <- v_title
      a2
    }
    a <- signture_profile(RNA_full_exp,cli,genelist) %>% zscore_mean(.,v_title) %>% mutate(RNA_Tumor_Sample_Barcode=rownames(.))
    a2 <- a[cli$RNA_Tumor_Sample_Barcode,] %>% left_join(cli,by="RNA_Tumor_Sample_Barcode") 
    a2
  }
  signture_zscore_mean_data_select <- function(w.T1,j) {
    col <- c("Response","RNA_Tumor_Sample_Barcode","Type")
    w.T1.1<- w.T1 %>% dplyr::select(col,j) 
    colnames(w.T1.1) <- c(col,"cell_type")
    w.T1.1
  }
  signture_zscore_mean_plot <- function(data,j,list_com) {
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
  
    signture_zscore_mean_plot(.,v_title,list_com)
  data
}

dir <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4i/"


########################## CYT ##################################
data <- read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4g/Fig4g_1.txt",sep = "\t",header=T)


  data1 <- data %>% dplyr::select(Response,Type,CYT)
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
  title <-"Immune cytolytic activity"
  P1<-ggplot(data2, aes(single_type, as.numeric(CYT),color=Response))+
    geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
    geom_point(aes(color=Response), size=2, position = position_jitter(w=0.05,h= 0))+
    geom_signif(comparisons = list,step_increase = 0.1,map_signif_level=F,test="wilcox.test",color='black',textsize = 5,show.legend = T)+
    #scale_y_continuous(limits=range)+
    labs(title=title,x="",y="",size=25)+
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
  
  pdf(paste(dir,"geome.CYT.pdf",sep = ""),width=6.5, height=5)
  print(P)
  dev.off()
  
########################## CYT ##################################

  
  
  
########################## HLA-II ##################################
  data <- signture_zscore_mean_data(RNA_full_exp,cli,genelist,v_title) %>% 
    signture_zscore_mean_data_select(.,v_title) 
  write.table(data,file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4g/Fig4g_2.txt",sep = "\t" )
  data <- read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4g/Fig4g_2.txt",sep = "\t",header=T)
  
  





data1 <- data %>% dplyr::select(Response,Type,HLA_II)
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
title <- "HLA-II"
P1<-ggplot(data2, aes(single_type, as.numeric(HLA_II),color=Response))+
  geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
  geom_point(aes(color=Response), size=2, position = position_jitter(w=0.05,h= 0))+
  geom_signif(comparisons = list,step_increase = 0.1,map_signif_level=F,test="wilcox.test",color='black',textsize = 5,show.legend = T)+
  #scale_y_continuous(limits=range)+
  labs(title=title,x="",y="",size=25)+
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

pdf(paste(dir,"mean.MHCII.pdf",sep = ""),width=6.5, height=5)
print(P1)
dev.off()

########################## HLA-II ##################################


########################## DC ##################################
data <- read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure4/Fig4g/Fig4g_3.txt",sep = "\t",header=T)
data1 <- data %>% dplyr::select(Response,Type,DC)
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
title <- "DC"
P1<-ggplot(data2, aes(single_type, as.numeric(DC),color=Response))+
  geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
  geom_point(aes(color=Response), size=2, position = position_jitter(w=0.05,h= 0))+
  geom_signif(comparisons = list,step_increase = 0.1,map_signif_level=F,test="wilcox.test",color='black',textsize = 5,show.legend = T)+
  #scale_y_continuous(limits=range)+
  labs(title=title,x="",y="",size=25)+
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



pdf(paste(dir,"mean.DC.pdf",sep = ""),width=6.5, height=5)
print(P1)
dev.off()
########################## DC ##################################