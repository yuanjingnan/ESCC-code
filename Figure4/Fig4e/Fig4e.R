library(estimate)
ESTIMATE_score <-function(RNA_full_exp,cli,width,height,dir_path) {
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
  estimate_cal <- function(data) {
    estimate_dir <- "output/RNA/ESTIMATE/"
    estimate_in_txt <- paste(estimate_dir,id,".RNA.exp.txt",sep = "")
    estimate_out_txt <- paste(estimate_dir,id,".RNA.exp.gct",sep = "")
    estimate_out_score_txt <- paste(estimate_dir,id,".RNA.exp.estimate_score.gct",sep = "")
    write.table(data,file = estimate_in_txt,sep = "\t",quote = F,col.names = T,row.names = T)
    filterCommonGenes(input.f=estimate_in_txt, output.f=estimate_out_txt, id="GeneSymbol")
    estimateScore(input.ds = estimate_out_txt,output.ds=estimate_out_score_txt)
    scores=read.table(estimate_out_score_txt,skip = 2,header = T)
    rownames(scores)=scores[,1]
    scores=t(scores[,3:ncol(scores)])
    
    rownames(scores) <- gsub('\\.', '-', rownames(scores))
    estimate.list <- colnames(scores)
    
    cli$Tumor_Sample_Barcode <- cli$RNA_Tumor_Sample_Barcode
    w.v1 <- scores %>% as.data.frame()%>% 
      mutate(Tumor_Sample_Barcode=rownames(.)) %>% 
      left_join(cli,by="Tumor_Sample_Barcode") 
    w.v2 <- w.v1[complete.cases(w.v1), ]
    out <- list(w=w.v2,scores=scores)
    out
  }
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
  data <- read_expression(RNA_full_exp,cli) %>% as.matrix(.) %>% estimate_cal(.)
  estimate.list <- c("StromalScore","ImmuneScore","ESTIMATEScore","TumorPurity")
  pdf(paste(dir_path,"estimate_box.pdf",sep = ""),width=width, height=height)
  for (i in estimate.list) {
    P <- estimate_data_select(data$w,i) %>% estimate_data_plot(.,i,list_com)
    print(P)
  }
  dev.off()
  write.table(data$scores,file = "output/RNA/ESTIMATE/estimate_box.txt",sep = "\t",quote = F,col.names = T,row.names = T)
}

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure4/Fig4e/"
RNA_ESCC_full_exp <- paste(dir,"RNA.Exp.ENSEMBL.txt",sep = "")

load(file =paste(dir,"list_ESCC.rda",sep = ""),verbose = T)#list_ESCC
ESTIMATE_score(RNA_ESCC_full_exp,list_ESCC,7.5,5,dir)

