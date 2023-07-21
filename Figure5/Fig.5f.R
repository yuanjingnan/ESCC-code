library(cowplot)
run <- "PITC_clonespace"
PITC_clonespace <- list()
file_freq_name <- "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure5/Fig5f/TCR.rate.csv"

PITC_data_clonespace <- data.frame(sample_type=as.character(),change=as.character(),freq=as.numeric(),fromtime_from=as.character())
if(run=="PITC_clonespace"){
  library(ggalluvial)
  library(ggplot2)
  library(scales)
  library(ggfittext)
  
  file_freq_name <- "/Users/yuanjingnan/Desktop/daily/20230213/TCR.rate.csv"
  cli_TCR <- read.table("/Users/yuanjingnan/ESCC_2020/data/raw/all.txt",header = T,row.names = 1,sep = "\t") %>%
    mutate(ESCC_ID=rownames(.)) %>%
    separate(ESCC_ID,into=c("ESCC","P"))
  
  
  
  ITC_setting <- function(file_freq_name,cli_TCR) {
    ITC_freq <- read.csv(file = file_freq_name)
    
    ITC_freq[is.na(ITC_freq)] <- 0
    ITC_freq1 <- ITC_freq %>%
      mutate(TCR_Type=
               case_when(
                 T1 > 0 & T3 > 0 & T1 > T3 & WA==0 & WB==0 & WC==0 ~ "ContractT3",
                 T1 > 0 & T3 > 0 & T1 < T3 & WA==0 & WB==0 & WC==0 ~ "ExpandedT1",
                 
                 T1 > 0 & T3 > 0 & T1 > T3  ~ "Contract",
                 T1 > 0 & T3 > 0 & T1 < T3 ~ "Expanded",
                 
                 T1 == 0 & T3 > 0 & WA==0 & WB==0 & WC==0 ~ "T3onlyT3",
                 T3 == 0 & T1 > 0 & WA==0 & WB==0 & WC==0 ~ "T1onlyT1",
                 
                 T1 == 0 & T3 > 0  ~ "T3only",
                 T3 == 0 & T1 > 0  ~ "T1only",
                 
                 T3 == 0 & T1 == 0  ~ "Wonly",
                 TRUE ~ "NS")) %>%
      mutate(clone_size_T1=
               case_when(
                 T1 == 0 ~ "0%",
                 T1 > 0.001 ~ ">0.1%",
                 T1>0.0005 &T1 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
      mutate(clone_size_T3=
               case_when(T3 > 0.001 ~ ">0.1%",
                         T3>0.0005 &T3 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
      mutate(patients_name=sample)%>%
      mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))%>%
      left_join(cli_TCR,by="patients_name")%>%
      mutate(change=paste(clone_size_T1,TCR_Type,clone_size_T3,sep = "_"))
    
    ITC_freq_T1  <- ITC_freq1%>% dplyr::select(T1 ,sample,change,clone_size_T1)%>%
      mutate(sample_type=paste(sample,"T1",sep = "-"))
    colnames(ITC_freq_T1) <- c("freq","sample","change","clone_size","sample_type")
    
    ITC_freq_WA  <- ITC_freq1%>% dplyr::select(WA ,sample,change,clone_size_T1)%>%
      mutate(sample_type=paste(sample,"WA",sep = "-"))
    colnames(ITC_freq_WA) <- c("freq","sample","change","clone_size","sample_type")
    
    ITC_freq_WB  <- ITC_freq1%>% dplyr::select(WB ,sample,change,clone_size_T1)%>%
      mutate(sample_type=paste(sample,"WB",sep = "-"))
    colnames(ITC_freq_WB) <- c("freq","sample","change","clone_size","sample_type")
    
    ITC_freq_WC  <- ITC_freq1%>% dplyr::select(WC ,sample,change,clone_size_T1)%>%
      mutate(sample_type=paste(sample,"WC",sep = "-"))
    colnames(ITC_freq_WC) <- c("freq","sample","change","clone_size","sample_type")
    
    ITC_freq_T3  <- ITC_freq1%>% dplyr::select(T3,sample,change,clone_size_T3)%>%
      mutate(sample_type=paste(sample,"T3",sep = "-"))
    colnames(ITC_freq_T3) <- c("freq","sample","change","clone_size","sample_type")
    
    
    ITC_freq_T <- rbind(ITC_freq_T1,ITC_freq_WA,ITC_freq_WB,ITC_freq_WC,ITC_freq_T3)%>%
      mutate(patients_name=sample)%>%
      left_join(cli_TCR,by="patients_name")
    ITC_freq_T
    
  }
  ITC_freq_T <-ITC_setting(file_freq_name,cli_TCR)
  sample_ID <- c("LJZ-T1" ,"LJZ-T3","CJF-T1" ,"CJF-T3","ZAL-T1" ,"ZAL-T3" ,"YSN-T1" ,"YSN-T3","WWJ-T1" ,"WWJ-T3","SLS-T1" ,"SLS-T3" ,
                 "YUWM-T1","YUWM-T3","WGF-T1" ,"WGF-T3",
                 "FDY-T1" ,"FDY-T3","SKA-T1" ,"SKA-T3" ,"YWM-T1" ,"YWM-T3" ,"YHS-T1" , 
                 "YHS-T3" ,"WGS-T1" ,"WGS-T3" ,"ZHS-T1" ,"ZHS-T3","XAH-T1" ,"XAH-T3" ,"CCX-T1","CCX-T3", "FDH-T1" ,"FDH-T3",
                 "QHJ-T1" ,"QHJ-T3","QHR-T1" ,"QHR-T3","ZFM-T1" ,"ZFM-T3","LNF-T1" ,"LNF-T3",
                 "YHZ-T1" ,"YHZ-T3","ZGQ-T1" ,"ZGQ-T3","ZY -T1" ,"ZY -T3","ZGD-T1","ZGD-T3") 
  
  P <- c("LJZ","CJF","ZAL","YSN","WWJ","SLS","YUWM" ,"WGF","FDY","SKA","YWM","YHS","ZHS","XAH",
         "CCX","FDH","QHJ","QHR","ZFM","LNF","YHZ","ZGQ","ZGD")
  
  for (i in P) {
    ITC_freq_df <- ITC_freq_T %>% subset(.,patients_name==i)
    
    sample <- unique(ITC_freq_df$sample)
    
    ITC_freq_df1 <- ITC_freq_df %>%
      group_by(sample_type,clone_size,change) %>% 
      summarise(across(freq, sum)) %>%
      mutate(change1=change)%>%
      separate(change1,into = c("T1","change_type","T3"),sep = "_")
    
    ITC_freq_df1 <- as.data.frame(ITC_freq_df1) %>%
      dplyr::select(sample_type,change,freq,T1,change_type)%>%
      mutate(sample_type1=sample_type) %>%
      separate(sample_type1,into = c("patient","sample"),sep = "-")%>%
      mutate(T1_change_type=paste(T1,change_type,sep = "_")) %>%
      dplyr::select(sample_type,change,freq,T1_change_type,patient)
    
    colnames(ITC_freq_df1) <- c("sample_type","change","freq","clone_size","patient")
    
    patient <- unique(ITC_freq_df1$patient)
    
    ITC_freq_df1$sample_type <- factor(ITC_freq_df1$sample_type,levels = c(paste(patient,"-T1",sep = ""),paste(patient,"-WA",sep = ""),paste(patient,"-WB",sep = ""),paste(patient,"-WC",sep = ""),paste(patient,"-T3",sep = "")))
    ITC_freq_df1$clone_size <- factor(ITC_freq_df1$clone_size,levels = c(
      "0%_Wonly",
      ">0.1%_T1onlyT1","0.05%~0.1%_T1onlyT1","<0.05%_T1onlyT1",
      ">0.1%_T1only","0.05%~0.1%_T1only","<0.05%_T1only",
      "0%_T3onlyT3",
      "0%_T3only",
      
      ">0.1%_ExpandedT1","0.05%~0.1%_ExpandedT1","<0.05%_ExpandedT1",
      ">0.1%_Expanded","0.05%~0.1%_Expanded","<0.05%_Expanded",
      
      
      "<0.05%_ContractT3","0.05%~0.1%_ContractT3",">0.1%_ContractT3",
      "<0.05%_Contract","0.05%~0.1%_Contract",">0.1%_Contract"))
    
    ITC_freq_df1$change <- factor(ITC_freq_df1$change,levels = c(
      ">0.1%_Wonly_>0.1%",
      "0.05%~0.1%_Wonly_>0.1%","0.05%~0.1%_Wonly_0.05%~0.1%",
      "<0.05%_Wonly_<0.05%","<0.05%_Wonly_0.05%~0.1%","<0.05%_Wonly_>0.1%",
      "0%_Wonly_<0.05%",
      
      ">0.1%_T1onlyT1_<0.05%",
      "0.05%~0.1%_T1onlyT1_>0.1%","0.05%~0.1%_T1onlyT1_0.05%~0.1%","0.05%~0.1%_T1onlyT1_<0.05%",
      "<0.05%_T1onlyT1_<0.05%","<0.05%_T1onlyT1_0.05%~0.1%","<0.05%_T1onlyT1_>0.1%",
      
      
      
      ">0.1%_T1only_<0.05%",
      
      "0.05%~0.1%_T1only_>0.1%","0.05%~0.1%_T1only_0.05%~0.1%","0.05%~0.1%_T1only_<0.05%",
      "<0.05%_T1only_<0.05%","<0.05%_T1only_0.05%~0.1%","<0.05%_T1only_>0.1%",
      
      "0%_T3onlyT3_<0.05%","0%_T3onlyT3_>0.1%","0%_T3onlyT3_0.05%~0.1%",
      ">0.1%_T3only_>0.1%",
      "0.05%~0.1%_T3only_>0.1%","0.05%~0.1%_T3only_0.05%~0.1%",
      "<0.05%_T3only_<0.05%","<0.05%_T3only_0.05%~0.1%","<0.05%_T3only_>0.1%",
      "0%_T3only_<0.05%","0%_T3only_>0.1%","0%_T3only_0.05%~0.1%",
      
      
      ">0.1%_ExpandedT1_>0.1%",
      "0.05%~0.1%_ExpandedT1_>0.1%","0.05%~0.1%_ExpandedT1_0.05%~0.1%",
      "<0.05%_ExpandedT1_<0.05%","<0.05%_ExpandedT1_0.05%~0.1%","<0.05%_ExpandedT1_>0.1%",
      ">0.1%_Expanded_>0.1%",
      "0.05%~0.1%_Expanded_>0.1%","0.05%~0.1%_Expanded_0.05%~0.1%",
      "<0.05%_Expanded_<0.05%","<0.05%_Expanded_0.05%~0.1%","<0.05%_Expanded_>0.1%",
      
      
      "<0.05%_ContractT3_<0.05%",
      "0.05%~0.1%_ContractT3_0.05%~0.1%","0.05%~0.1%_ContractT3_<0.05%",
      ">0.1%_ContractT3_>0.1%",">0.1%_ContractT3_0.05%~0.1%",">0.1%_ContractT3_<0.05%",
      "<0.05%_Contract_<0.05%",
      "0.05%~0.1%_Contract_0.05%~0.1%","0.05%~0.1%_Contract_<0.05%",
      ">0.1%_Contract_>0.1%",">0.1%_Contract_0.05%~0.1%",">0.1%_Contract_<0.05%"
      
      
    ))
    ITC_freq_df1  <- ITC_freq_df1[order(ITC_freq_df1$change),c(1:4)]
    ITC_freq_df1 <- ITC_freq_df1 %>% dplyr::filter(freq>0)
    #"#C51B7D" "#E9A3C9" "#FDE0EF" "#F7F7F7" "#E6F5D0" "#A1D76A" "#4D9221"
    #"#EDF8FB" "#CCECE6" "#99D8C9" "#66C2A4" "#41AE76" "#238B45" "#005824"
    #"#EDF8E9" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#005A32"
    #"#FEEDDE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#8C2D04"
    #"#FEE5D9" "#FCBBA1" "#FC9272" "#FB6A4A" "#EF3B2C" "#CB181D" "#99000D"
    
    PITC_data_clonespace <- rbind(PITC_data_clonespace,ITC_freq_df1)
    PITC_clonespace[[subset(cli_TCR,patients_name==i)$P]] <- 
      
      ggplot(data = ITC_freq_df1,
             aes(x = sample_type, y = freq, alluvium = change,stratum = clone_size)) +
      theme_bw() +
      scale_fill_brewer(type = "qual", palette ="Set3")+
      geom_alluvium(aes(fill = clone_size),alpha = .75,width = 1/4) +
      geom_stratum(aes(stratum = clone_size,fill = clone_size,order = clone_size), alpha = 0,width = 1/4)+
      scale_fill_manual(
        values=c(rep("#FFBD16",3),rep("#FFBD16",3),rep("#FFBDDB",1),rep("#377EB8",1),rep("#377EB8",1),
                 rep("#41AB5D",3),rep("#EF3B2C",3),rep("#41AB5D",3),rep("#EF3B2C",3),
                 
                 rep("#FFBD16",8),rep("#FFBDDB",7),rep("#377EB8",11),
                 rep("#41AB5D",6),rep("#EF3B2C",6),rep("#FF901B",6)), 
        breaks=c(
          ">0.1%_T1onlyT1","0.05%~0.1%_T1onlyT1","<0.05%_T1onlyT1",                          
          ">0.1%_T1only","0.05%~0.1%_T1only","<0.05%_T1only",
          "0%_Wonly","0%_T3onlyT3",
          "0%_T3only",
          ">0.1%_Expanded","0.05%~0.1%_Expanded","<0.05%_Expanded",
          "<0.05%_Contract","0.05%~0.1%_Contract",">0.1%_Contract",
          
          ">0.1%_ExpandedT1","0.05%~0.1%_ExpandedT1","<0.05%_ExpandedT1",
          "<0.05%_ContractT3","0.05%~0.1%_ContractT3",">0.1%_ContractT3",
          
          
          ">0.1%_T1only_<0.05%",
          "0.05%~0.1%_T1only_>0.1%","0.05%~0.1%_T1only_0.05%~0.1%","0.05%~0.1%_T1only_<0.05%",
          "<0.05%_T1only_<0.05%","<0.05%_T1only_0.05%~0.1%","<0.05%_T1only_>0.1%",
          
          ">0.1%_Wonly_>0.1%",
          "0.05%~0.1%_Wonly_>0.1%","0.05%~0.1%_Wonly_0.05%~0.1%",
          "<0.05%_Wonly_<0.05%","<0.05%_Wonly_0.05%~0.1%","<0.05%_Wonly_>0.1%",
          "0%_Wonly_<0.05%",
          
          
          
          ">0.1%_T3only_>0.1%",
          "0.05%~0.1%_T3only_>0.1%","0.05%~0.1%_T3only_0.05%~0.1%",
          "<0.05%_T3only_<0.05%","<0.05%_T3only_0.05%~0.1%","<0.05%_T3only_>0.1%",
          "0%_T3only_<0.05%","0%_T3only_>0.1%","0%_T3only_0.05%~0.1%",
          
          ">0.1%_Expanded_>0.1%",
          "0.05%~0.1%_Expanded_>0.1%","0.05%~0.1%_Expanded_0.05%~0.1%",
          "<0.05%_Expanded_<0.05%","<0.05%_Expanded_0.05%~0.1%","<0.05%_Expanded_>0.1%",
          
          "<0.05%_Contract_<0.05%",
          "0.05%~0.1%_Contract_0.05%~0.1%","0.05%~0.1%_Contract_<0.05%",
          ">0.1%_Contract_>0.1%",">0.1%_Contract_0.05%~0.1%",">0.1%_Contract_<0.05%",
          
          ">0.1%_ExpandedT1_>0.1%",
          "0.05%~0.1%_ExpandedT1_>0.1%","0.05%~0.1%_ExpandedT1_0.05%~0.1%",
          "<0.05%_ExpandedT1_<0.05%","<0.05%_ExpandedT1_0.05%~0.1%","<0.05%_ExpandedT1_>0.1%",
          
          "<0.05%_ContractT3_<0.05%",
          "0.05%~0.1%_ContractT3_0.05%~0.1%","0.05%~0.1%_ContractT3_<0.05%",
          ">0.1%_ContractT3_>0.1%",">0.1%_ContractT3_0.05%~0.1%",">0.1%_ContractT3_<0.05%"
        ))+
      ggtitle(paste(subset(cli_TCR,patients_name==i)$P))+
      theme(legend.position = "none")+
      geom_text(aes(label = paste0(scales::percent(..count.., accuracy = 1.))), stat = "stratum", size = 3)
    #geom_text(stat = "flow", nudge_x = 0.2) 
    
    
  }
  
}



pdf(paste("/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure5/Fig5f/PITC_clonespace.pdf",sep = ""),width=40, height=20)
plot_grid(
  PITC_clonespace$P13, PITC_clonespace$P16, PITC_clonespace$P06, PITC_clonespace$P09, PITC_clonespace$P05 ,
  PITC_clonespace$P01, PITC_clonespace$P02, PITC_clonespace$P11 ,PITC_clonespace$P10 ,
  PITC_clonespace$P12, PITC_clonespace$P07 ,PITC_clonespace$P04 ,PITC_clonespace$P21 ,PITC_clonespace$P22, 
  PITC_clonespace$P17, PITC_clonespace$P24,PITC_clonespace$P20,
  labels = NULL, ncol = 5
)

dev.off()

LJZ_info <- PITC_data_clonespace %>% separate(sample_type,into=c("patients_name","sample"),sep="-") %>% 
  dplyr::filter(patients_name=="LJZ")
write.table(LJZ_info,file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure5/Fig5f/Fig5f.txt",sep = "\t" )
LJZ_info <- read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure5/Fig5f/Fig5f.txt",sep = "\t",header=T)
