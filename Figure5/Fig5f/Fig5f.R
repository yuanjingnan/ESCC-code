

library(plyr)
library(ggthemes)
library(rstatix)
library(EnhancedVolcano)
library(ggsignif)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)
library(cowplot)
library(dplyr)
library(tidyr)

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure5/Fig5f/"



###################### P13_T3_blood ###################### 
P_T3_blood <- list()
run <- "P_T3_blood"
P_T3_blood_data <- data.frame(sample_type=as.character(),change=as.character(),freq=as.numeric(),fromtime_from=as.character())

if(run=="P_T3_blood"){
  file_freq_name <- paste(dir,"TCR.Freq.RmNonProduct.csv",sep = "")
  
  cli_TCR <- read.table(paste(dir,"all.txt",sep = ""),header = T,row.names = 1,sep = "\t") %>%
    mutate(ESCC_ID=rownames(.)) %>%
    separate(ESCC_ID,into=c("ESCC","P"))
  ITC_freq <- read.csv(file = file_freq_name)
  ITC_freq[is.na(ITC_freq)] <- 0
  ITC_freq_other <- subset(ITC_freq,!sample%in%c("CCX","FDY","ZHS")) 
  ITC_freq_CCX <- subset(ITC_freq,sample%in%c("CCX")) %>% mutate(WC=WB)
  ITC_freq_FDY_ZHS <- subset(ITC_freq,sample%in%c("FDY","ZHS")) %>% mutate(WB=WA)
  ITC_freq_v1 <- rbind(ITC_freq_CCX,ITC_freq_FDY_ZHS,ITC_freq_other)
  ITC_freq1 <- ITC_freq_v1%>%
    subset(.,T1==0 &T3>0)%>%
    mutate(TCR_Type=
             case_when(T1 > T3  ~ "Contract",T1 < T3 ~ "Expanded",TRUE ~ "NS")) %>%
    mutate(clone_size_T1=
             case_when(T1 > 0.001 ~ ">0.1%",
                       T1>0.0005 &T1 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(clone_size_T3=
             case_when(T3 > 0.001 ~ ">0.1%",
                       T3>0.0005 &T3 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(patients_name=sample)%>%
    mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))%>%
    left_join(cli_TCR,by="patients_name")%>%
    mutate(change=paste(clone_size_T1,TCR_Type,clone_size_T3,sep = "_"))%>%
    mutate(blood_Type=
             case_when(T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "ITC_WA_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "ITC_WA_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "ITC_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "ITC_WA_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "ITC_WA",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "ITC_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "ITC_WC",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "ITC_ITC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "T3_only_WA_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "T3_only_WA_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "T3_only_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "T3_only_WA_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "T3_only_WA",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "T3_only_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "T3_only_WC",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "T3_only_no",
                       TRUE ~ "NS"))%>%
    mutate(fromtime=case_when(blood_Type %in%c("ITC_WA","ITC_WA_WB","ITC_WA_WB_WC","ITC_WA_WC","T3_only_WA","T3_only_WA_WB","T3_only_WA_WB_WC","T3_only_WA_WC" ) ~ "WA",
                              blood_Type %in%c("ITC_WB","ITC_WB_WC","T3_only_WB","T3_only_WB_WC") ~ "WB",
                              blood_Type %in%c("ITC_WC","T3_only_WC") ~ "WC",
                              blood_Type %in%c("T3_only_no") ~ "T3",
                              TRUE ~ "NS"))%>%
    dplyr::filter(fromtime%in%c("WA","WB","WC","T3"))
  ITC_freq2 <-  subset(ITC_freq1,blood_Type%in%c("T3_only_WA_WB_WC","T3_only_WA_WB","T3_only_WB_WC",
                                                 "T3_only_WA_WC","T3_only_WA","T3_only_WB","T3_only_WC","T3_only_no")) %>% mutate(change=paste(change,fromtime,sep = ":"))
  
  
  freq_WA <- subset(ITC_freq2,WA>0) %>% 
    mutate(sample_type=paste(patients_name,"WA",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)
  freq_WB <- subset(ITC_freq2,WB>0) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)
  freq_WC <- subset(ITC_freq2,WC>0) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WC,sample_type,change,clone_size_T1,sample)
  freq_T3 <- subset(ITC_freq2,T3>0) %>% 
    mutate(sample_type=paste(patients_name,"T3",sep = "-"))%>% 
    dplyr::select(T3,sample_type,change,clone_size_T1,sample)
  
  ITC_freq_WA_for_B  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WC","T3_only_WA","T3_only_WA_WC")) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WA_for_C  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WB","T3_only_WA","T3_only_WA_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WB_for_C  <- ITC_freq2%>%  dplyr::filter(WB>0) %>%
    subset(.,blood_Type%in%c("ITC_WB","T3_only_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WB=0)
  
  colnames(ITC_freq_WA_for_B) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WA_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WB_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WA) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WB) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WC) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_T3) <- c("freq","sample_type","change","clone_size","sample")
  
  
  
  freq_WB <- rbind(freq_WB,ITC_freq_WA_for_B)
  freq_WC <- rbind(freq_WC,ITC_freq_WA_for_C) %>% rbind(.,ITC_freq_WB_for_C)
  
  ITC_freq_B <- rbind(freq_WA,freq_WB,freq_WC,freq_T3)%>%
    mutate(patients_name=sample)%>%
    left_join(cli_TCR,by="patients_name")
  
  
  
  
  
  sample_ID <- c("LJZ-T1" ,"LJZ-T3","CJF-T1" ,"CJF-T3","ZAL-T1" ,"ZAL-T3" ,"YSN-T1" ,"YSN-T3","WWJ-T1" ,"WWJ-T3","SLS-T1" ,"SLS-T3" ,
                 "YUWM-T1","YUWM-T3","WGF-T1" ,"WGF-T3",
                 "FDY-T1" ,"FDY-T3","SKA-T1" ,"SKA-T3" ,"YWM-T1" ,"YWM-T3" ,"YHS-T1" , 
                 "YHS-T3" ,"WGS-T1" ,"WGS-T3" ,"ZHS-T1" ,"ZHS-T3","XAH-T1" ,"XAH-T3" ,"CCX-T1","CCX-T3", "FDH-T1" ,"FDH-T3",
                 "QHJ-T1" ,"QHJ-T3","QHR-T1" ,"QHR-T3","ZFM-T1" ,"ZFM-T3","LNF-T1" ,"LNF-T3",
                 "YHZ-T1" ,"YHZ-T3","ZGQ-T1" ,"ZGQ-T3","ZY -T1" ,"ZY -T3","ZGD-T1","ZGD-T3") 
  
  P <- c("LJZ","CJF","ZAL","YSN","WWJ","SLS","YUWM" ,"WGF","SKA","YWM","YHS","XAH",
         "FDH","QHJ","QHR","ZFM","LNF","YHZ","ZGQ","ZGD",c("FDY","CCX","ZHS"))
  
  
  
  for (i in P) {
    ITC_freq_df <- ITC_freq_B %>% subset(.,patients_name==i)
    ITC_freq_df1 <- ITC_freq_df %>%
      group_by(sample_type,clone_size,change) %>% 
      dplyr::summarise(dplyr::across(freq, sum))
    ITC_freq_df1 <- as.data.frame(ITC_freq_df1) %>%
      dplyr::select(sample_type,change,freq)%>%
      mutate(change1=change)%>%
      separate(change1,into=c("changetype","fromtime"),sep = ":")%>%
      mutate(sample_type1=sample_type)%>%
      separate(sample_type1,into=c("sample_type2","sample_type3"),sep = "-") %>%   
      mutate(fromtime_from=paste(fromtime,sample_type3,sep = "_"))%>%
      dplyr::select(sample_type,change,freq,fromtime_from)
    
    colnames(ITC_freq_df1) <- c("sample_type","change","freq","fromtime_from")
    
    ITC_freq_df1$sample_type <- factor(ITC_freq_df1$sample_type,levels = c(paste(i,"WA",sep = "-"),paste(i,"WB",sep = "-"),
                                                                           paste(i,"WC",sep = "-"),paste(i,"T3",sep = "-")))
    
    
    ITC_freq_df1$change <- factor(ITC_freq_df1$change,levels = c(
      ">0.1%_Expanded_>0.1%:T3",
      "0.05%~0.1%_Expanded_>0.1%:T3","0.05%~0.1%_Expanded_0.05%~0.1%:T3",
      "<0.05%_Expanded_<0.05%:T3","<0.05%_Expanded_0.05%~0.1%:T3","<0.05%_Expanded_>0.1%:T3",
      
      ">0.1%_Expanded_>0.1%:WC",
      "0.05%~0.1%_Expanded_>0.1%:WC","0.05%~0.1%_Expanded_0.05%~0.1%:WC",
      "<0.05%_Expanded_<0.05%:WC","<0.05%_Expanded_0.05%~0.1%:WC","<0.05%_Expanded_>0.1%:WC",
      
      ">0.1%_Expanded_>0.1%:WB",
      "0.05%~0.1%_Expanded_>0.1%:WB","0.05%~0.1%_Expanded_0.05%~0.1%:WB",
      "<0.05%_Expanded_<0.05%:WB","<0.05%_Expanded_0.05%~0.1%:WB","<0.05%_Expanded_>0.1%:WB",
      
      ">0.1%_Expanded_>0.1%:WA",
      "0.05%~0.1%_Expanded_>0.1%:WA","0.05%~0.1%_Expanded_0.05%~0.1%:WA",
      "<0.05%_Expanded_<0.05%:WA","<0.05%_Expanded_0.05%~0.1%:WA","<0.05%_Expanded_>0.1%:WA"))
    ITC_freq_df1$fromtime <- factor(ITC_freq_df1$fromtime,levels =c("T1","WC","WB","WA"))
    ITC_freq_df1$fromtime_from <- factor(ITC_freq_df1$fromtime_from,levels =c("T3_T3","WC_T3","WB_T3","WA_T3",
                                                                              "T3_WC","WC_WC","WB_WC","WA_WC",
                                                                              "T3_WB","WC_WB","WB_WB","WA_WB",
                                                                              "T3_WA","WC_WA","WB_WA","WA_WA"))
    
    ITC_freq_df1  <- ITC_freq_df1[order(ITC_freq_df1$change),c(1:4)] 
    P_T3_blood_data <- rbind(P_T3_blood_data,ITC_freq_df1)
    
    P_T3_blood[[subset(cli_TCR,patients_name==i)$P]] <- 
      ggplot(data = ITC_freq_df1,aes(x = sample_type, y = freq, alluvium = change,stratum = fromtime_from)) +
      theme_bw() +
      geom_alluvium(aes(fill = fromtime_from,order = change),alpha = .75,width = 1/2) +
      geom_stratum(aes(stratum = fromtime_from,fill = fromtime_from,order = fromtime_from), width = 1/2,alpha = 0)+
      scale_fill_manual(values=c(
        
        "#08306B","#377EB8" ,"#9ECAE1","#DEEBF7" ,
                 "#08306B","#377EB8" ,"#9ECAE1","#DEEBF7" ,
                 "#08306B","#377EB8" ,"#9ECAE1","#DEEBF7" ,
                 "#08306B","#377EB8" ,"#9ECAE1","#DEEBF7" ,
                 rep("#08306B",6),
                 rep("#377EB8" ,6),rep("#9ECAE1" ,6),rep("#DEEBF7" ,6)), 
        
        breaks=c(
          "T3_T3","WC_T3","WB_T3","WA_T3",
          "T3_WC","WC_WC","WB_WC","WA_WC",
          "T3_WB","WC_WB","WB_WB","WA_WB",
          "T3_WA","WC_WA","WB_WA","WA_WA",
          ">0.1%_Expanded_>0.1%:T3",
          "0.05%~0.1%_Expanded_>0.1%:T3","0.05%~0.1%_Expanded_0.05%~0.1%:T3",
          "<0.05%_Expanded_<0.05%:T3","<0.05%_Expanded_0.05%~0.1%:T3","<0.05%_Expanded_>0.1%:T3",
          
          ">0.1%_Expanded_>0.1%:WC",
          "0.05%~0.1%_Expanded_>0.1%:WC","0.05%~0.1%_Expanded_0.05%~0.1%:WC",
          "<0.05%_Expanded_<0.05%:WC","<0.05%_Expanded_0.05%~0.1%:WC","<0.05%_Expanded_>0.1%:WC",
          
          ">0.1%_Expanded_>0.1%:WB",
          "0.05%~0.1%_Expanded_>0.1%:WB","0.05%~0.1%_Expanded_0.05%~0.1%:WB",
          "<0.05%_Expanded_<0.05%:WB","<0.05%_Expanded_0.05%~0.1%:WB","<0.05%_Expanded_>0.1%:WB",
          
          ">0.1%_Expanded_>0.1%:WA",
          "0.05%~0.1%_Expanded_>0.1%:WA","0.05%~0.1%_Expanded_0.05%~0.1%:WA",
          "<0.05%_Expanded_<0.05%:WA","<0.05%_Expanded_0.05%~0.1%:WA","<0.05%_Expanded_>0.1%:WA"))+
      ggtitle(paste(subset(cli_TCR,patients_name==i)$P))+
      theme(legend.position = "none")+
      geom_text(aes(label = paste0(scales::percent(..count.., accuracy = .1))), stat = "stratum", size = 3)
    
  }
  
}

pdf(paste(dir,"P13_T3_only_blood.pdf",sep = ""),width=5, height=5)
P_T3_blood$P13
dev.off()

###################### P13_T3_blood ###################### 

###################### P13_ITC_blood_Contracted ###################### 



P_ITC_blood_Contracted <- list()
P_ITC_blood_Contracted_data <- data.frame(sample_type=as.character(),change=as.character(),freq=as.numeric(),fromtime_from=as.character())

run <- "ITC_blood_Contracted"
if(run=="ITC_blood_Contracted"){
  
  file_freq_name <- paste(dir,"TCR.Freq.RmNonProduct.csv",sep = "")
  
  
  cli_TCR <- read.table(paste(dir,"all.txt",sep = ""),header = T,row.names = 1,sep = "\t") %>%
    mutate(ESCC_ID=rownames(.)) %>%
    separate(ESCC_ID,into=c("ESCC","P"))
  
  
  ITC_freq <- read.csv(file = file_freq_name)
  ITC_freq[is.na(ITC_freq)] <- 0
  ITC_freq_other <- subset(ITC_freq,!sample%in%c("CCX","FDY","ZHS")) 
  ITC_freq_CCX <- subset(ITC_freq,sample%in%c("CCX")) %>% mutate(WC=WB)
  ITC_freq_FDY_ZHS <- subset(ITC_freq,sample%in%c("FDY","ZHS")) %>% mutate(WB=WA)
  ITC_freq_v1 <- rbind(ITC_freq_CCX,ITC_freq_FDY_ZHS,ITC_freq_other)
  ITC_freq1 <- ITC_freq_v1%>%
    subset(.,T1>0 & T3>0)%>%
    mutate(TCR_Type=
             case_when(T1 > T3  ~ "Contract",T1 < T3 ~ "Expanded",TRUE ~ "NS")) %>%
    mutate(clone_size_T1=
             case_when(T1 > 0.001 ~ ">0.1%",
                       T1>0.0005 &T1 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(clone_size_T3=
             case_when(T3 > 0.001 ~ ">0.1%",
                       T3>0.0005 &T3 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(patients_name=sample)%>%
    mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))%>%
    left_join(cli_TCR,by="patients_name")%>%
    mutate(change=paste(clone_size_T1,TCR_Type,clone_size_T3,sep = "_"))%>%
    mutate(blood_Type=
             case_when(T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "ITC_WA_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "ITC_WA_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "ITC_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "ITC_WA_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "ITC_WA",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "ITC_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "ITC_WC",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "ITC_ITC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "T3_only_WA_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "T3_only_WA_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "T3_only_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "T3_only_WA_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "T3_only_WA",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "T3_only_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "T3_only_WC",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "T3_only_no",
                       TRUE ~ "NS"))%>%
    mutate(fromtime=case_when(blood_Type %in%c("ITC_WA","ITC_WA_WB","ITC_WA_WB_WC","ITC_WA_WC","T3_only_WA","T3_only_WA_WB","T3_only_WA_WB_WC","T3_only_WA_WC" ) ~ "WA",
                              blood_Type %in%c("ITC_WB","ITC_WB_WC","T3_only_WB","T3_only_WB_WC") ~ "WB",
                              blood_Type %in%c("ITC_WC","T3_only_WC") ~ "WC",
                              blood_Type %in%c("ITC_ITC") ~ "T1",
                              TRUE ~ "NS"))%>%
    dplyr::filter(TCR_Type=="Contract")%>%
    dplyr::filter(fromtime%in%c("WA","WB","WC","T1"))
  ITC_freq2 <-  subset(ITC_freq1,blood_Type%in%c("ITC_WA_WB_WC","ITC_WA_WB","ITC_WB_WC",
                                                 "ITC_WA_WC","ITC_WA","ITC_WB","ITC_WC","ITC_ITC")) %>% mutate(change=paste(change,fromtime,sep = ":"))
  
  
  freq_WA <- subset(ITC_freq2,WA>0) %>% 
    mutate(sample_type=paste(patients_name,"WA",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)
  freq_WB <- subset(ITC_freq2,WB>0) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)
  freq_WC <- subset(ITC_freq2,WC>0) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WC,sample_type,change,clone_size_T1,sample)
  freq_T3 <- subset(ITC_freq2,T3>0) %>% 
    mutate(sample_type=paste(patients_name,"T3",sep = "-"))%>% 
    dplyr::select(T3,sample_type,change,clone_size_T1,sample)
  
  ITC_freq_WA_for_B  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WC","T3_only_WA","T3_only_WA_WC")) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WA_for_C  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WB","T3_only_WA","T3_only_WA_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WB_for_C  <- ITC_freq2%>%  dplyr::filter(WB>0) %>%
    subset(.,blood_Type%in%c("ITC_WB","T3_only_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WB=0)
  
  colnames(ITC_freq_WA_for_B) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WA_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WB_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WA) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WB) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WC) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_T3) <- c("freq","sample_type","change","clone_size","sample")
  
  
  
  freq_WB <- rbind(freq_WB,ITC_freq_WA_for_B)
  freq_WC <- rbind(freq_WC,ITC_freq_WA_for_C) %>% rbind(.,ITC_freq_WB_for_C)
  
  ITC_freq_B <- rbind(freq_WA,freq_WB,freq_WC,freq_T3)%>%
    mutate(patients_name=sample)%>%
    left_join(cli_TCR,by="patients_name")
  
  
  
  
  
  sample_ID <- c("LJZ-T1" ,"LJZ-T3","CJF-T1" ,"CJF-T3","ZAL-T1" ,"ZAL-T3" ,"YSN-T1" ,"YSN-T3","WWJ-T1" ,"WWJ-T3","SLS-T1" ,"SLS-T3" ,
                 "YUWM-T1","YUWM-T3","WGF-T1" ,"WGF-T3",
                 "FDY-T1" ,"FDY-T3","SKA-T1" ,"SKA-T3" ,"YWM-T1" ,"YWM-T3" ,"YHS-T1" , 
                 "YHS-T3" ,"WGS-T1" ,"WGS-T3" ,"ZHS-T1" ,"ZHS-T3","XAH-T1" ,"XAH-T3" ,"CCX-T1","CCX-T3", "FDH-T1" ,"FDH-T3",
                 "QHJ-T1" ,"QHJ-T3","QHR-T1" ,"QHR-T3","ZFM-T1" ,"ZFM-T3","LNF-T1" ,"LNF-T3",
                 "YHZ-T1" ,"YHZ-T3","ZGQ-T1" ,"ZGQ-T3","ZY -T1" ,"ZY -T3","ZGD-T1","ZGD-T3") 
  
  P <- c("LJZ","CJF","ZAL","YSN","WWJ","SLS","YUWM" ,"WGF","SKA","YWM","YHS","XAH",
         "FDH","QHJ","QHR","ZFM","LNF","YHZ","ZGQ","ZGD",c("FDY","CCX","ZHS"))
  
  
  
  for (i in P) {
    ITC_freq_df <- ITC_freq_B %>% subset(.,patients_name==i)
    ITC_freq_df1 <- ITC_freq_df %>%
      group_by(sample_type,clone_size,change) %>% 
      dplyr::summarise(dplyr::across(freq, sum))
    ITC_freq_df1 <- as.data.frame(ITC_freq_df1) %>%
      dplyr::select(sample_type,change,freq)%>%
      mutate(change1=change)%>%
      separate(change1,into=c("changetype","fromtime"),sep = ":")%>%
      mutate(sample_type1=sample_type)%>%
      separate(sample_type1,into=c("sample_type2","sample_type3"),sep = "-") %>%   
      mutate(fromtime_from=paste(fromtime,sample_type3,sep = "_"))%>%
      dplyr::select(sample_type,change,freq,fromtime_from)
    
    colnames(ITC_freq_df1) <- c("sample_type","change","freq","fromtime_from")
    
    ITC_freq_df1$sample_type <- factor(ITC_freq_df1$sample_type,levels = c(paste(i,"WA",sep = "-"),paste(i,"WB",sep = "-"),
                                                                           paste(i,"WC",sep = "-"),paste(i,"T3",sep = "-")))
    
    
    ITC_freq_df1$change <- factor(ITC_freq_df1$change,levels = c(
      ">0.1%_Contract_>0.1%:T1",">0.1%_Contract_0.05%~0.1%:T1",">0.1%_Contract_<0.05%:T1",
      "0.05%~0.1%_Contract_0.05%~0.1%:T1","0.05%~0.1%_Contract_<0.05%:T1",
      "<0.05%_Contract_<0.05%:T1",
      
      ">0.1%_Contract_>0.1%:WC",">0.1%_Contract_0.05%~0.1%:WC",">0.1%_Contract_<0.05%:WC",
      "0.05%~0.1%_Contract_0.05%~0.1%:WC","0.05%~0.1%_Contract_<0.05%:WC",
      "<0.05%_Contract_<0.05%:WC",
      
      ">0.1%_Contract_>0.1%:WB",">0.1%_Contract_0.05%~0.1%:WB",">0.1%_Contract_<0.05%:WB",
      "0.05%~0.1%_Contract_0.05%~0.1%:WB","0.05%~0.1%_Contract_<0.05%:WB",
      "<0.05%_Contract_<0.05%:WB",
      
      ">0.1%_Contract_>0.1%:WA",">0.1%_Contract_0.05%~0.1%:WA",">0.1%_Contract_<0.05%:WA",
      "0.05%~0.1%_Contract_0.05%~0.1%:WA","0.05%~0.1%_Contract_<0.05%:WA",
      "<0.05%_Contract_<0.05%:WA"))
    ITC_freq_df1$fromtime <- factor(ITC_freq_df1$fromtime,levels =c("T1","WC","WB","WA"))
    ITC_freq_df1$fromtime_from <- factor(ITC_freq_df1$fromtime_from,levels =c("T1_T3","WC_T3","WB_T3","WA_T3",
                                                                              "T1_WC","WC_WC","WB_WC","WA_WC",
                                                                              "T1_WB","WC_WB","WB_WB","WA_WB",
                                                                              "T1_WA","WC_WA","WB_WA","WA_WA"))
    
    ITC_freq_df1  <- ITC_freq_df1[order(ITC_freq_df1$change),c(1:4)] 
    
    P_ITC_blood_Contracted_data <- rbind(P_ITC_blood_Contracted_data,ITC_freq_df1)
    P_ITC_blood_Contracted[[subset(cli_TCR,patients_name==i)$P]] <- 
      
      ggplot(data = ITC_freq_df1,aes(x = sample_type, y = freq, alluvium = change,stratum = fromtime_from)) +
      theme_bw() +
      geom_alluvium(aes(fill = fromtime_from,order = change),alpha = .75,width = 1/2) +
      geom_stratum(aes(stratum = fromtime_from,fill = fromtime_from,order = fromtime_from), width = 1/2,alpha = 0)+
      scale_fill_manual(values=c(
        
        "#99000D","#EF3B2C" ,"#FDAE6B","#FDD0A2" ,
                 "#99000D","#EF3B2C" ,"#FDAE6B","#FDD0A2",
                 "#99000D","#EF3B2C" ,"#FDAE6B","#FDD0A2" ,
                 "#99000D","#EF3B2C" ,"#FDAE6B","#FDD0A2" ,
                 rep("#99000D",6),
                 rep("#EF3B2C" ,6),rep("#FDAE6B" ,6),rep("#FDD0A2" ,6)), 
        
        breaks=c(
          "T1_T3","WC_T3","WB_T3","WA_T3",
          "T1_WC","WC_WC","WB_WC","WA_WC",
          "T1_WB","WC_WB","WB_WB","WA_WB",
          "T1_WA","WC_WA","WB_WA","WA_WA",
          ">0.1%_Contract_>0.1%:T1",">0.1%_Contract_0.05%~0.1%:T1",">0.1%_Contract_<0.05%:T1",
          "0.05%~0.1%_Contract_0.05%~0.1%:T1","0.05%~0.1%_Contract_<0.05%:T1",
          "<0.05%_Contract_<0.05%:T1",
          
          ">0.1%_Contract_>0.1%:WC",">0.1%_Contract_0.05%~0.1%:WC",">0.1%_Contract_<0.05%:WC",
          "0.05%~0.1%_Contract_0.05%~0.1%:WC","0.05%~0.1%_Contract_<0.05%:WC",
          "<0.05%_Contract_<0.05%:WC",
          
          ">0.1%_Contract_>0.1%:WB",">0.1%_Contract_0.05%~0.1%:WB",">0.1%_Contract_<0.05%:WB",
          "0.05%~0.1%_Contract_0.05%~0.1%:WB","0.05%~0.1%_Contract_<0.05%:WB",
          "<0.05%_Contract_<0.05%:WB",
          
          ">0.1%_Contract_>0.1%:WA",">0.1%_Contract_0.05%~0.1%:WA",">0.1%_Contract_<0.05%:WA",
          "0.05%~0.1%_Contract_0.05%~0.1%:WA","0.05%~0.1%_Contract_<0.05%:WA",
          "<0.05%_Contract_<0.05%:WA"
        ))+
      ggtitle(paste(subset(cli_TCR,patients_name==i)$P))+
      theme(legend.position = "none")+
      geom_text(aes(label = paste0(scales::percent(..count.., accuracy = .1))), stat = "stratum", size = 3)
    
  }
  
}


pdf(paste(dir,"P13_ITC_blood_Contracted.pdf",sep = ""),width=5, height=5)
P_ITC_blood_Contracted$P13
dev.off()









P_ITC_blood_Contracted_data1 <- P_ITC_blood_Contracted_data %>% mutate(fromtime_from1= paste(fromtime_from,"Contract",sep = "_"))

P_ITC_blood_Expanded_data1 <- P_ITC_blood_Expanded_data %>% mutate(fromtime_from1= paste(fromtime_from,"Expanded",sep = "_"))

P_ITC_blood <- rbind(P_ITC_blood_Contracted_data1,P_ITC_blood_Expanded_data1) %>%
  group_by(fromtime_from1,sample_type) %>% 
  summarise(across(freq, sum)) %>% 
  as.data.frame()%>% 
  mutate(sample_type1=sample_type) %>% separate(sample_type1,into=c("patients_name","sample_id")) %>% 
  left_join(cli_TCR,by="patients_name")%>%
  mutate(sample_id1= paste(sample_id,Response))





list <- combn(c("T3 Well responders","T3 Poor responders",
                "WA Well responders","WA Poor responders",
                "WB Well responders","WB Poor responders",
                "WC Well responders","WC Poor responders"), simplify = F)


list <- list(c("T3 Well responders","T3 Poor responders"),
             c("WA Well responders","WA Poor responders"),
             c("WB Well responders","WB Poor responders"),
             c("WC Well responders","WC Poor responders"))
P_ITC_blood$sample_id1 <- factor(P_ITC_blood$sample_id1, levels = c("T3 Well responders","T3 Poor responders",
                                                                    "WA Well responders","WA Poor responders",
                                                                    "WB Well responders","WB Poor responders",
                                                                    "WC Well responders","WC Poor responders"))

PITC_P5 <- ggplot(P_ITC_blood, aes(sample_id1, as.numeric(freq),color=sample_id1))+
  geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
  geom_point(aes(color=sample_id1), size=2, position = position_jitter(w=0.05,h= 0))+
  facet_wrap(~fromtime_from1,nrow=5)+
  geom_signif(comparisons = list,
              step_increase = 0.1,map_signif_level=F,test="wilcox.test",
              color='black',textsize = 5,show.legend = T)+
  labs(title="",x="",y="",size=25)+
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
        axis.line = element_line(size=1, colour = "black"))


P_ITC_blood <- rbind(P_ITC_blood_Contracted_data1,P_ITC_blood_Expanded_data1) %>%
  group_by(fromtime_from1,sample_type) %>% 
  summarise(across(freq, sum)) %>% 
  as.data.frame()%>% 
  mutate(sample_type1=sample_type) %>% separate(sample_type1,into=c("patients_name","sample_id")) %>% 
  left_join(cli_TCR,by="patients_name")%>%
  mutate(sample_id1= paste(sample_id,Response))


list <- list(c("T1_T3_Expanded","T1_T3_Contract"),c("WA_T3_Expanded","WA_T3_Contract"),c("WB_T3_Expanded","WB_T3_Contract"),c("WC_T3_Expanded","WC_T3_Contract"),
             c("WA_WA_Expanded","WA_WA_Contract"),
             c("WA_WB_Expanded","WA_WB_Contract"),c("WB_WB_Expanded","WB_WB_Contract"),
             c("WA_WC_Expanded","WA_WC_Contract"),c("WB_WC_Expanded","WB_WC_Contract"),c("WC_WC_Expanded","WC_WC_Contract"))

P_ITC_blood$fromtime_from1 <- factor(P_ITC_blood$fromtime_from1, levels = c(c("T1_T3_Expanded","T1_T3_Contract"),c("WA_T3_Expanded","WA_T3_Contract"),c("WB_T3_Expanded","WB_T3_Contract"),c("WC_T3_Expanded","WC_T3_Contract"),
                                                                            c("WA_WA_Expanded","WA_WA_Contract"),
                                                                            c("WA_WB_Expanded","WA_WB_Contract"),c("WB_WB_Expanded","WB_WB_Contract"),
                                                                            c("WA_WC_Expanded","WA_WC_Contract"),c("WB_WC_Expanded","WB_WC_Contract"),c("WC_WC_Expanded","WC_WC_Contract")))

PITC_P6 <- ggplot(P_ITC_blood, aes(fromtime_from1, as.numeric(freq),color=fromtime_from1))+
  geom_boxplot(size = 1,width=0.7,position = position_dodge(1))+
  geom_point(aes(color=fromtime_from1), size=2, position = position_jitter(w=0.05,h= 0))+
  facet_wrap(~sample_id1,nrow=5)+
  geom_signif(comparisons = list,
              step_increase = 0.1,map_signif_level=F,test="wilcox.test",
              color='black',textsize = 5,show.legend = T)+
  labs(title="",x="",y="",size=25)+
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
        axis.line = element_line(size=1, colour = "black"))




pdf(paste("/Users/yuanjingnan/Desktop/daily/20221117/ITC_blood.stat.2.pdf",sep = ""),width=20, height=20)


print(PITC_P5)
print(PITC_P6)

dev.off()


###################### P13_ITC_blood_Contracted ###################### 


###################### P13_ITC_blood_Expanded ###################### 

P_ITC_blood_Expanded <- list()
run <- "ITC_blood_Expanded"
P_ITC_blood_Expanded_data <- data.frame(sample_type=as.character(),change=as.character(),freq=as.numeric(),fromtime_from=as.character())

if(run=="ITC_blood_Expanded"){
  
  file_freq_name <- paste(dir,"TCR.Freq.RmNonProduct.csv",sep = "")
  cli_TCR <- read.table(paste(dir,"all.txt",sep = ""),header = T,row.names = 1,sep = "\t") %>%
    mutate(ESCC_ID=rownames(.)) %>%
    separate(ESCC_ID,into=c("ESCC","P"))
  
  
  ITC_freq <- read.csv(file = file_freq_name)
  ITC_freq[is.na(ITC_freq)] <- 0
  ITC_freq_other <- subset(ITC_freq,!sample%in%c("CCX","FDY","ZHS")) 
  ITC_freq_CCX <- subset(ITC_freq,sample%in%c("CCX")) %>% mutate(WC=WB)
  ITC_freq_FDY_ZHS <- subset(ITC_freq,sample%in%c("FDY","ZHS")) %>% mutate(WB=WA)
  ITC_freq_v1 <- rbind(ITC_freq_CCX,ITC_freq_FDY_ZHS,ITC_freq_other)
  ITC_freq1 <- ITC_freq_v1%>%
    subset(.,T1>0 & T3>0)%>%
    mutate(TCR_Type=
             case_when(T1 > T3  ~ "Contract",T1 < T3 ~ "Expanded",TRUE ~ "NS")) %>%
    mutate(clone_size_T1=
             case_when(T1 > 0.001 ~ ">0.1%",
                       T1>0.0005 &T1 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(clone_size_T3=
             case_when(T3 > 0.001 ~ ">0.1%",
                       T3>0.0005 &T3 < 0.001 ~ "0.05%~0.1%",TRUE ~ "<0.05%"))%>%
    mutate(patients_name=sample)%>%
    mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))%>%
    left_join(cli_TCR,by="patients_name")%>%
    mutate(change=paste(clone_size_T1,TCR_Type,clone_size_T3,sep = "_"))%>%
    mutate(blood_Type=
             case_when(T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "ITC_WA_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "ITC_WA_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "ITC_WB_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "ITC_WA_WC",
                       T1 > 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "ITC_WA",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "ITC_WB",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "ITC_WC",
                       T1 > 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "ITC_ITC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC > 0 ~ "T3_only_WA_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB > 0 & WC == 0 ~ "T3_only_WA_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC > 0 ~ "T3_only_WB_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC > 0 ~ "T3_only_WA_WC",
                       T1 == 0 &  T3 > 0 & WA > 0 & WB == 0 & WC == 0 ~ "T3_only_WA",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB > 0 & WC == 0 ~ "T3_only_WB",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC > 0 ~ "T3_only_WC",
                       T1 == 0 &  T3 > 0 & WA == 0 & WB == 0 & WC == 0 ~ "T3_only_no",
                       TRUE ~ "NS"))%>%
    mutate(fromtime=case_when(blood_Type %in%c("ITC_WA","ITC_WA_WB","ITC_WA_WB_WC","ITC_WA_WC","T3_only_WA","T3_only_WA_WB","T3_only_WA_WB_WC","T3_only_WA_WC" ) ~ "WA",
                              blood_Type %in%c("ITC_WB","ITC_WB_WC","T3_only_WB","T3_only_WB_WC") ~ "WB",
                              blood_Type %in%c("ITC_WC","T3_only_WC") ~ "WC",
                              blood_Type %in%c("ITC_ITC") ~ "T1",
                              TRUE ~ "NS"))%>%
    dplyr::filter(TCR_Type=="Expanded")%>%
    dplyr::filter(fromtime%in%c("WA","WB","WC","T1"))
  ITC_freq2 <-  subset(ITC_freq1,blood_Type%in%c("ITC_WA_WB_WC","ITC_WA_WB","ITC_WB_WC",
                                                 "ITC_WA_WC","ITC_WA","ITC_WB","ITC_WC","ITC_ITC")) %>% mutate(change=paste(change,fromtime,sep = ":"))
  
  
  freq_WA <- subset(ITC_freq2,WA>0) %>% 
    mutate(sample_type=paste(patients_name,"WA",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)
  freq_WB <- subset(ITC_freq2,WB>0) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)
  freq_WC <- subset(ITC_freq2,WC>0) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WC,sample_type,change,clone_size_T1,sample)
  freq_T3 <- subset(ITC_freq2,T3>0) %>% 
    mutate(sample_type=paste(patients_name,"T3",sep = "-"))%>% 
    dplyr::select(T3,sample_type,change,clone_size_T1,sample)
  
  ITC_freq_WA_for_B  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WC","T3_only_WA","T3_only_WA_WC")) %>% 
    mutate(sample_type=paste(patients_name,"WB",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WA_for_C  <- ITC_freq2%>%  dplyr::filter(WA>0) %>%
    subset(.,blood_Type%in%c("ITC_WA","ITC_WA_WB","T3_only_WA","T3_only_WA_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WA,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WA=0)
  
  
  ITC_freq_WB_for_C  <- ITC_freq2%>%  dplyr::filter(WB>0) %>%
    subset(.,blood_Type%in%c("ITC_WB","T3_only_WB")) %>% 
    mutate(sample_type=paste(patients_name,"WC",sep = "-"))%>% 
    dplyr::select(WB,sample_type,change,clone_size_T1,sample)%>% 
    mutate(WB=0)
  
  colnames(ITC_freq_WA_for_B) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WA_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(ITC_freq_WB_for_C) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WA) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WB) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_WC) <- c("freq","sample_type","change","clone_size","sample")
  colnames(freq_T3) <- c("freq","sample_type","change","clone_size","sample")
  
  
  
  freq_WB <- rbind(freq_WB,ITC_freq_WA_for_B)
  freq_WC <- rbind(freq_WC,ITC_freq_WA_for_C) %>% rbind(.,ITC_freq_WB_for_C)
  
  ITC_freq_B <- rbind(freq_WA,freq_WB,freq_WC,freq_T3)%>%
    mutate(patients_name=sample)%>%
    left_join(cli_TCR,by="patients_name")
  
  
  
  
  
  sample_ID <- c("LJZ-T1" ,"LJZ-T3","CJF-T1" ,"CJF-T3","ZAL-T1" ,"ZAL-T3" ,"YSN-T1" ,"YSN-T3","WWJ-T1" ,"WWJ-T3","SLS-T1" ,"SLS-T3" ,
                 "YUWM-T1","YUWM-T3","WGF-T1" ,"WGF-T3",
                 "FDY-T1" ,"FDY-T3","SKA-T1" ,"SKA-T3" ,"YWM-T1" ,"YWM-T3" ,"YHS-T1" , 
                 "YHS-T3" ,"WGS-T1" ,"WGS-T3" ,"ZHS-T1" ,"ZHS-T3","XAH-T1" ,"XAH-T3" ,"CCX-T1","CCX-T3", "FDH-T1" ,"FDH-T3",
                 "QHJ-T1" ,"QHJ-T3","QHR-T1" ,"QHR-T3","ZFM-T1" ,"ZFM-T3","LNF-T1" ,"LNF-T3",
                 "YHZ-T1" ,"YHZ-T3","ZGQ-T1" ,"ZGQ-T3","ZY -T1" ,"ZY -T3","ZGD-T1","ZGD-T3") 
  
  P <- c("LJZ","CJF","ZAL","YSN","WWJ","SLS","YUWM" ,"WGF","SKA","YWM","YHS","XAH",
         "FDH","QHJ","QHR","ZFM","LNF","YHZ","ZGQ","ZGD",c("FDY","CCX","ZHS"))
  
  
  
  for (i in P) {
    ITC_freq_df <- ITC_freq_B %>% subset(.,patients_name==i)
    ITC_freq_df1 <- ITC_freq_df %>%
      group_by(sample_type,clone_size,change) %>% 
      dplyr::summarise(dplyr::across(freq, sum))
    ITC_freq_df1 <- as.data.frame(ITC_freq_df1) %>%
      dplyr::select(sample_type,change,freq)%>%
      mutate(change1=change)%>%
      separate(change1,into=c("changetype","fromtime"),sep = ":")%>%
      mutate(sample_type1=sample_type)%>%
      separate(sample_type1,into=c("sample_type2","sample_type3"),sep = "-") %>%   
      mutate(fromtime_from=paste(fromtime,sample_type3,sep = "_"))%>%
      dplyr::select(sample_type,change,freq,fromtime_from)
    
    colnames(ITC_freq_df1) <- c("sample_type","change","freq","fromtime_from")
    
    ITC_freq_df1$sample_type <- factor(ITC_freq_df1$sample_type,levels = c(paste(i,"WA",sep = "-"),paste(i,"WB",sep = "-"),
                                                                           paste(i,"WC",sep = "-"),paste(i,"T3",sep = "-")))
    
    
    ITC_freq_df1$change <- factor(ITC_freq_df1$change,levels = c(
      ">0.1%_Expanded_>0.1%:T1",
      "0.05%~0.1%_Expanded_>0.1%:T1","0.05%~0.1%_Expanded_0.05%~0.1%:T1",
      "<0.05%_Expanded_<0.05%:T1","<0.05%_Expanded_0.05%~0.1%:T1","<0.05%_Expanded_>0.1%:T1",
      
      ">0.1%_Expanded_>0.1%:WC",
      "0.05%~0.1%_Expanded_>0.1%:WC","0.05%~0.1%_Expanded_0.05%~0.1%:WC",
      "<0.05%_Expanded_<0.05%:WC","<0.05%_Expanded_0.05%~0.1%:WC","<0.05%_Expanded_>0.1%:WC",
      
      ">0.1%_Expanded_>0.1%:WB",
      "0.05%~0.1%_Expanded_>0.1%:WB","0.05%~0.1%_Expanded_0.05%~0.1%:WB",
      "<0.05%_Expanded_<0.05%:WB","<0.05%_Expanded_0.05%~0.1%:WB","<0.05%_Expanded_>0.1%:WB",
      
      ">0.1%_Expanded_>0.1%:WA",
      "0.05%~0.1%_Expanded_>0.1%:WA","0.05%~0.1%_Expanded_0.05%~0.1%:WA",
      "<0.05%_Expanded_<0.05%:WA","<0.05%_Expanded_0.05%~0.1%:WA","<0.05%_Expanded_>0.1%:WA"))
    ITC_freq_df1$fromtime <- factor(ITC_freq_df1$fromtime,levels =c("T1","WC","WB","WA"))
    ITC_freq_df1$fromtime_from <- factor(ITC_freq_df1$fromtime_from,levels =c("T1_T3","WC_T3","WB_T3","WA_T3",
                                                                              "T1_WC","WC_WC","WB_WC","WA_WC",
                                                                              "T1_WB","WC_WB","WB_WB","WA_WB",
                                                                              "T1_WA","WC_WA","WB_WA","WA_WA"))
    
    ITC_freq_df1  <- ITC_freq_df1[order(ITC_freq_df1$change),c(1:4)] 
    
    P_ITC_blood_Expanded_data <- rbind(P_ITC_blood_Expanded_data,ITC_freq_df1)
    P_ITC_blood_Expanded[[subset(cli_TCR,patients_name==i)$P]] <- 
      
      ggplot(data = ITC_freq_df1,aes(x = sample_type, y = freq, alluvium = change,stratum = fromtime_from)) +
      theme_bw() +
      geom_alluvium(aes(fill = fromtime_from,order = change),alpha = .75,width = 1/2) +
      geom_stratum(aes(stratum = fromtime_from,fill = fromtime_from,order = fromtime_from), width = 1/2,alpha = 0)+

      scale_fill_manual(values=c(
        
        "#005A32","#238B45" ,"#41AB5D","#A1D99B" ,
                 "#005A32","#238B45" ,"#41AB5D","#A1D99B" ,
                 "#005A32","#238B45" ,"#41AB5D","#A1D99B" ,
                 "#005A32","#238B45" ,"#41AB5D","#A1D99B" ,
                 rep("#005A32",6),
                 rep("#238B45" ,6),rep("#41AB5D" ,6),rep("#A1D99B" ,6)), 
        
        breaks=c(
          "T1_T3","WC_T3","WB_T3","WA_T3",
          "T1_WC","WC_WC","WB_WC","WA_WC",
          "T1_WB","WC_WB","WB_WB","WA_WB",
          "T1_WA","WC_WA","WB_WA","WA_WA",
          ">0.1%_Expanded_>0.1%:T1",
          "0.05%~0.1%_Expanded_>0.1%:T1","0.05%~0.1%_Expanded_0.05%~0.1%:T1",
          "<0.05%_Expanded_<0.05%:T1","<0.05%_Expanded_0.05%~0.1%:T1","<0.05%_Expanded_>0.1%:T1",
          
          ">0.1%_Expanded_>0.1%:WC",
          "0.05%~0.1%_Expanded_>0.1%:WC","0.05%~0.1%_Expanded_0.05%~0.1%:WC",
          "<0.05%_Expanded_<0.05%:WC","<0.05%_Expanded_0.05%~0.1%:WC","<0.05%_Expanded_>0.1%:WC",
          
          ">0.1%_Expanded_>0.1%:WB",
          "0.05%~0.1%_Expanded_>0.1%:WB","0.05%~0.1%_Expanded_0.05%~0.1%:WB",
          "<0.05%_Expanded_<0.05%:WB","<0.05%_Expanded_0.05%~0.1%:WB","<0.05%_Expanded_>0.1%:WB",
          
          ">0.1%_Expanded_>0.1%:WA",
          "0.05%~0.1%_Expanded_>0.1%:WA","0.05%~0.1%_Expanded_0.05%~0.1%:WA",
          "<0.05%_Expanded_<0.05%:WA","<0.05%_Expanded_0.05%~0.1%:WA","<0.05%_Expanded_>0.1%:WA"))+
      ggtitle(paste(subset(cli_TCR,patients_name==i)$P))+
      theme(legend.position = "none")+
      geom_text(aes(label = paste0(scales::percent(..count.., accuracy = .1))), stat = "stratum", size = 3)
    
  }
  
}


pdf(paste(dir,"P13_ITC_blood_Expanded.pdf",sep = ""),width=5, height=5)
P_ITC_blood_Expanded$P13
dev.off()



###################### P13_ITC_blood_Expanded ###################### 


###################### P13_PITC ###################### 

library(cowplot)
library(ggalluvial)
library(ggplot2)
library(scales)
library(ggfittext)
run <- "PITC"
PITC <- list()
PITC_data <- data.frame(sample_type=as.character(),change=as.character(),freq=as.numeric(),fromtime_from=as.character())
if(run=="PITC"){

  
  file_freq_name <- paste(dir,"TCR.Freq.RmNonProduct.csv",sep = "")
  
  cli_TCR <- read.table(paste(dir,"all.txt",sep = ""),header = T,row.names = 1,sep = "\t") %>%
    mutate(ESCC_ID=rownames(.)) %>%
    separate(ESCC_ID,into=c("ESCC","P"))
  
  
  
  ITC_setting <- function(file_freq_name,cli_TCR) {
    ITC_freq <- read.csv(file = file_freq_name)
    ITC_freq[is.na(ITC_freq)] <- 0
    ITC_freq1 <- subset(ITC_freq,T3>0)  %>%
      mutate(TCR_Type=
               case_when(T1 > 0 & T3 > 0 & T1 > T3  ~ "Contract",
                         T1 > 0 & T3 > 0 & T1 < T3 ~ "Expanded",
                         T1 == 0 & T3 > 0  ~ "T3only",
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
    ITC_freq_T1  <- ITC_freq1%>% dplyr::select(T1,sample,change,clone_size_T1)%>%
      mutate(sample_type=paste(sample,"T1",sep = "-"))
    colnames(ITC_freq_T1) <- c("freq","sample","change","clone_size","sample_type")
    ITC_freq_T3  <- ITC_freq1%>% dplyr::select(T3,sample,change,clone_size_T3)%>%
      mutate(sample_type=paste(sample,"T3",sep = "-"))
    colnames(ITC_freq_T3) <- c("freq","sample","change","clone_size","sample_type")
    ITC_freq_T <- rbind(ITC_freq_T1,ITC_freq_T3)%>%
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
    ITC_freq_df1 <- ITC_freq_df %>%
      group_by(sample_type,clone_size,change) %>% 
      dplyr::summarise(dplyr::across(freq, sum)) %>%
      mutate(change1=change)%>%
      separate(change1,into = c("T1","change_type","T3"),sep = "_")
    ITC_freq_df1 <- as.data.frame(ITC_freq_df1) %>%
      dplyr::select(sample_type,change,freq,T1,change_type)%>%
      mutate(T1_change_type=paste(T1,change_type,sep = "_")) %>%
      dplyr::select(sample_type,change,freq,T1_change_type)
    
    colnames(ITC_freq_df1) <- c("sample_type","change","freq","clone_size")
    
    
    ITC_freq_df1$sample_type <- factor(ITC_freq_df1$sample_type,levels = unique(ITC_freq_df1$sample_type))
    ITC_freq_df1$clone_size <- factor(ITC_freq_df1$clone_size,levels = c("0%_T3only",">0.1%_Expanded","0.05%~0.1%_Expanded","<0.05%_Expanded",
                                                                         "<0.05%_Contract","0.05%~0.1%_Contract",">0.1%_Contract"))
    ITC_freq_df1$change <- factor(ITC_freq_df1$change,levels = c(
      "0%_T3only_>0.1%","0%_T3only_0.05%~0.1%","0%_T3only_<0.05%",
      ">0.1%_Expanded_>0.1%",
      "0.05%~0.1%_Expanded_>0.1%","0.05%~0.1%_Expanded_0.05%~0.1%",
      "<0.05%_Expanded_<0.05%","<0.05%_Expanded_0.05%~0.1%","<0.05%_Expanded_>0.1%",
      "<0.05%_Contract_<0.05%",
      "0.05%~0.1%_Contract_0.05%~0.1%","0.05%~0.1%_Contract_<0.05%",
      ">0.1%_Contract_>0.1%",">0.1%_Contract_0.05%~0.1%",">0.1%_Contract_<0.05%"))
    ITC_freq_df1  <- ITC_freq_df1[order(ITC_freq_df1$change),c(1:4)]

    
    PITC_data <- rbind(PITC_data,ITC_freq_df1)
    PITC[[subset(cli_TCR,patients_name==i)$P]] <- 
      
      ggplot(data = ITC_freq_df1,
             aes(x = sample_type, y = freq, alluvium = change,stratum = clone_size)) +
      theme_bw() +
      scale_fill_brewer(type = "qual", palette ="Set3")+
      geom_alluvium(aes(fill = change),alpha = .75,width = 1/4) +
      geom_stratum(aes(stratum = clone_size,fill = clone_size,order = clone_size), alpha = 0,width = 1/4)+
      scale_fill_manual(values=c("#377EB8","#41AB5D" ,"#74C476","#A1D99B","#EF3B2C","#FB6A4A","#FC9272",
                                          rep("#377EB8",3),
                                          rep("#41AB5D" ,1),rep("#74C476",2),rep("#A1D99B",3),
                                          rep("#FC9272",1),rep("#FB6A4A",2),rep("#EF3B2C",3)), 
                        breaks=c(
                          "0%_T3only",
                          ">0.1%_Expanded","0.05%~0.1%_Expanded","<0.05%_Expanded",
                          ">0.1%_Contract","0.05%~0.1%_Contract","<0.05%_Contract",
                          
                          "0%_T3only_>0.1%","0%_T3only_0.05%~0.1%","0%_T3only_<0.05%",
                          
                          ">0.1%_Expanded_>0.1%",
                          "0.05%~0.1%_Expanded_>0.1%","0.05%~0.1%_Expanded_0.05%~0.1%",
                          "<0.05%_Expanded_<0.05%","<0.05%_Expanded_0.05%~0.1%","<0.05%_Expanded_>0.1%",
                          
                          "<0.05%_Contract_<0.05%",
                          "0.05%~0.1%_Contract_0.05%~0.1%","0.05%~0.1%_Contract_<0.05%",
                          ">0.1%_Contract_>0.1%",">0.1%_Contract_0.05%~0.1%",">0.1%_Contract_<0.05%"))+
      ggtitle(paste(subset(cli_TCR,patients_name==i)$P))+
      theme(legend.position = "none")+
      geom_text(aes(label = paste0(scales::percent(..count.., accuracy = .1))), stat = "stratum", size = 3)
    #geom_text(stat = "flow", nudge_x = 0.2) +
    
    
  }
  
}


pdf(paste(dir,"P13_all_ITCs.pdf",sep = ""),width=5, height=5)
PITC$P13
dev.off()


###################### P13_PITC ###################### 

