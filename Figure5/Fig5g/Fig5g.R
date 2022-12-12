library(plyr)
library(ggthemes)
library(rstatix)
library(EnhancedVolcano)
library(ggsignif)
ITC_count_read <- function(file_count_name) {
  data1 <- read.csv(file = file_count_name)
  data2 <- data1[,c("T1","T3","cdr3aa","sample")] %>% mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))
  data3 <- data2[rowSums(is.na(data2[,c("T1","T3")])) %in%c(1,0),]
  rownames(data3) <- data3$cdr3aa_sample
  data31 <- data2[rowSums(is.na(data2[,c("T1","T3")])) %in%c(0),]
  

  all_P_data <- data.frame(cdr3aa=as.character(),sample=as.character(),Pvalue=as.character())
  for (i in unique(data31$sample)) {
    print(i)
    data4 <- data3 %>% dplyr::filter(sample==i)
    data41 <- data4

    data4[is.na(data4)] <- 0
    data5 <- data41[rowSums(is.na(data41[,c("T1","T3")])) ==0,]
    T1_sum <- sum(data4[,1])
    T3_sum <- sum(data4[,2])
    ratio <- T1_sum/T3_sum
    for (k in 1:dim(data5)[1]) {
      mat <- matrix(data=c(data5[k,1],T1_sum,data5[k,2]*ratio,T3_sum*ratio), ncol=2)
      
      f <- fisher.test(as.table(mat), 
                       alternative = "two.sided")
      all_P_data_1 <- data.frame(cdr3aa=data5[k,3],sample=data5[k,4],Pvalue=f$p.value,
                                 Foldchange=log2((data5[k,2]+1)*ratio/(data5[k,1]+1)))
      all_P_data <- rbind(all_P_data,all_P_data_1)
    }
    
  }
  all_P_data
}
EnhancedVolcano_plot <- function(TCR_expand_regress,i) {
  person_TCR <- TCR_expand_regress[which(TCR_expand_regress$P==i),]
  min <- sort(person_TCR$Pvalue[person_TCR$Pvalue!=0])[2]
  person_TCR[which(person_TCR$Pvalue==0),"Pvalue"] <- min
  keyvals <- ifelse(person_TCR$TCR_Type == "Expended", '#9cce96',
                    ifelse(person_TCR$TCR_Type == "Contract", '#bf3d2e',
                           '#D9D9D9'))
                           keyvals[is.na(keyvals)] <- '#D9D9D9'
                             names(keyvals)[keyvals == '#9cce96'] <- 'Expanded'
                             names(keyvals)[keyvals == '#bf3d2e'] <- 'Contracted'
                             names(keyvals)[keyvals == '#D9D9D9'] <- 'NS'
                             P1 <- EnhancedVolcano(person_TCR,
                                                   subtitle = NULL,
                                                   lab = person_TCR$cdr3aa,
                                                   pCutoff = 0.05,
                                                   FCcutoff = 1,
                                                   title = paste(i),
                                                   caption = NULL,
                                                   x = "Foldchange",
                                                   y = "Pvalue",
                                                   xlab = bquote(~Log[2]~ 'fold change'),
                                                   ylab = bquote(~-Log[10]~italic(P)),
                                                   colAlpha = 1,
                                                   pointSize = 0.5,
                                                   colCustom = keyvals,
                                                   selectLab = "",
                                                   gridlines.major=FALSE, gridlines.minor=FALSE,

                             )+theme(legend.position = "none")
                             P1
}

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure5/Fig5g/"

cli_TCR <- read.table(paste(dir,"all.txt",sep = ""),header = T,row.names = 1,sep = "\t") %>%
  mutate(ESCC_ID=rownames(.)) %>%
  separate(ESCC_ID,into=c("ESCC","P"))
rownames(cli_TCR) <- cli_TCR$patients_name
cli_TCR$sample <- cli_TCR$patients_name


file_count_name <- paste(dir,"TCR.Count.RmNonProduct.csv",sep = "")
file_freq_name <- paste(dir,"TCR.Freq.RmNonProduct.csv",sep = "")



all_P_data <- ITC_count_read(file_count_name)%>% mutate(cdr3aa_sample=paste(cdr3aa,sample,sep = "_"))



TCR_expand_regress <- all_P_data %>% mutate(TCR_Type=case_when(Pvalue < 0.05 & Foldchange > 1 ~ "Expended",
                                                               Pvalue < 0.05 & Foldchange < -1 ~ "Contract",
                                                               TRUE ~ "NS"))%>% mutate(patients_name=sample)%>%left_join(cli_TCR,by="patients_name")

P2 <- list()


pdf(paste(dir,"TCR.allchange.pdf",sep = ""),width=3, height=3)
pre_existing_list_filter <-c("P13","P17")
for (i in pre_existing_list_filter) {
  P2[[i]] <- EnhancedVolcano_plot(TCR_expand_regress,i)
  print(P2[[i]])
}


dev.off()

