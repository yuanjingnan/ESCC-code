library(glmnet)
library(ggpubr)
library(dplyr)

##training
val_list <- readRDS('Fig.3b.valid_list.rds')
ris <- read.csv('Fig.3b.tumor.residual.csv')
file <-  c("D:/BGI/ESCC/GSEA/ESCC/SYMBL_rmdup.rmFC0.GseaPreranked.1632966616794/HALLMARK_INTERFERON_GAMMA_RESPONSE.tsv",    
           "D:/BGI/ESCC/GSEA/ESCC/SYMBL_rmdup.rmFC0.GseaPreranked.1632966616794/HALLMARK_INTERFERON_ALPHA_RESPONSE.tsv",        
           "D:/BGI/ESCC/GSEA/ESCC/SYMBL_rmdup.rmFC0.GseaPreranked.1632966616794/HALLMARK_EPITHELIAL_MESENCHYMAL_TRANSITION.tsv")
name <- c("INF GAMMA","INF ALPHA","EMT")

a <- read.table(file[1],sep='\t',header=T) %>% filter(CORE.ENRICHMENT=='Yes',RANK.METRIC.SCORE>1)
b <- read.table(file[2],sep='\t',header=T) %>% filter(CORE.ENRICHMENT=='Yes',RANK.METRIC.SCORE>1)
c <- read.table(file[3],sep='\t',header=T) %>% filter(CORE.ENRICHMENT=='Yes',RANK.METRIC.SCORE< -1)
gene <- unique(c(a$SYMBOL,b$SYMBOL,c$SYMBOL))
set.seed(42)
count_log <- val_list[['ESCC_Log2']]
input <- count_log[count_log$hgnc_name %in% gene,]

row.names(input) <- input$hgnc_name
input <- input[,4:ncol(input)]
input <- input[,ris$sample]
a <- t(input)
b <- as.matrix(ris$score)

fit <- cv.glmnet(a,b,alpha=0.5,grouped = FALSE)
fit.ridge <-glmnet(x=a,y=b,alpha = 0.5,lambda = fit$lambda.min)
count_bgi_train_combine_count <- coef(fit.ridge)

#get formula 
num <- count_bgi_train_combine_count@x[-1]
g <- count_bgi_train_combine_count@Dimnames[[1]][-1] 
g <- g[count_bgi_train_combine_count@i]
f <- paste0("(",num,"¡¤",g,")")
formu <- paste(c(count_bgi_train_combine_count@x[1],f),collapse = '+')

#####
library(RColorBrewer)
mat <- val_list[['ESCC']]
set.seed(42)
num <- c()
res <- count_bgi_train_combine_count
gene <- names(res[2:nrow(res),])
gene <- intersect(gene,row.names(mat))
res <- res[row.names(res) %in% gene,]
#input <- mat[names(res),]
input <- log2(mat[names(res),]+1)
for (i in 1:ncol(input)){
  sample <- input[,i]
  n <- sum(res*sample)
  num <- c(num,n)
}
plot <- data.frame(sample=colnames(input),score=num)
plot <- merge(plot,val_list[['ESCC_cli']])
p <- ggboxplot(plot,x="group",y="score",add="jitter",fill="white",color="group",width=0.4)+scale_color_manual(values=brewer.pal(3,"Set1")[c(1,2)])+theme(legend.position = "none",axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),axis.title.y = element_text(size = 20))+xlab("")+ylab("Pathway score")+stat_compare_means(comparisons  = list(unique(plot$group)))
print(p)
