


library(survminer)
library(survival)

dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure3/fig3i/"

load(paste(dir,"TCGA_ESCC.full.cli.rda",sep = ""),verbose = T)#TCGA_cli



################### TCGA-Asian-Survival #####################
a <- TCGA_cli %>% 
  mutate(Subtype=case_when(sample %in% TP ~ "TP",sample %in% IE ~ "IE",sample %in% FE ~ "FE",sample %in% TPF ~ "TPF",TRUE ~ "filter")) %>%
  mutate(Race=case_when(race=="ASIAN" ~ "ASIAN",race=="BLACK OR AFRICAN AMERICAN" ~ "BLACK",race=="WHITE" ~ "WHITE",TRUE ~ "Unknown")) %>%
  dplyr::filter(sample%in%c(TP,FE,IE,TPF)) %>% 
  dplyr::filter(race%in%c("ASIAN","WHITE")) %>% mutate(Subtype_IE=case_when(Subtype=="IE"~ "IE",Subtype!="IE"~ "non_IE"))%>% 
  mutate(Subtype_Race_IE=paste(Race,Subtype_IE,sep = "_"))%>% 
  mutate(Subtype_3=case_when(sample %in% TP ~ "TP",sample %in% IE ~ "IE",sample %in% FE ~ "FE+TPF",sample %in% TPF ~ "FE+TPF",TRUE ~ "filter")) 
mrt_ASIAN <- a %>% dplyr::filter(race=="ASIAN") 
survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_ASIAN) %>% tbl_survfit(times = c(365,730,1095), label = "Treatment") 
followup_time(Surv(OS.time,OS), data = mrt_ASIAN)
survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_ASIAN) %>% surv_median(.)

P <- survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_ASIAN) %>%
  ggsurvplot(.,data=mrt_ASIAN,pval=T,risk.table=T,surv.median.line = "hv",
             size=1,linetype="solid",ggtheme = theme_classic(),
             palette =c("#22628e", "#cf7e45"),# custom color palettes
             risk.table.height = 0.3,surv.scale = "percent", 
             risk.table.fontsize = 4,
             font.main = 12,
             risk.table.y.text.col = T,
             risk.table.y.text = T)



pdf(paste(dir,"TCGA-Asian-Survival.pdf",sep = ""),width=5, height=5)
print(P)   
dev.off()

################### TCGA-Asian-Survival #####################




################### TCGA-White-Survival #####################
a <- TCGA_cli %>% 
  mutate(Subtype=case_when(sample %in% TP ~ "TP",sample %in% IE ~ "IE",sample %in% FE ~ "FE",sample %in% TPF ~ "TPF",TRUE ~ "filter")) %>%
  mutate(Race=case_when(race=="ASIAN" ~ "ASIAN",race=="BLACK OR AFRICAN AMERICAN" ~ "BLACK",race=="WHITE" ~ "WHITE",TRUE ~ "Unknown")) %>%
  dplyr::filter(sample%in%c(TP,FE,IE,TPF)) %>% 
  dplyr::filter(race%in%c("ASIAN","WHITE")) %>% mutate(Subtype_IE=case_when(Subtype=="IE"~ "IE",Subtype!="IE"~ "non_IE"))%>% 
  mutate(Subtype_Race_IE=paste(Race,Subtype_IE,sep = "_"))%>% 
  mutate(Subtype_3=case_when(sample %in% TP ~ "TP",sample %in% IE ~ "IE",sample %in% FE ~ "FE+TPF",sample %in% TPF ~ "FE+TPF",TRUE ~ "filter")) 
mrt_WHITE <- a %>% dplyr::filter(race=="WHITE") 
survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_WHITE) %>% tbl_survfit(times = c(365,730,1095), label = "Treatment") 
followup_time(Surv(OS.time,OS), data = mrt_WHITE)
survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_WHITE) %>% surv_median(.)

P <- survfit(Surv(OS.time,OS) ~ Subtype_IE, data = mrt_WHITE) %>%
  ggsurvplot(.,data=mrt_WHITE,pval=T,risk.table=T,surv.median.line = "hv",
             size=1,linetype="solid",ggtheme = theme_classic(),
             palette =c("#22628e", "#cf7e45"),# custom color palettes
             risk.table.height = 0.3,surv.scale = "percent", 
             risk.table.fontsize = 4,
             font.main = 12,
             risk.table.y.text.col = T,
             risk.table.y.text = T)



pdf(paste(dir,"TCGA-White-Survival.pdf",sep = ""),width=5, height=5)
print(P)   
dev.off()

################### TCGA-White-Survival #####################


