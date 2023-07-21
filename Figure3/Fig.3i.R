


library(survminer)
library(survival)



################### TCGA-Asian-Survival #####################
mrt_ASIAN = read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3i/Fig3i.ASIAN.txt",sep = "\t")
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

mrt_WHITE = read.table(file = "/Users/yuanjingnan/Desktop/daily/20230602/figure-20230602/code/Figure3/Fig3i/Fig3i.WHITE.txt",sep = "\t")

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


