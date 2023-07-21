library(survminer)
library(gtsummary)
library(survival, lib.loc = "/usr/local/Cellar/r/4.2.1_3/lib/R/library")


#################### Extended Fig bc ####################

dir <- "/Users/yuanjingnan/Desktop/daily/20230208/extended3/extended3/"

SUR_nIT_OS <- read.csv(paste(dir,'patient_los25.csv',sep = "")) %>% 
  mutate(Treatment=case_when(neoadjuvant == "3" ~'nIT',neoadjuvant == "1" ~'nCT',neoadjuvant == "2" ~'nCRT'))%>% 
  mutate(treatment=case_when(Treatment == "nIT" ~"1",Treatment == "nCT" ~"0",Treatment == "nCRT" ~"0"))%>% 
  mutate(treatment =as.numeric(treatment)) %>%
  dplyr::filter(Treatment=="nIT")%>%
  mutate(Response_status=case_when(X.1 == "Well responders" ~'1',X.1 == "Poor responders" ~'0'))

survfit(Surv(OS_adj,survival_adj)~ X.1,data=SUR_nIT_OS) %>% tbl_survfit(times = c(12, 24), label = "Treatment") 
coxph(Surv(OS_adj,survival_adj) ~ X.1, data = SUR_nIT_OS) %>% gtsummary::tbl_regression(exp = TRUE) 
fit <- survfit(Surv(OS_adj,survival_adj)~ Treatment,data=SUR_nIT_OS)

pdf(paste(dir,"OS_nIT.pdf",sep = ""),width=5, height=8)
P1 <- ggsurvplot(fit,data=SUR_nIT_OS,pval=T,risk.table=T,
                 break.x.by = 6,break.y.by = 0.1,
                 legend.title='nIT (OS)',
                 surv.median.line = "hv",
                 size=2,
                 linetype="solid",
                 font.legend = c(15, "plain", "black"),
                 font.x = 12,
                 xlab = "Time since first treatment(months)", 
                 ylab = "Overall Survival",
                 risk.table.height = 0.5,
                 surv.scale = "percent",
                 risk.table.fontsize = 4,
                 font.main = 12,
                 risk.table.y.text.col = T,
                 risk.table.y.text = T)

print(P1)    
dev.off()



SUR_nIT_RFS <- read.csv(paste(dir,'patient_los25_RFS.csv',sep = "")) %>% 
  mutate(Treatment=case_when(neoadjuvant == "3" ~'nIT',neoadjuvant == "1" ~'nCT',neoadjuvant == "2" ~'nCRT'))%>% 
  mutate(treatment=case_when(Treatment == "nIT" ~"1",Treatment == "nCT" ~"0",Treatment == "nCRT" ~"0"))%>% 
  mutate(treatment =as.numeric(treatment)) %>%
  dplyr::filter(Treatment=="nIT")
SUR_nIT_RFS$Treatment <- factor(SUR_nIT_RFS$Treatment,levels = c("nCT","nCRT","nIT"))
survfit(Surv(RFS_adj,RFS_EVE_ADJ)~ Treatment,data=SUR_nIT_RFS) %>% tbl_survfit(times = c(12, 24), label = "Treatment") 
coxph(Surv(RFS_adj,RFS_EVE_ADJ) ~ treatment, data = SUR_nIT_RFS) %>% gtsummary::tbl_regression(exp = TRUE) 
fit <- survfit(Surv(RFS_adj,RFS_EVE_ADJ)~ Treatment,data=SUR_nIT_RFS)



pdf(paste(dir,'RFS_nIT.pdf',sep = ""),width=5, height=8)
P2 <- ggsurvplot(fit,data=SUR_nIT_RFS,pval=T,risk.table=T,
                 break.x.by = 6,break.y.by = 0.1,
                 legend.title='nIT (RFS)',
                 surv.median.line = "hv",
                 size=2,
                 linetype="solid",
                 font.legend = c(15, "plain", "black"),
                 font.x = 12,
                 xlab = "Time since first treatment(months)", 
                 ylab = "Recurrence free survival",
                 risk.table.height = 0.5,
                 surv.scale = "percent",
                 risk.table.fontsize = 4,
                 font.main = 12,
                 risk.table.y.text.col = T,
                 risk.table.y.text = T)

print(P2)    
dev.off()

#################### Extended Fig 2bc ####################


