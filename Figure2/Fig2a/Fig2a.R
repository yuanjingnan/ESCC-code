
library(dplyr)



dir <- "/Users/yuanjingnan/Desktop/daily/20221212/ESCC材料汇总.20221212/figure/Figure/Figure2/Fig2a/"


AE <- read.table(file = paste(dir,"AE.txt",sep = ""),header = T,sep = "\t") 

df_nCRT_vs_nIT <- AE[which(AE$Group=='nCRT_vs_nIT'),c("Events","Values","Grade","actual_values","Grade_com") ] %>%
  mutate(Values_1=Values) %>%
  mutate(Values_1=case_when(Values == 0 ~ "",TRUE ~ as.character(round(Values,1))))
df_nCRT_vs_nIT$Events <- factor(df_nCRT_vs_nIT$Events, 
                                levels = rev(c("Vomiting","Granulocytopenia","Anorexia","Anemia","Thrombocytopenia",
                                               "Liver dysfunction",'Fatigue',"Constipation","Diarrhea","Fever without infection",
                                               "Pneumonia","Radiation esophagitis","Esophageal perforation","Any")))
df_nCRT_vs_nIT$Grade_com <- factor(df_nCRT_vs_nIT$Grade_com,levels = c("Grade 1-2","Grade >3"))



df_nCT_vs_nIT <- AE[which(AE$Group=='nCT_vs_nIT'),c("Events","Values","Grade","actual_values","Grade_com") ]%>%
  mutate(Values_1=Values) %>%
  mutate(Values_1=case_when(Values == 0 ~ "",TRUE ~ as.character(round(Values,1))))

df_nCT_vs_nIT$Events <- factor(df_nCT_vs_nIT$Events, 
                               levels = rev(c("Vomiting","Granulocytopenia","Anorexia","Anemia","Thrombocytopenia",
                                              "Liver dysfunction",'Fatigue',"Constipation","Diarrhea","Fever without infection",
                                              "Pneumonia","Radiation esophagitis","Esophageal perforation","Any")))
df_nCT_vs_nIT$Grade_com <- factor(df_nCT_vs_nIT$Grade_com,levels = c("Grade 1-2","Grade >3"))



P1 <- ggplot(df_nCRT_vs_nIT, aes(x = Events, y = ifelse(Grade %in% c("G12","G34"), -Values, Values), fill = Grade_com)) + 
  geom_col() +
  coord_flip()+
  theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.text.x = element_text(size = 12,colour = "black"),axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12,colour = "black"),axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        #legend.text = element_text(colour="blue", size=20),
        axis.line.x = element_line(colour = "black"))+
  scale_fill_manual(values=c("#3274A4", "#9D2831"))+
  geom_text(aes(label = Values_1,
                colour=Grade_com,
                y = ifelse(Grade %in% c("G12","G34"), -actual_values, actual_values),
                angle = 0,
                hjust = ifelse(Grade %in% c("G12","G34"), 1.25, -0.25),
                vjust = 0.5),
            size = 4) +
  scale_colour_manual(values=c( "black","white"))+ 
  ylab("Incidence(%)") + 
  xlab("Events")+
  ylim(-100, 100)+
  scale_y_continuous(breaks=seq(-100,100,20),labels=paste(c(rev(seq(0,100,20)),seq(20,100,20)),sep = ""))+ 
  guides(fill=guide_legend(title=""))


P2 <- ggplot(df_nCT_vs_nIT, 
             aes(x = Events, y = ifelse(Grade %in% c("G12","G34"), -Values, Values), fill = Grade_com)) + 
  geom_col() +
  coord_flip()+
  theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.text.x = element_text(size = 12,colour = "black"),axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12,colour = "black"),axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        #legend.text = element_text(colour="blue", size=20),
        axis.line.x = element_line(colour = "black"))+
  scale_fill_manual(values=c("#3274A4", "#9D2831"))+
  geom_text(aes(label = Values_1,
                colour=Grade_com,
                y = ifelse(Grade %in% c("G12","G34"), -actual_values, actual_values),
                angle = 0,
                hjust = ifelse(Grade %in% c("G12","G34"), 1.25, -0.25),
                vjust = 0.5),
            size = 4) +
  scale_colour_manual(values=c( "black","white"))+ 
  ylab("Incidence(%)") + 
  xlab("Events")+
  ylim(-100, 100)+
  scale_y_continuous(breaks=seq(-100,100,20),labels=paste(c(rev(seq(0,100,20)),seq(20,100,20)),sep = ""))+ 
  guides(fill=guide_legend(title=""))

pdf(paste(dir,"AE.pdf",sep = ""),width=8, height=4)
print(P1)
print(P2)
dev.off()




