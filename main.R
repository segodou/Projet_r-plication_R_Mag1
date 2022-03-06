library(tidyverse)
library(haven)
library(ipumsr)
library(foreign)
library(sandwich)   # for: vcovCL
library("AER")      # for: ivreg
library("lmtest")   # for: coeftest
library(stargazer)
library(lubridate)
library(ggplot2)

path = "G:/Nouveau/SaiRstudio/project/"
pums <- read.table(paste(path,"database/QOB.raw",sep=""),
                   header           = FALSE,
                   stringsAsFactors = FALSE)
colnames(pums)[c(1,2,4,5,6,9:13,16,18,19:21,24,25,27)] <- c("AGE", "AGEQ", "EDUC",
                                              "ENOCENT","ESOCENT", "LWKLYWGE", 
                      "MARRIED", "MIDATL", "MT", "NEWENG", "CENSUS", "QOB", "RACE",
                        "SMSA", "SOATL", "WNOCENT", "WSOCENT", "YOB")
pums <- as_tibble(pums)
pums

pums %>%
  mutate(cohort = factor(1*(YOB<=39 & YOB >=30) +
                           2*(YOB<=49 & YOB >=40),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> pums

# CrÃ©eons une nouvelle base de donnÃ©e
pums %>%
  select(AGE, AGEQ, EDUC,
         ENOCENT,ESOCENT, LWKLYWGE, 
         MARRIED, MIDATL, MT, NEWENG, CENSUS, QOB, RACE,
         SMSA, SOATL, WNOCENT, WSOCENT, YOB, cohort) -> mydata

mydata %>%
  mutate(date=ymd(paste("19",YOB,QOB * 3, sep=""),truncated = 2)) ->
  mydata

# Figure 2 
mydata %>%
  group_by(date, QOB) %>%
  summarise(moyenne = mean(EDUC, na.rm=TRUE)) -> temp
temp %>% 
  mutate(QOB_factor = factor(QOB)) -> temp

ggplot(data=temp, aes(x=date , y=moyenne)) +
  geom_line()+
  geom_point(aes(col=QOB_factor), size = 7)+
  scale_color_manual(values = c("#DC101C", "#181616", "#181616", "#181616")) +
  geom_text(aes(label=QOB_factor), hjust=0.5, vjust=0.5, col="white")+
  theme(legend.position = "None")+
  ggtitle("Average education by quarter of birth")+
  xlab("Year of birth") + ylab("Years of education")

# Figure 5
mydata %>%
  group_by(date, QOB) %>%
  summarise(moyen = mean(LWKLYWGE, na.rm=TRUE)) -> temp1
temp1 %>% 
  mutate(QOB_factor = factor(QOB)) -> temp1

ggplot(data=temp1, aes(x=date , y=moyen, shape=)) +
  geom_line()+
  geom_point(aes(col=QOB_factor), size = 7)+
  scale_color_manual(values = c("#DC101C", "#181616", "#181616", "#181616")) +
  geom_text(aes(label=QOB_factor), hjust=0.5, vjust=0.5, col="white")+
  theme(legend.position = "None")+
  ggtitle("Average weekly wage by quarter of birth")+
  xlab("Year of birth") + ylab("Log weekly earnings")

# Tableau 

mydata %>%
  filter(cohort == "30-39") -> mydata.tab5

# Régression
reg1.MCO <- lm(LWKLYWGE ~ EDUC + factor(YOB), data=mydata.tab5)
summary(reg1.MCO)

mydata.tab5 %>%
  mutate( instru = factor(YOB):factor(QOB) )-> mydata.tab5
reg2.TSLS <- ivreg(LWKLYWGE ~ EDUC + factor(YOB) | instru, data=mydata.tab5)
summary(reg2.TSLS)

reg3.MCO <- lm(LWKLYWGE ~ EDUC + factor(YOB)+ AGEQ + I(AGEQ^2), data=mydata.tab5)
summary(reg3.MCO)

reg4.TSLS <- ivreg(LWKLYWGE ~ EDUC + factor(YOB) + AGEQ + I(AGEQ^2) | instru +
                     AGEQ + I(AGEQ^2), data=mydata.tab5)
summary(reg4.TSLS)

mydata.tab5 %>%
  mutate( region_of_residence = factor(NEWENG + MIDATL + ENOCENT + 
                                      WNOCENT + SOATL + ESOCENT + 
                                      WSOCENT + MT)) -> mydata.tab5
reg5.MCO <- lm(LWKLYWGE ~ EDUC + RACE  + SMSA + MARRIED + factor(YOB) + 
                 region_of_residence, data=mydata.tab5)
summary(reg5.MCO)

reg6.TSLS <- ivreg(LWKLYWGE ~ EDUC + RACE  + SMSA + MARRIED + factor(YOB) +
                     region_of_residence | instru + RACE  + SMSA + 
                     MARRIED + factor(YOB) + region_of_residence, data=mydata.tab5)
summary(reg6.TSLS)

reg7.MCO <- lm(LWKLYWGE ~ EDUC + RACE  + SMSA + MARRIED + factor(YOB) + 
                 region_of_residence +AGEQ + I(AGEQ^2), data=mydata.tab5)
summary(reg7.MCO)

reg8.TSLS <- ivreg(LWKLYWGE ~ EDUC + RACE  + SMSA + MARRIED + factor(YOB) +
                     region_of_residence + AGEQ + I(AGEQ^2) | instru + RACE  + SMSA + 
                     MARRIED + AGEQ + I(AGEQ^2) + factor(YOB) +
                     region_of_residence , data=mydata.tab5)
summary(reg8.TSLS)

# Construction tableau
stargazer(reg1.MCO, reg2.TSLS, reg3.MCO, reg4.TSLS, reg5.MCO, reg6.TSLS, reg7.MCO,
          reg8.TSLS,
          dep.var.caption="",dep.var.labels="",
          omit.table.layout = "n", star.cutoffs = NA,keep.stat=c("rsq","n"),no.space=TRUE,
          header=FALSE,
          keep=c("EDUC","RACE", "SMSA", "MARRIED", "AGE", "AGEQ^2"),
          column.labels=c("OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS"),
          title="OLS and TSLS of the return to education for men born 1390-1939: 1980 CENSUS", type="text"
)


# Wald estimation 
## generate the dummy variable (for 3.and 4. quarter) 
mydata.tab5$wald_dum <- (mydata.tab5$QOB == 2 | mydata.tab5$QOB == 3 | mydata.tab5$QOB == 4) * 1 


mydata.tab5 %>%
  group_by(wald_dum == 0) %>%
  summarize(moy_salaire =  mean(LWKLYWGE, na.rm=TRUE), 
            moy_educ = mean(EDUC, na.rm=TRUE)) %>%
  ungroup() -> temp3

# Crée un tableau
temp2 <- t(data.matrix(temp3[,-1]))
colnames(temp2) <- unlist(lapply(temp3$`wald_dum == 0`), as.character)
knitr::kable(temp2, digits=3,  
             caption="Mean characteristics")
temp3$moy_educ[2]

# Calcul de l'estimateur de wald
temp3 %>%
  mutate(wage_Q1 = moy_salaire[2]) %>%
  mutate(wage_Q2_4 = moy_salaire[1]) %>%
  mutate(educ_Q1 = moy_educ[2]) %>%
  mutate(educ_Q2_4 = moy_educ[1]) %>%
  mutate(wald_est = (wage_Q1 - wage_Q2_4)/(educ_Q1 - educ_Q2_4)) %>%
  mutate(wage_Q1 = NULL) %>%
  mutate(wage_Q2_4 = NULL) %>%
  mutate(educ_Q1 = NULL) %>%
  mutate(educ_Q2_4 = NULL) ->temp3

temp3$wald_est[1]


# Question 3 ipums

ddi_ipums   <- read_ipums_ddi(paste(path,"database/usa_00002.xml",sep=""))
mydata_ipums <- read_ipums_micro(ddi_ipums, data_file =
                            paste(path,"database/usa_00002.dat",sep=""))

#Pour rendre la base plus utilisable, on supprime les lignes pour lesquels
# incwage et wkswork1 égal 0, sinon quand on applique log on obtient des na, nan et inf
# ce qui rend les choses compliqués pour la suite
mydata_ipums %>%
  filter(INCWAGE > 0) %>%
  filter(WKSWORK1 > 0) -> mydata_ipum
# voir exercice 2 sur ipums pour l'explication sur le code copié.
mydata_ipum %>%
  mutate(LWKLYWGE = log(INCWAGE / WKSWORK1)) %>%
  mutate(INCWAGE:=NULL,WKSWORK1:=NULL) %>%
  mutate(EDUC = NA) %>%
  mutate(EDUC = ifelse(EDUCD == 14, 1, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 15, 2, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 16, 3, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 17, 4, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 22, 5, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 23, 6, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 25, 7, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 26, 8, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 30, 9, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 40, 10, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 50, 11, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 60, 12, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 65 | EDUCD == 70, 13, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 80, 14, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 90, 15, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 100, 16, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 110, 17, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 111, 18, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 112, 19, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD >= 113, 20, EDUC)) %>%
  mutate(EDUCD := NULL) ->
  mydata2_ipums

mydata2_ipums %>%
  mutate(cohort = factor(1*(BIRTHYR<=1939 & BIRTHYR >=1930) +
                           2*(BIRTHYR<=1949 & BIRTHYR >=1940),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> mydata2_ipums


# mydata2_ipums %>% filter(!is.na(LWKLYWGE))
# mydata2_ipums %>% filter(!is.nan(LWKLYWGE))
# mydata2_ipums %>% filter(!is.inf(LWKLYWGE))

# Figure 2 avec les données ipums 
mydata2_ipums %>%
  group_by(BIRTHYR, BIRTHQTR) %>%
  summarise(moyenne = mean(EDUC, na.rm=TRUE)) -> temp_ipums
temp_ipums %>% 
  mutate(BIRTHQTR_factor = factor(order(BIRTHQTR))) -> temp_ipums

ggplot(data=temp_ipums %>% filter(BIRTHYR >= 1930 & BIRTHYR <= 1950), aes(x=BIRTHYR , y=moyenne)) +
  geom_line()+
  geom_point(aes(col=BIRTHQTR_factor), size = 7)+
  scale_color_manual(values = c("#DC101C", "#181616", "#181616", "#181616")) +
  geom_text(aes(label=BIRTHQTR_factor), hjust=0.5, vjust=0.5, col="white")+
  theme(legend.position = "None")+
  ggtitle("Average education by quarter of birth with ipums USA data")+
  xlab("Year of birth") + ylab("Years of education")

# Figure 5
mydata2_ipums %>%
  group_by(BIRTHYR, BIRTHQTR) %>%
  summarise(moyenne = mean(LWKLYWGE, na.rm=TRUE)) -> temp1_ipums
temp1_ipums %>% 
  mutate(BIRTHQTR_factor = factor(order(BIRTHQTR))) -> temp1_ipums

ggplot(data=temp1_ipums  %>% filter(BIRTHYR >= 1930 & BIRTHYR <= 1950), aes(x=BIRTHYR , y=moyenne)) +
  geom_line()+
  geom_point(aes(col=BIRTHQTR_factor), size = 7)+
  scale_color_manual(values = c("#DC101C", "#181616", "#181616", "#181616")) +
  geom_text(aes(label=BIRTHQTR_factor), hjust=0.5, vjust=0.5, col="white")+
  theme(legend.position = "None")+
  ggtitle("Average weekly wage by quarter of birth with ipums USA data")+
  xlab("Year of birth") + ylab("Log weekly earnings")

# Tableau 

mydata2_ipums %>%
  filter(cohort == "30-39") -> mydata3_ipums

# Régression : n'ayant pas les autres variables intruments, on ne fera que les 4 premiers 
# estimation

reg1.MCO <- lm(LWKLYWGE ~ EDUC + factor(BIRTHYR), data=mydata3_ipums)
summary(reg1.MCO)

mydata3_ipums %>%
  mutate( instru = factor(BIRTHYR):factor(BIRTHQTR) )-> mydata3_ipums
reg2.TSLS <- ivreg(LWKLYWGE ~ EDUC + factor(BIRTHYR) | instru, data=mydata3_ipums)
summary(reg2.TSLS)

reg3.MCO <- lm(LWKLYWGE ~ EDUC + factor(BIRTHYR)+ AGE + I(AGE^2), data=mydata3_ipums)
summary(reg3.MCO)

reg4.TSLS <- ivreg(LWKLYWGE ~ EDUC + factor(BIRTHYR) + AGE + I(AGE^2) | instru +
                     AGE + I(AGE^2), data=mydata3_ipums)
summary(reg4.TSLS)

# Construction tableau
stargazer(reg1.MCO, reg2.TSLS, reg3.MCO, reg4.TSLS,
          dep.var.caption="",dep.var.labels="",
          omit.table.layout = "n", star.cutoffs = NA,keep.stat=c("rsq","n"),no.space=TRUE,
          header=FALSE,
          keep=c("EDUC", "AGE", "AGE^2"),
          column.labels=c("OLS", "TSLS", "OLS", "TSLS"),
          title="OLS and TSLS of the return to education for men born 1390-1939: 1980 CENSUS", type="text"
)

# Wald estimation 
## generate the dummy variable (for 3.and 4. quarter) 
mydata3_ipums$wald_dum <- (mydata3_ipums$BIRTHQTR == 2 | mydata3_ipums$BIRTHQTR == 3 
                           | mydata3_ipums$BIRTHQTR == 4) * 1 

mydata3_ipums %>%
  group_by(wald_dum == 0) %>%
  summarize(moy_salaire =  mean(LWKLYWGE, na.rm=TRUE), 
            moy_educ = mean(EDUC, na.rm=TRUE)) %>%
  ungroup() -> temp4

# Crée un tableau
temp2 <- t(data.matrix(temp4[,-1]))
colnames(temp2) <- unlist(lapply(temp3$`wald_dum == 0`), as.character)
knitr::kable(temp2, digits=3,  
             caption="Mean characteristics")
temp4$moy_educ[2]
#autre methode
(data <-
    data.frame(
      "x" = c( "Moy_salaire", "Moy_educ"),
      "1st quarter" = c(temp4$moy_salaire[2], temp4$moy_educ[2]),
      "3rd or 4th" = c(temp4$moy_salaire[1], temp4$moy_educ[1])
    )
)

# Calcul de l'estimateur de wald
temp4 %>%
  mutate(wage_Q1 = moy_salaire[2]) %>%
  mutate(wage_Q2_4 = moy_salaire[1]) %>%
  mutate(educ_Q1 = moy_educ[2]) %>%
  mutate(educ_Q2_4 = moy_educ[1]) %>%
  mutate(wald_est = (wage_Q1 - wage_Q2_4)/(educ_Q1 - educ_Q2_4)) %>%
  mutate(wage_Q1 = NULL) %>%
  mutate(wage_Q2_4 = NULL) %>%
  mutate(educ_Q1 = NULL) %>%
  mutate(educ_Q2_4 = NULL) ->temp4

temp4$wald_est[1]

(data <-
    data.frame(
       "x" = c( "Moy_salaire", "Moy_educ"),
      "1st quarter" = c(temp3$moy_salaire[2], temp3$moy_educ[2]),
      "3rd or 4th" = c(temp3$moy_salaire[1], temp3$moy_educ[1])
    )
)
