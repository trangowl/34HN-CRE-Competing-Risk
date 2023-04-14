##### Filter ineligible date data

library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(ggplot2)
library(Rmisc)
library(tidyr)
library(purrr)
library(shiny)
library(data.table)
library(stringi)
library(broom)
library(survival) #competing risk
library(mstate)
library(knitr)
library(tidycmprsk)
library(ggsurvfit)
library(cmprsk)
library(neatRanges)


## ##### Baseline 
baseline  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                        sheet="BASELINE") %>% 
  select(USUBJID,ENDATE,ADMISDATE,WARD, CLISYN) %>% 
  dplyr::rename("Date of enrollment"="ENDATE") %>% 
  mutate_at(vars(`Date of enrollment`, 
                 ADMISDATE),
            funs(as.Date(., "%d/%m/%Y")))

## antibiotic baseline
baseline_anti  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                             sheet="BASELINE_ANTI") %>%
  select(USUBJID,ANTIBIOTIC,ANTISTARTDATE,ANTIENDDATE) %>% 
  dplyr::rename("ANTIBIOTIC_Baseline"="ANTIBIOTIC",
                "ANTISTARTDATE_Baseline"="ANTISTARTDATE",
                "ANTIENDDATE_Baseline"="ANTIENDDATE")%>% 
  mutate_at(vars(ANTISTARTDATE_Baseline, 
                 ANTIENDDATE_Baseline),
            funs(as.Date(., "%d/%m/%Y")))%>% 
  mutate(anb_length_baseline=ANTIENDDATE_Baseline-ANTISTARTDATE_Baseline+1) 

##micro baseline
baseline_micr  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                             sheet="BASELINE_MICR") 

#### After enrollment-Discharge
dis  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                   sheet="DIS") %>%
  select(USUBJID,OUTCOME,DATEDIS,ICDCODE)%>% 
  mutate_at(vars(DATEDIS),
            funs(as.Date(., "%d/%m/%Y"))) 


dis_anti  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                        sheet="DIS_ANTI") %>%
  select(USUBJID,ANTIBIOTIC,ANTIBIOTICSTART,ANTIBIOTICEND) %>% 
  dplyr::rename("ANTIBIOTIC_Dis"="ANTIBIOTIC",
                "ANTISTARTDATE_Dis"="ANTIBIOTICSTART",
                "ANTIENDDATE_Dis"="ANTIBIOTICEND") %>% 
  mutate_at(vars(ANTISTARTDATE_Dis, 
                 ANTIENDDATE_Dis),
            funs(as.Date(., "%d/%m/%Y")))%>% 
  mutate(anb_length_dis=ANTIENDDATE_Dis-ANTISTARTDATE_Dis+1)


### Micro discharge
dis_micr  <- read_excel("C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                        sheet="DIS_DISMICR") 
## Merge all data
merged <- baseline %>% 
  left_join(., baseline_anti, by=c("USUBJID")) %>% 
  left_join(., dis, by=c("USUBJID")) %>% 
  left_join(., dis_anti, by=c("USUBJID")) %>% 
  distinct() %>% 
  mutate(ANB_to_EN=`Date of enrollment`-ANTISTARTDATE_Baseline,
         EN_to_ANB_Dis=ANTISTARTDATE_Dis-`Date of enrollment`,
         Baseline_anb_duration=ANTIENDDATE_Baseline-ANTISTARTDATE_Baseline,
         Dis_anb_duration= ANTIENDDATE_Dis-ANTISTARTDATE_Dis,
         EN_to_DIS = DATEDIS - `Date of enrollment`,
         Ad_to_DIS = DATEDIS - ADMISDATE,
         Ad_to_EN=`Date of enrollment`-ADMISDATE)

## Longer format
merged_long <- merged %>% 
  gather(., "Period", "Duration", 
         -c(USUBJID,WARD,CLISYN,ANTIBIOTIC_Baseline,anb_length_baseline,
            OUTCOME,ICDCODE,ANTIBIOTIC_Dis,anb_length_dis,`Date of enrollment`,
            ADMISDATE,ANTISTARTDATE_Baseline,ANTIENDDATE_Baseline,
            DATEDIS, ANTISTARTDATE_Dis, ANTIENDDATE_Dis))

## List of ineligible
merged_long_f_na <- merged_long %>% 
  select(USUBJID,Period,Duration) %>% 
  mutate(Period_interpret=case_when(Period=="ANB_to_EN"~"Baseline ANB start - Enrollment",
                                    Period=="EN_to_ANB_Dis"~"Enrollment - ANB Discharge",
                                    Period=="Baseline_anb_duration"~"Baseline ANB start - Baseline ANB end",
                                    Period=="Dis_anb_duration"~"Discharge ANB start - Discharge ANB end",
                                    Period=="EN_to_DIS"~"Enrollment - Discharge",
                                    Period=="Ad_to_DIS"~"Admission - Discharge",
                                    Period=="Ad_to_EN"~"Admission - Enrollment"))

## Filter Duration too long (>60 days) and negative value (after<before)
write.csv(merged_long_f_na %>% 
  filter(Duration >=60 | Duration<0|is.na(Duration)) %>% 
  distinct(), "34HN CRE List of patient id with ineligible duration.csv")

### Microbial dat
unique(baseline_micr$SPECIMEN)
unique(dis_micr$SPECIMEN)

baseline_micr_f <- baseline_micr %>% 
  mutate(SPECIMEN=stri_trans_general(SPECIMEN, id = "Latin-ASCII")) %>%
  mutate(SPECIMEN=casefold(SPECIMEN, upper = FALSE))%>%
  filter(!(SPECIMEN %in% c("nuoc tieu",
                           "dich nao tuy",
                           "dich ho hap",
                           "mau","dom",
                           "dich mang bung",
                           "dam","dich phe quan",
                           "catheter"))) %>% 
  select(USUBJID,SPECIMEN) %>% 
  distinct()
  
dis_micr_f <- dis_micr %>% 
  mutate(SPECIMEN=stri_trans_general(SPECIMEN, id = "Latin-ASCII")) %>%
  mutate(SPECIMEN=casefold(SPECIMEN, upper = FALSE))%>%
  filter(!(SPECIMEN %in% c("nuoc tieu","dich nao tuy",
                           "dich ho hap","mau","dom",
                           "dich mang bung","dam","dich phe quan",
                           "catheter","cay nuoc tieu",
                           "cong dam","dom`"))) %>% 
  select(USUBJID,SPECIMEN) %>% 
  distinct()

write.csv(rbind(baseline_micr_f,dis_micr_f), 
          "34HN CRE List of patient ineligible microbial specimen.csv")
