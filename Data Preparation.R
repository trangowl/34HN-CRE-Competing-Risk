
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
library(shiny)

input_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\"
output_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\Clean Data\\"

# function
readRDS2 <- function(object, file, ...) readRDS(object, file = paste0(input_folder, file), ...)

saveRDS2 <- function(object, file, ...) saveRDS(object, file = paste0(output_folder, file), ...)

add_folder <- function(file) paste0(input_folder, file)

reader_factory1 <- function(f) {
  function(file, ...) as_tibble(f(add_folder(file), header = TRUE, encoding = "UTF-8", ...))
}

reader_factory2 <- function(f) function(...) reader_factory1(f)(..., sep = ",")

read.csv2 <- reader_factory1(read.csv)

fread2 <- reader_factory2(fread)

read.table2 <- reader_factory2(read.table)

read_excel2 <- function(file, ...) read_excel(add_folder(file), ...)

### Anb category
anb_cat <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>% 
  read_excel2(sheet="Category ANB") %>% 
  filter(category =="ANTIBIO")

### Drug classification
drugc <- "List of antibiotic atc code, ingredient, group and subgroup.csv" %>% 
  read.csv2() %>% 
  select(ATC.code,ATC.level.name,Subgroup)

### Merge anb_cat and drug classification
anb_cat_m <- anb_cat %>% 
  left_join(., drugc, by=c("text"="ATC.level.name")) %>% 
  distinct()

## ##### Baseline 
baseline  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>% 
  read_excel2(sheet="BASELINE") %>% 
  select(USUBJID,ENDATE,ADMISDATE,WARD, CLISYN) %>% 
  dplyr::rename("Date of enrollment"="ENDATE") %>% 
  mutate_at(vars(`Date of enrollment`, 
                 ADMISDATE),
            funs(as.Date(., "%d/%m/%Y")))

## antibiotic baseline
baseline_anti  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>%
  read_excel2(sheet="BASELINE_ANTI") %>% 
  select(USUBJID,ANTIBIOTIC,ANTISTARTDATE,ANTIENDDATE) %>% 
  mutate_at(vars(ANTISTARTDATE, 
                 ANTIENDDATE),
            funs(as.Date(., "%d/%m/%Y")))%>% 
  mutate(Period="Before Enrollment")


#### After enrollment-Discharge
dis  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>%
  read_excel2(sheet="DIS") %>% 
  select(USUBJID,OUTCOME,DATEDIS,ICDCODE)%>% 
  mutate_at(vars(DATEDIS),
            funs(as.Date(., "%d/%m/%Y"))) 


dis_anti  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>%
  read_excel2(sheet="DIS_ANTI") %>% 
  select(USUBJID,ANTIBIOTIC,ANTIBIOTICSTART,ANTIBIOTICEND) %>% 
  dplyr::rename("ANTISTARTDATE"="ANTIBIOTICSTART",
                "ANTIENDDATE"="ANTIBIOTICEND") %>%
  mutate_at(vars(ANTISTARTDATE, 
                 ANTIENDDATE),
            funs(as.Date(., "%d/%m/%Y")))%>% 
  mutate(Period="After Enrollment")  

# combine baseline and discharge antibiotic
anti_dat <-  rbind(baseline_anti,dis_anti)%>% 
  left_join(., anb_cat_m, by=c("ANTIBIOTIC"="submissionvalue"))

baseline_micr  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>%
  read_excel2(sheet="BASELINE_MICR") %>% 
  select(USUBJID,COLLDATE,SPECIMEN,ORGANISM) %>% 
  mutate(Period="Before Enrollment")

dis_micr  <- "27-2-2023-_34HN_SS2_V1_Data.xlsx" %>%
  read_excel2(sheet="DIS_DISMICR") %>% 
  select(USUBJID,DATEOFCOLL,SPECIMEN,ORGANISM) %>% 
  dplyr::rename("COLLDATE"="DATEOFCOLL") %>% 
  mutate(Period="After Enrollment")

micr_dat <- rbind(baseline_micr, dis_micr) %>% 
  mutate_at(vars("COLLDATE"),
            funs(as.Date(., "%d/%m/%Y")))


## Merge all data
merged <- baseline %>% 
  left_join(., dis, by=c("USUBJID")) %>% 
  left_join(., anti_dat, by=c("USUBJID")) %>% 
  distinct() %>% 
  mutate(Period=relevel(as.factor(Period), ref="Before Enrollment")) %>% 
  mutate(USUBJID=as.factor(USUBJID)) %>% 
  left_join(., micr_dat, by=c("USUBJID", "Period")) %>% 
  distinct()
### join microdata
saveRDS2(merged, "Merged Data.rds")
saveRDS2(micr_dat, "Microbial Data.rds")
saveRDS2(anti_dat, "Antibiotic Data.rds")
