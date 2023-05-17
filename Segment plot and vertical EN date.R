
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
library(ggforce)
library(scales)
library(janitor)
library(ggnewscale)
## function to import data
input_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\"
output_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\Clean Data\\"

readRDS2 <- function(object, file, ...) readRDS(object, file = paste0(input_folder, file), ...)


## Frequency to determine top subgroup within each group
merged <- "Merged Data.rds" %>% 
  readRDS2()

prop_tab_by_group <- merged %>%
  filter(!is.na(Subgroup)) %>% 
  select(USUBJID,Subgroup, CLISYN,text) %>% 
  unique() %>% 
  tabyl(Subgroup,CLISYN) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns("front")


CRE_top10_subg <- prop_tab_by_group %>%                                      
  arrange(desc(`1`)) %>% 
  slice(1:10) 

CSE_top10_subg <- prop_tab_by_group %>%                                      
  arrange(desc(`2`)) %>% 
  slice(1:10) 

NE_top10_subg <- prop_tab_by_group %>%                                      
  arrange(desc(`3`)) %>% 
  slice(1:10) 

merged <- merged %>% 
  mutate(CRE_subgroup=ifelse(Subgroup %in% CRE_top10_subg$Subgroup,
                             Subgroup, "Other"),
         CSE_subgroup=ifelse(Subgroup %in% CSE_top10_subg$Subgroup,
                             Subgroup, "Other"),
         NE_subgroup=ifelse(Subgroup %in% NE_top10_subg$Subgroup,
                             Subgroup, "Other"))
## Data of anb period
##### CLINSYN=1

CRE <- merged %>%
  filter(!is.na(Period) | !is.na(ANTISTARTDATE)| !is.na(ANTIENDDATE)) %>%
  filter(CLISYN=="1") %>% 
  group_by(USUBJID) 

pdf(paste0(output_folder, "plots CRE.pdf"), 
    onefile = TRUE,
    width=12,height=15,
    
    pointsize=12)

for(i in 1:8){  
ggplot(CRE,
           aes(x = ANTISTARTDATE-ADMISDATE,
               y = CRE_subgroup)) +
    facet_wrap_paginate(~ USUBJID, 
                        ncol = 6, nrow = 6, page = i,
                        scales="free_x")+
    geom_segment(aes(
      x = ANTISTARTDATE-ADMISDATE,
      y = CRE_subgroup ,
      xend = ANTIENDDATE-ADMISDATE,
      yend = CRE_subgroup,
      color = CRE_subgroup
    ),
    linewidth=1.5) +
    #geom_line(data=...aes(x=.., y=..))+
    
    geom_vline(data=CRE,
               aes(xintercept = `Date of enrollment`-ADMISDATE),
               color="red"
               )+
    geom_vline(data=CRE,
               aes(xintercept = COLLDATE-ADMISDATE),
               linetype="dashed")+
    theme_classic() +
    theme(legend.position="bottom")+
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y=element_blank()) -> p
  
print(p)
} 
dev.off()

##### CLINSYN=2
CSE <- merged %>%
  filter(!is.na(Period) | !is.na(ANTISTARTDATE)| !is.na(ANTIENDDATE)) %>%
  filter(CLISYN=="2") %>% 
  group_by(USUBJID) 

pdf(paste0(output_folder, "plots CSE.pdf"),
    onefile = TRUE,
    width=12,height=15,
    
    pointsize=12)
for(i in 1:8){  
  ggplot(CSE,
         aes(x = ANTISTARTDATE-ADMISDATE,
             y = CSE_subgroup)) +
    facet_wrap_paginate(~ USUBJID, 
                        ncol = 6, nrow = 6, page = i,
                        scales=c("free_x"))+
    geom_segment(aes(
      x = ANTISTARTDATE-ADMISDATE,
      y = CSE_subgroup ,
      xend = ANTIENDDATE-ADMISDATE,
      yend = CSE_subgroup,
      color = CSE_subgroup
    ),
    linewidth=1.5) +
    #geom_line(data=...aes(x=.., y=..))+
    
    geom_vline(data=CSE,
               aes(xintercept = `Date of enrollment`-ADMISDATE),
               color="red"
    )+
    geom_vline(data=CSE,
               aes(xintercept = COLLDATE-ADMISDATE),
               linetype="dashed")+
    theme_classic() +
    theme(legend.position="bottom")+
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y=element_blank()) -> p
  
  
  print(p)
} 
dev.off()

##### CLINSYN=3
NE <- merged %>%
  filter(!is.na(Period) | !is.na(ANTISTARTDATE)| !is.na(ANTIENDDATE)) %>%
  filter(CLISYN=="3") %>% 
  group_by(USUBJID) 

pdf(paste0(output_folder, "plots NE.pdf"),
    onefile = TRUE,
    width=12,height=15,
    
    pointsize=12)
for(i in 1:8){  
  ggplot(NE,
         aes(x = ANTISTARTDATE-ADMISDATE,
             y = NE_subgroup)) +
    facet_wrap_paginate(~ USUBJID, 
                        ncol = 6, nrow = 6, page = i,
                        scales="free_x")+
    geom_segment(aes(
      x = ANTISTARTDATE-ADMISDATE,
      y = NE_subgroup ,
      xend = ANTIENDDATE-ADMISDATE,
      yend = NE_subgroup,
      color = NE_subgroup
    ),
    linewidth=1.5) +
    #geom_line(data=...aes(x=.., y=..))+
    
    geom_vline(data=NE,
               aes(xintercept = `Date of enrollment`-ADMISDATE),
               color="red"
    )+
    geom_vline(data=NE,
               aes(xintercept = COLLDATE-ADMISDATE),
               
               linetype="dashed")+
    #scale_color_manual(name = "Organism") +
    #scale_linetype(name = "Organism") +
    theme_classic() +
    theme(legend.position="bottom")+
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y=element_blank()) -> p
  
  
  print(p)
} 
dev.off()

