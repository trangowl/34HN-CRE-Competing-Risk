options("install.lock"=FALSE)
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

library(pivottabler)
library(janitor)
## function to import data
input_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\"
output_folder <- "C:\\Users\\tranglq\\OneDrive - Oxford University Clinical Research Unit\\34HN - DATA - CRE case-control\\Multistate Model\\34HN-CRE-Competing-Risk\\Clean Data\\"

readRDS2 <- function(object, file, ...) readRDS(object, file = paste0(input_folder, file), ...)


## Frequency to determine top subgroup within each group
merged <- "Merged Data.rds" %>% 
  readRDS2()

### Table
## Frequency table
tab_dat <- merged %>% 
  group_by(Period,CLISYN,Subgroup,text) %>% 
  dplyr::summarise(n_id=length(unique(USUBJID))) 
  
  
## pivot table
pt <- PivotTable$new()
pt$addData(merged)
pt$addColumnDataGroups("CLISYN")
pt$addRowDataGroups("Period")
pt$addRowDataGroups("Subgroup") 
pt$addRowDataGroups("text")

 
pt$defineCalculation(calculationName="TotalGroup", 
                     summariseExpression="length(unique(USUBJID))")

pt$renderPivot()



#### table janitor
prop_tab <- merged %>%
  select(USUBJID,Subgroup, CLISYN, Period,text) %>% 
  unique() %>% 
  tabyl(Subgroup,CLISYN, Period) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns("front")

prop_tab$`Before Enrollment` %>% 
  write.csv(.,paste0(output_folder, "Before Enrollment.csv")) 

prop_tab$`After Enrollment` %>% 
  write.csv(.,paste0(output_folder, "After Enrollment.csv")) 

