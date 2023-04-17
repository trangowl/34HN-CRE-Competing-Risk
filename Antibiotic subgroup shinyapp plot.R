
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


### Anb category
anb_cat <- read_excel("27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                      sheet="Category ANB") %>% 
  filter(category =="ANTIBIO")

### Drug classification
drugc <- read.csv("List of antibiotic atc code, ingredient, group and subgroup.csv") %>% 
  select(ATC.code,ATC.level.name,Subgroup)

### Merge anb_cat and drug classification
anb_cat_m <- anb_cat %>% 
  left_join(., drugc, by=c("text"="ATC.level.name")) %>% 
  distinct()

## ##### Baseline 
baseline  <- read_excel("27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                        sheet="BASELINE") %>% 
  select(USUBJID,ENDATE,ADMISDATE,WARD, CLISYN) %>% 
  dplyr::rename("Date of enrollment"="ENDATE") %>% 
  mutate_at(vars(`Date of enrollment`, 
                 ADMISDATE),
            funs(as.Date(., "%d/%m/%Y")))

## antibiotic baseline
baseline_anti  <- read_excel("27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                             sheet="BASELINE_ANTI") %>%
  select(USUBJID,ANTIBIOTIC,ANTISTARTDATE,ANTIENDDATE) %>% 
  mutate_at(vars(ANTISTARTDATE, 
                 ANTIENDDATE),
            funs(as.Date(., "%d/%m/%Y")))%>% 
  mutate(Period="Before Enrollment")


#### After enrollment-Discharge
dis  <- read_excel("27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                   sheet="DIS") %>%
  select(USUBJID,OUTCOME,DATEDIS,ICDCODE)%>% 
  mutate_at(vars(DATEDIS),
            funs(as.Date(., "%d/%m/%Y"))) 


dis_anti  <- read_excel("27-2-2023-_34HN_SS2_V1_Data.xlsx", 
                        sheet="DIS_ANTI") %>%
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



## Merge all data
merged <- baseline %>% 
  left_join(., dis, by=c("USUBJID")) %>% 
  left_join(., anti_dat, by=c("USUBJID")) %>% 
  distinct() %>% 
  mutate(Subgroup_small=case_when(Subgroup=="Carbapenems"~"Carbapenems",
                                  Subgroup=="Fluoroquinolones"~"Fluoroquinolones",
                                  Subgroup=="Combinations of penicillins, incl. beta-lactamase inhibitors"  ~"Combinations of penicillins, incl. beta-lactamase inhibitors" ,
                                  TRUE~"Other")) %>% 
  mutate(Period=relevel(as.factor(Period), ref="Before Enrollment")) %>% 
  mutate(USUBJID=as.factor(USUBJID))



### Example Plot

merged %>%
  filter(!is.na(Period)) %>%
  filter(USUBJID == "003-01-1-0-R") %>%
  ggplot(.,
         aes(x = ANTISTARTDATE,
             y = Subgroup)) +
  geom_segment(aes(
    x = ANTISTARTDATE,
    y = Subgroup ,
    xend = ANTIENDDATE,
    yend = Subgroup,
    color = Period
  )) +
  facet_wrap( ~ Period) +
  geom_vline(xintercept = merged$`Date of enrollment`[which(merged$USUBJID ==
                                                              "003-01-1-0-R")],
             linetype = "dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))

######
ui <- shinyUI(navbarPage("",
                         tabPanel("Antibiotic duration in individual level",
                                  sidebarPanel(
                                               
                                               selectInput("id", 
                                                           "Subject ID",
                                                           unique(merged$USUBJID),
                                                           selected="003-01-1-0")),
                                  
                                  mainPanel(plotOutput("plot"),
                                            tableOutput("summary"))),
                         
                         
                         
                         
))

server <- function(input, output,session) {
  
  data_id <- reactive({
    id1 <- input$id
    merged[(merged$USUBJID %in% id1),]
    
  })
  
  output$plot <- renderPlot({
    data_id() %>% 
      filter(!is.na(Period)) %>% 
      #filter(USUBJID %in% input$id) %>% 
      ggplot(.,aes(x = ANTISTARTDATE,
                 y = Subgroup)) +
      geom_segment(aes(
        x = ANTISTARTDATE,
        y = Subgroup ,
        xend = ANTIENDDATE,
        yend = Subgroup,
        color = Period,
        group=Period)) +
      geom_line()+
      #facet_wrap(~Period)+
      geom_vline(xintercept = data_id()$`Date of enrollment`,
                 linetype="dashed")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme_classic()+
      labs("Antiobiotc duration")
    
    
  })
  
  output$summary <- renderTable({
    data_id() %>% 
      mutate_if(is.Date,~format(.,"%d-%m-%Y")) 
  })
}
shinyApp(ui = ui, server = server)
