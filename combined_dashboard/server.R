#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  med_cost_ayush <- readRDS(file = 'C:/NSS75SCH25/Analysis/Outpatient/ayushmedcost.rds') %>%
    select(-c("op_totalmedexp_rs","op_totalmedexp_rs_low","op_totalmedexp_rs_upp", "op_diagnostics_rs","op_diagnostics_rs_low","op_diagnostics_rs_upp"))
  med_cost_allo <- readRDS(file = 'C:/NSS75SCH25/Analysis/Outpatient/allomedcost.rds')
  med_cost_ip<- readRDS(file = 'C:/NSS75SCH25/Analysis/Inpatient/medcostip.rds')

combined = left_join(med_cost_allo, med_cost_ayush, by = c('level_care', 'ailment', 'state_name')) %>%
  pivot_longer(cols =c('op_allopathy_meds_rs', 'op_diagnostics_rs', 'op_ayush_meds_rs'), names_to = 'Category', values_to= 'Expenditure')
 
##outpatient expenditure on medications, diagnostics
  selectedData <- reactive({
    #   input$ailment
    #  input$levelcare
    # input$statename

    df1 <- combined %>%
      dplyr::filter(ailment %in% input$ailment,
                    level_care %in% input$levelcare,
                    state_name %in% input$statename)
    df1
  })
  ###inpatient expenditure on medications
  selectedData2<-reactive ({
  df2 <- med_cost_ip %>%
    dplyr::filter(ailment %in% input$ailment,
                  level_care %in% input$levelcare,
                  state_name %in% input$statename)%>%
    pivot_longer(cols=c("ip_medicines_rs","ip_diagnostic_rs"),names_to="expense",values_to="ip_rs") %>% 
    mutate(expense = str_replace(expense,"ip_",""))
  df2
  })
  ####Prescription pattern for ayush meds outpatient
  selectedData3 <- reactive ({
    df3<- combined %>%
      dplyr::filter(ailment %in% input$ailment,
                    level_care %in% input$levelcare,
                    state_name %in% input$statename)  %>%
      mutate(ayush_yn = case_when(op_ayush_meds == 'Not received' ~ 'No',
                                  TRUE ~ 'Yes')) %>%
      dplyr::filter(ayush_yn == 'Yes')

    df3
      
  })
  ##prescription patterns for allopathy meds outpatient
  selectedData4 <- reactive({
    df4<- combined %>%
      mutate(allo_yn = case_when(op_allopathy_meds == 'Not received' ~ 'No',
                                 TRUE ~ 'Yes')) %>%
      dplyr::filter(allo_yn == 'Yes')
    df4
  })
 
  output$Plot <- renderPlot({
    x1 <-ggplot(selectedData(), aes(x= Category, y=Expenditure)) + geom_bar(position="stack", stat="identity", fill='red') +ggtitle('Outpatient Expenditure')
    x2 <-ggplot(selectedData2(), aes(x=expense, y=ip_rs)) + geom_bar(position="stack", stat="identity", fill= 'blue') + ggtitle ('Inpatient Expenditure')
    x3 <- ggplot(selectedData3(), aes(x=ayush_yn, fill=op_ayush_meds)) +geom_bar() + ggtitle('How Ayush medicines are received when prescribed')
    x4 <- ggplot(selectedData4(), aes(x=allo_yn, fill = op_allopathy_meds)) + geom_bar() + ggtitle('How allopathy medicines are received when prescribed')
    grid.arrange(x1,x2,x3,x4, ncol=2)
    })
  })

