#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  med_cost_ayush <- readRDS(file = 'C:/NSS75SCH25/Analysis/Outpatient/ayushmedcost.rds') %>%
    mutate(ayush_yn = case_when(op_ayush_meds == 'Not received' ~ 'No',
                                TRUE ~ 'Yes'))
  med_cost_allo <- readRDS(file = 'C:/NSS75SCH25/Analysis/Outpatient/allomedcost.rds') %>%
    mutate(allo_yn = case_when(op_allopathy_meds == 'Not received' ~ 'No',
                               TRUE ~ 'Yes'))
  indirect_cost <-readRDS(file = 'C:/NSS75SCH25/Analysis/Outpatient/indirectcost.rds')

  op_cost_combined = left_join(med_cost_allo, med_cost_ayush, by =c('level_care', 'ailment', 'state_name')) %>%#'op_xray_scans', 'op_other_diagnostics' 
    left_join(indirect_cost, by = c('level_care', 'ailment', 'state_name')) %>%
    select(-c("op_diagnostics_rs.y", "op_diagnostics_rs_low.y",  "op_totalmedexp_rs.y", "op_totalmedexp_rs_low.y", "op_totalmedexp_rs_upp.y")) %>%
    rename(op_total_medexp_rs =op_totalmedexp_rs.x,
           op_total_medexp_rs_low =op_totalmedexp_rs_low.x,
           op_total_medexp_rs_upp=op_totalmedexp_rs_upp.x,
           op_diagnostics_rs = op_diagnostics_rs.x,
           op_diagnostics_rs_low =op_diagnostics_rs_low.x,
           op_diagnostics_rs_upp = op_diagnostics_rs_upp.x  
           ) %>%
    pivot_longer(cols =c('op_allopathy_meds_rs', 'op_diagnostics_rs', 'op_ayush_meds_rs', 'op_transport_rs', 'op_othernonmed_rs'), names_to = 'Category', values_to= 'Expenditure')
        # generate bins based on input$bins from ui.R
  selectedData <- reactive({
    #   input$ailment
    #  input$levelcare
    # input$statename
    
    df1 <- op_cost_combined %>%
      dplyr::filter(ailment %in% input$ailment,
                    level_care %in% input$levelcare,
                    state_name %in% input$statename)
    df1
  })
        # draw the histogram with the specified number of bins
    output$Plot <- renderPlot({
      ggplot(selectedData(), aes(x= Category, y=Expenditure)) + 
        geom_bar(position="stack", stat="identity")

    })
   # op_count = left_join(med_cost_allo, med_cost_ayush, by =c('level_care', 'ailment', 'state_name', 'op_xray_scans', 'op_other_diagnostics'))%>%
    #  select(c("level_care","ailment","state_name","op_allopathy_meds","op_xray_scans","op_other_diagnostics", "op_ayush_meds", 'ayush_yn', 'allo_yn'))  %>%
     # mutate(medication = case_when(ayush_yn == 'Yes'  ~ 'Ayush',
                                #    ayush_yn == 'No' ~ 'Allopathy')) 

    #selectData2 <- reactive({
     # df2 <- op_count %>%
      #dplyr::filter(ailment %in% input$ailment,
         #           level_care %in% input$levelcare,
          #          state_name %in% input$statename)
    #df2
    #})
    #output$plot2 <- renderPlot ({
     # ggplot(selectData2(), aes(x='medication', fill =c('op_ayush_meds', 'op_allopathy_meds')),
      #       geom_bar(position = 'fill'))
    #})
})
