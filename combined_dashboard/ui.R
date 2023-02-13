#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("National Sample Survey Round 75 Schedule 25"),
    

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput('ailment',
                      label = 'Select ailment: ',
                      choices = c('Diabetes', 'Cancer', 'Cardiovascular Diseases'),
                      selected = 'Diabetes'),
          selectInput('levelcare',
                      label = 'Select level of care: ',
                      choices = list('Government', 'Private'),
                      selected = 'Government'),
          selectInput("statename",
                      label = 'Select state: ',
                      choices = list("Jammu & Kashmir","Himachal Pradesh","Punjab","Chandigarh","Uttarakhand","Haryana","Delhi","Rajasthan","Uttar Pradesh","Bihar","Sikkim","Arunachal Pradesh","Nagaland","Manipur","Mizoram","Tripura","Meghalaya","Assam","West Bengal","Jharkhand","Odisha","Chhattisgarh","Madhya Pradesh", "Gujarat","Daman & Diu","Dadra&","Maharashtra","Andhra Pradesh","Karnataka","Goa","Lakshadweep","Kerala","Tamil Nadu","Puducherry","Andaman & Nicobar Islands","Telangana"),
                      selected= 'Maharashtra')
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot")
            #title('Outpatient Expenditure')
            #title('Inpatient Expenditure')
        )
    )
))
