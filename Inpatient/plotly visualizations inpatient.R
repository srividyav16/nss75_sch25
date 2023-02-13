###filtering 
diabetes_ip <- inpatient %>%
  filter(ail_nature_ip==16)
library(plotly)  
fig1<- plot_ly(diabetes_ip, x=~ip_duration, y=~age)         
fig1

colSums(diabetes_ip[ , c(33, 34,35,36,37,38)], na.rm = TRUE)
labels = c('Doctor/physician Fee', 'Medicines', 'Diagnostic tests', 'Bed charges', 'Other')
values = c(3385475, 7202133, 3040033, 3023441, 1923335)
fig2<- plot_ly(type='pie', labels=labels, values=values,
               textinfo = 'label+percent', 
               title='Distribution of total medical costs across all hospitals', 
               textposition='inside', 
               showlegend = FALSE)
fig2
x<- diabetes_ip %>%
  filter(ip_institution_type=='Govt./public hospital')
colSums(x[ ,c(33, 34,35,36,37,38)], na.rm = TRUE)

y<- diabetes_ip %>%
  filter(ip_institution_type=='Private hospital')
colSums(y[ ,c(33, 34,35,36,37,38)], na.rm = TRUE)
fig3<- plot_ly(type='pie', labels=~c('Doctor/physician Fee','Medicines','Diagnostic tests', 'Bed charges', 'Other'), values=~c(129100, 1461395, 601368, 45903, 251775), textinfo='label+percent', 
               title = "Distribution of direct medical costs in Govt. Hospitals", showlegend = F)
fig3

fig4<- plot_ly(type='pie', labels=~c('Doctor/physician Fee','Medicines','Diagnostic tests', 'Bed charges', 'Other'), values=~c(3149595, 5510728, 2320160, 2893788, 1570285), textinfo='label+percent',
               title = "Distribution of direct medical costs in private Hospitals", showlegend = F)

