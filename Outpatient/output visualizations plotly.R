#filtering df to get only ailment type as diabetes
diabetes_op<- outpatient %>%
  dplyr::filter(ail_nature==16)
library(plotly)
?plotly()


plot1 <- plot_ly (diabetes_op, x = ~level_of_care,  y = ~ail_duration, type="bar")

plot1


plot2<-plot_ly (diabetes_op, x = ~level_of_care,  y = ~op_totalmedexp_rs, type="bar")
plot2

###trt nature in max private hospitals
data<- diabetes_op %>%
  filter(level_of_care=="Private hospital")
table(data$treatment_nature)

labels=c('Allopathy', 'Indian system', 'Homeopathy', 'Naturopathy and Yoga', 'No treatment', 'Other')
values = c(2137, 21, 10, 0, 0, 1)
plot3<- plot_ly(type='pie', labels=labels, values=values,
                textinfo = 'label+percent')
plot3<-plot3 %>%
  layout(title='Nature of treatment in private hospitals')
plot3

