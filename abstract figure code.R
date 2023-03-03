library(dplyr)
R75250L01 <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L01.RDS")) %>%
  mutate(stratum = V9,
         hamlet = V14,
         psu = V15,
         region_code = as.numeric(str_replace_all(V7,"^0+","")),
         district = as.numeric(V8),
         hhid = paste0(V2,V3,V4,
                       V5,V6,V7,
                       V8,V9,V10,
                       V11,V12,V13,
                       V14,V15,V16)  
  ) %>%
  dplyr::select(c(stratum,hamlet,psu,hhid, region_code, district)) %>%
  mutate(psu = case_when(is.na(psu) & !is.na(hamlet) ~ hamlet, 
                         TRUE ~ psu)) %>% 
  dplyr::filter(!is.na(psu))
####inpatient data
inpatient_npcdcs<- readRDS('C:/NSS75SCH25/Preprocessing/Inpatient/inpatient_npcdcs.rds') %>%
  mutate(ip_medexpfull_rs = case_when(is.na(ip_totalmedexp_rs) ~ 0,
                                      TRUE ~ ip_totalmedexp_rs)) %>%
  separate(col=ip_id,into=c("sl_no","hhid"),sep=3,remove = FALSE) %>%
  mutate(ailment = case_when(ail_nature_ip == '13' ~ 'Cancer',
                             ail_nature_ip == '16' ~ 'Diabetes',
                             ail_nature_ip %in% c('34','35') ~ 'Cardiovascular Disease')) %>%
  mutate(level_care = case_when(ip_institution_type %in% c('Private hospital', 'Charitable/Trust/NGO run hospital') ~ 'Private',
                                TRUE ~ 'Public')) %>%
  inner_join(., R75250L01, by = 'hhid')
##outpatient 
outpatient_npcdcs <- readRDS('C:/NSS75SCH25/Preprocessing/Outpatient/outpatient_npcdcs.rds') %>%
  mutate(op_medexpfull_rs = case_when(is.na(op_totalmedexp_rs) ~ 0,
                                      TRUE ~ op_totalmedexp_rs)) %>%
  separate(col=block8id,into=c("sl_no","hhid"),sep=3,remove = FALSE) %>%
  mutate(ailment = case_when(ail_nature == '13' ~ 'Cancer',
                             ail_nature == '16' ~ 'Diabetes',
                             ail_nature %in% c('34','35') ~ 'Cardiovascular Disease')) %>%
  mutate(level_care = case_when(level_of_care == 'Govt./public hospital' ~ 'Public',
                                TRUE ~ 'Private')) %>%
  inner_join(., R75250L01, by = 'hhid')

##reading in district code with state names
region_codes <- read.csv("C:/code/nss_sch25/round 75/R75 district codes.csv")
####merging with inpatient df
inpatient_npcdcs_m <- left_join(inpatient_npcdcs, region_codes, by = c("region_code","district"="district_code"))
###merging with outpatient df
outpatient_npcdcs_m <- left_join(outpatient_npcdcs, region_codes, by = c("region_code","district"="district_code"))

###merging ip and op


library(srvyr)
###total med exp for inpatient
ip_medexptotal <- inpatient_npcdcs_m %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment) %>%
  srvyr::summarize_at(vars(ip_medexpfull_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))
#saving output
ip_medexp_total <- saveRDS(ip_medexptotal, file ='C:/NSS75SCH25/Analysis/Abstract/ip_medexp_total.rds')

###total med exp for outpatient
op_medexptotal <- outpatient_npcdcs_m %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment) %>%
  srvyr::summarize_at(vars(op_medexpfull_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))
#saving output
op_medexp_total <- saveRDS(op_medexptotal, file = 'C:/NSS75SCH25/Analysis/Abstract/op_medexp_total.rds')


###merging combined summary df
merged_ip_op <- inner_join(ip_medexptotal, op_medexptotal, by = c('level_care', 'ailment')) %>%
  mutate(totalmedexp = (ip_medexpfull_rs + op_medexpfull_rs)) %>%
  pivot_longer(cols= c('ip_medexpfull_rs', 'op_medexpfull_rs'), names_to="expense",values_to="rs")

###DATA VISUALIZATION
library(ggplot2)

plot<-ggplot(merged_ip_op, aes(fill = expense, x = ailment, y = rs)) +  
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level_care)

print(plot + ggtitle('Total Medical Expenditure Comparison in Public and Private Levels')+
        labs(y = 'Expenditure', x = 'Level of care')+
        labs(fill = 'Type of treatment') +
        scale_fill_hue(labels=c('Inpatient', 'Outpatient')))


