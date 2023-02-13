library(readxl)

library(dplyr)


   ###
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
   
   
   inpatient <- read.csv("C:/NSS75SCH25/Preprocessing/Inpatient/inpatient_m6and7.csv") %>%
     separate(col=ip_id,into=c("sl_no","hhid"),sep=3,remove = FALSE) %>%
     dplyr::filter(ail_nature_ip  %in% c(13,16,34, 35)) %>%
     mutate(ailment = case_when(ail_nature_ip == '13' ~ 'Cancer',
                                ail_nature_ip == '16' ~ 'Diabetes',
                                ail_nature_ip %in% c('34','35') ~ 'Cardiovascular Disease')) %>%
     mutate(level_care = case_when(ip_institution_type %in% c('Govt./public hospital', 'Charitable/Trust/NGO run hospital') ~ 'Government',
                                   TRUE ~ 'Private' )) %>% 
     inner_join(., R75250L01, by='hhid')

   ###possible missing psus
region_codes <- read.csv("C:/code/nss_sch25/round 75/R75 district codes.csv")

x <- left_join(inpatient, region_codes, by = c("region_code","district"="district_code"))
###incorporating state names from district codes
state_list = list(unique(x$state_name))
print(state_list)

library(srvyr)


#group by level of care, ailment type,state, medical and nonmedical exp
test <- x %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name) %>%
  srvyr::summarize_at(vars(ip_totalmedexp_rs,ip_totalnonmed_exp_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))
med_nonmed <- saveRDS(test,file = 'C:/NSS75SCH25/Analysis/Inpatient/mednonmed.rds')

###group by level of care, ailment type, state: medical exp, medicines
med_direct_rs <- x %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name) %>%
  srvyr::summarize_at(vars(ip_totalmedexp_rs, ip_medicines_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))   
med_cost <- saveRDS(med_direct_rs,file = 'C:/NSS75SCH25/Analysis/Inpatient/medcost.rds')

###group by level of care, ailment type, state: medical exp, diagnostics
diag_direct_rs  <- x %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name) %>%
  srvyr::summarize_at(vars(ip_totalmedexp_rs, ip_diagnostic_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))   
diag_cost<- saveRDS(diag_direct_rs, file = 'C:/NSS75SCH25/Analysis/Inpatient/diagcost.rds')

###group by level of care, ailment type, state: nonmedical exp, transport costs
transport_indi_rs  <- x %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name ) %>%
  srvyr::summarize_at(vars(ip_totalnonmed_exp_rs, ip_transport_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))   
transp_cost_nonmed <- saveRDS(transport_indi_rs, file='C:/NSS75SCH25/Analysis/Inpatient/transportcostnonmed.rds')
###group by level of care, ailment type, state: nonmedical exp, other costs
other_indi_rs  <- x %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name, ip_totalnonmed_exp_rs) %>%
  srvyr::summarize_at(vars(ip_othernonmed_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci")) 
other_cost_nonmed <- saveRDS(other_indi_rs, file='C:/NSS75SCH25/Analysis/Inpatient/othercostnonmed.rds')
