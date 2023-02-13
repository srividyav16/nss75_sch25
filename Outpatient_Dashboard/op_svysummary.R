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

###filtering npcdcs diseases
outpatient <-readRDS("C:/NSS75SCH25/Preprocessing/Outpatient/outpatientm8&9.rds") %>%
  separate(col=block8id, sep =3, into =c('sl_no', 'hhid'), remove = FALSE) %>%
  dplyr::filter(ail_nature %in% c(13,16,34,35)) %>%
                  dplyr::mutate(ailment = case_when(ail_nature== '13' ~ "Cancer",
                                                    ail_nature == '16' ~ "Diabetes",
                                                    ail_nature %in% c('34', '35') ~ "Cardiovascular diseases")) %>%
  dplyr::mutate(level_care = case_when(level_of_care == 'Govt./public hospital' ~ 'Government',
                                       level_of_care %in% c('Private hospital','Private doctor/clinic') ~ 'Private',
                                       level_of_care == 'informal health care provider' ~ 'Informal HealthCare Provider')) %>%
    inner_join(., R75250L01, by = 'hhid') %>%
  select(-block8id)

region_codes <- read.csv("C:/code/nss_sch25/round 75/R75 district codes.csv")

op_svy <- left_join(outpatient, region_codes, by = c("region_code","district"="district_code"))

#group by level of care, ailment type,state, medical and nonmedical exp
test <- op_svy %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name) %>%
  srvyr::summarize_at(vars(op_totalmedexp_rs,op_totalnonmed_exp_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))
med_nonmed <- saveRDS(test,file = 'C:/NSS75SCH25/Analysis/Outpatient/mednonmedop.rds')

###group by level of care, ailment type, state: medical exp, ayush medicines and diag
ayush_med_direct_rs <- op_svy %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name, op_ayush_meds, op_xray_scans,op_other_diagnostics) %>%
  srvyr::summarize_at(vars(op_totalmedexp_rs, op_ayush_meds_rs, op_diagnostics_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))   
med_cost_ayush <- saveRDS(ayush_med_direct_rs,file = 'C:/NSS75SCH25/Analysis/Outpatient/ayushmedcost.rds')

###group by level of care, ailment type, state: medical exp, allopathy medicines and diag
allopathy_med_direct_rs <- op_svy %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name, op_allopathy_meds, op_xray_scans,op_other_diagnostics) %>%
  srvyr::summarize_at(vars(op_totalmedexp_rs, op_allopathy_meds_rs, op_diagnostics_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))   
med_cost_allo <- saveRDS(allopathy_med_direct_rs,file = 'C:/NSS75SCH25/Analysis/Outpatient/allomedcost.rds')

###group by level of care, ailment type, state: nonmedical exp -- transport costs and other
indirect_rs <- op_svy %>%
  as_survey_design(ids = psu,
                   strata = stratum, 
                   weights = sampleweight,
                   nest=TRUE) %>%
  group_by(level_care, ailment, state_name) %>%
  srvyr::summarize_at(vars(op_totalnonmed_exp_rs, op_transport_rs, op_othernonmed_rs),
                      ~survey_mean(.,na.rm=TRUE,vartype="ci"))  
indirect_cost <-saveRDS(indirect_rs, file = 'C:/NSS75SCH25/Analysis/Outpatient/indirectcost.rds')
