variable_list <- readxl::read_excel("C:/NSS75SCH25/NSS Sch 25 Variable List.xlsx" ,sheet="nss_sch25")
library(dplyr)
# Block 6: particulars of medical treatment received as in-patient of a medical institution during the last 365 days
block6 <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L05.RDS")) %>% 
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 6") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
      ) %>%
  mutate(ip_duration = as.numeric(ip_duration)) %>%
  mutate_at(vars(c(treatment_nature_ip, ip_tbh_nature, ip_tpd_nature)), function(x) factor(x, levels=c(1:4, 9), labels=c("Allopathy",
                                                                                                                         "Indian system of medicine (desi dawai: ayurveda, unani or siddha)",
                                                                                                                         "Homoeopathy",
                                                                                                                         "Yoga & Naturopathy",
                                                                                                                         "Other"))) %>%
  mutate_at(vars(ip_institution_type), function(x) factor(x, levels=c(1:3), labels=c('Govt./public hospital',
                                                                                     'Charitable/Trust/NGO run hospital',
                                                                                     'Private hospital'))) %>%
  mutate_at(vars(ip_reason_notgovt), function(x) factor(x, levels=c(1:6, 9), labels=c("Required specific services not available",
                                                                                      "Available but quality not satisfactory",
                                                                                      "Quality satisfactory but facility too far",
                                                                                      "Quality satisfactory but involves long waiting", 
                                                                                      "Financial constraint",
                                                                                      "Preference for a trusted doctor/hospital",
                                                                                      "Others"))) %>%
  mutate_at(vars(ip_when_admitted), function(x) factor(x, levels=c(1:3), labels= c('During last 15 days',
                                                                                   '16 days to 365 days ago',
                                                                                   'More than 365 days ago'))) %>%
  mutate_at(vars(ip_when_discharged), function(x) factor(x, levels=c(1:3), labels=c('Not yet',
                                                                                    'During last 15 days',
                                                                                    '16 days to 365 days ago'))) %>%
  mutate_at(vars(c(ip_tbh_level, ip_tpd_level)), function(x) factor(x, levels =c(1:5), labels = c("Govt./public hospital",
                                                                                                  "Govt./public hospital",
                                                                                                  "Private hospital",
                                                                                                  "Private doctor/clinic",
                                                                                                  "Informal health care provider"))) %>%
  mutate_at(vars(ip_ward_type), function(x) factor(x, levels=c(1:3), labels=c("Free", 
                                                                        "Paying general",
                                                                        "Paying special"))) %>%
  mutate_at(vars(c(ip_surgery, ip_medication, ip_diagnostic, ip_otherdiagnostic )), function(x) factor(x,levels=c(1:4),labels=c("Not received",
                                                                                                                                         "Free",
                                                                                                                                         "Partly",
                                                                                                                                         "Payment"))) %>%
  mutate_at(vars(c(ip_trt_before_hosp, ip_trt_post_disch)), function(x) factor(x, levels=c(1:2), labels=c('Yes', 
                                                                                                          'No')))
                                                                                     
                                                                                                                                          
 

# Block 7: expenses incurred during the last 365 days for treatment of members as in-patient of medical institution
# Block 7 is divided into 2 datasets
###level 6 -- block 7, items 1-14
block7a <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L06.RDS")) %>%
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 7a") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
  ) %>%
  mutate_at(vars(ip_free_med_advice), function(x) factor(x, levels=c(1:4), labels=c("Yes: govt./public", 
                                                                                    "Yes :Pvt.(incl. Charitable/NGO/Trust run hospital)", 
                                                                                    "Both", 
                                                                                    "No" )))

###level 7-- block 7, items 15-20
block7b <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L07.RDS")) %>%
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 7b") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
  ) %>%
  mutate_at(vars(c(ip_major_fin_src, ip_next_fin_src)), function(x) factor(x, levels=c(1:4, 9), labels=c("Household income/ savings",
                                                                                                         "Borrowings",
                                                                                                         "Sale of physical assets",
                                                                                                         "Contributions from friends and relatives",
                                                                                                         "Other sources"))) %>%
  mutate_at(vars(ip_hosp_place), function(x) factor(x, levels=c(1:5), labels=c("Same district (rural area)",
                                                                               "Same district (urban area)",
                                                                               "Within state different district (rural area)",
                                                                               "Within state different district (urban area)",
                                                                               "Other state")))

 ###merging level 6 and 7 to get complete data for block 7 - inpatient info
block7<- bind_cols(block7a, block7b, id=NULL)
block7<- block7 %>%
  select(-c(block7id...22, level...23, filler...24, slno_hospitalization...25, slno_member...26, age...27, block7blank...34, nss...35, nsc...36, mult...37)
         ) %>%
  rename(block7id=block7id...1,
         level=level...2,
         filler=filler...3,
         slno_hospitalization=slno_hospitalization...4,
         slno_member=slno_member...5,
         age=age...6,
         block7blank=block7blank...18,
         nss=nss...19,
         nsc=nsc...20,
         mult=mult...21,
         ) 

#merging block 6 and block 7 to get complete inpatient info
inpatient<-bind_cols(block6, block7, id=NULL) %>%
  select(-c(block7id,level...32, filler...33, slno_hospitalization...34, slno_member...35, age...36, nss...49, nsc...50, mult...51)) %>%
  rename(ip_id=block6id,
         level=level...2,
         filler=filler...3,
         slno_hospitalization=slno_hospitalization...4,
         slno_member=slno_member...5,
         age=age...6,
         nss=nss...28,
         nsc=nsc...29,
         mult=mult...30) %>%
  mutate(sampleweight=case_when(nss==nsc ~ mult/100,
                                TRUE     ~ mult/200))



ns25p04_m6and7 <- writexl::write_xlsx(inpatient, 'C:/NSS75SCH25/ns25p04_m6and7.xlsx')



