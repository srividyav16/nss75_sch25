variable_list <- readxl::read_excel("round 75/NSS Sch 25 Variable List.xlsx",sheet="nss_sch25")

####Level 8,Block 8-  particulars of spells of ailment of household members during the last 15 days (hospitalisation and nonhospitalisation cases
block8 <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L08.RDS")) %>% 
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 8") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
  ) %>%
  mutate(outpatient_case = case_when(hosp_yn==2 ~ 1,
                                     TRUE       ~ 0)) %>%
  mutate(inpatient_case =  case_when(hosp_yn==1 ~ 1,
                                     TRUE       ~ 0)) %>%
  mutate_at(vars(hosp_yn, chronic_yn, trt_med_advice_yn), function(x) factor(x, levels=c(1:2), labels=c('Yes', 
                                                                                'No'))) %>%
  mutate_at(vars(ail_status), function(x) factor(x, levels=c(1:4), labels=c("started more than 15 days ago and is continuing",
                                                                             "started more than 15 days ago and has ended",
                                                                             "started within 15 days and is continuing",
                                                                             "started within 15 days and has ended"))) %>%
  mutate_at(vars(treatment_nature), function(x) factor(x, levels=c(1:5, 9), labels=c("Allopathy",
                                                                                     "Indian system of medicine (desi dawai: ayurveda, unani or siddha)",
                                                                                     "Homoeopathy",
                                                                                     "Yoga & Naturopathy",
                                                                                     "No treatment",
                                                                                     "Other"))) %>%
  mutate_at(vars(level_of_care), function(x) factor(x, levels=c(1:5), labels=c("Govt./public hospital",
                                                                               "Govt./public hospital",
                                                                               "Private hospital",
                                                                               "Private doctor/clinic",
                                                                               "informal health care provider"))) %>%
  mutate_at(vars(reason_no_govt_service), function(x) factor(x, levels=c(1:6, 9), labels=c("required specific services not available",
                                                                                           "available but quality not satisfactory",
                                                                                           "quality satisfactory but facility too far",
                                                                                           "quality satisfactory but involves long waiting", 
                                                                                           "financial constraint",
                                                                                           "preference for a trusted doctor/hospital",
                                                                                           "others"))) %>%
  mutate_at(vars(reason_no_medadv), function(x) factor(x, levels=c(1:5, 9), labels=c("no medical facility available in the neighbourhood",
                                                                                     "facility too expensive",
                                                                                     "cannot afford to wait long due to domestic/economic engagement",
                                                                                     "ailment not considered serious enough",
                                                                                     "familial/religious belief",
                                                                                     "others"))) %>%
  mutate_at(vars(person_consulted), function(x) factor(x, levels=c(1:2, 9), labels=c("self / other household member/ friend",
                                                                                     "medicine shop", 
                                                                                     "others")))
  



### Level 9 block 9 items 1-18
block9a <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L09.RDS")) %>%
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 9a") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
  )


###level 10-block 9, items 19-23
block9b <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L10.RDS")) %>%
  rename_all(function(x) 
    variable_list %>% 
      dplyr::filter(block=="Block 9b") %>% 
      dplyr::select(variable_name) %>% 
      pull(.)
  )

###merging level 9 and 10 to get complete block 9
block9<- bind_cols(block9a, block9b, id=NULL) %>%
  select(-c(block9id...26,level...27, filler...28, slno_ailment...29, slno_member...30, age...31, block9blank...37, nss...38, nsc...39, mult...40)) %>%
  rename(block8id=block9id...1,
         level=level...2,
         filler=filler...3,
         slno_ailment=slno_ailment...4,
         slno_member=slno_member...5,
         age=age...6,
         block9blank=block9blank...22,
         nss=nss...23,
         nsc=nsc...24,
         mult=mult...25) %>%
  mutate(sampleweight=case_when(nss==nsc ~ mult/100,
                                TRUE     ~ mult/200)) %>%
  mutate_at(vars(op_free_med_advice), function(x) factor(x, levels=c(1:4), labels=c("Yes: govt./public", 
                                                                                    "Yes :Pvt.(incl. Charitable/NGO/Trust run hospital)", 
                                                                                    "Both", 
                                                                                    "No"))) %>%
  mutate_at(vars(c(op_surgery, op_ayush_meds, op_allopathy_meds, op_xray_scans,  op_other_diagnostics)), function(x) factor(x, levels=c(1:4),
                                                                                                                            labels=c("Not received",
                                                                                                                            "Received: free",
                                                                                                                            "Partly free", 
                                                                                                                            "On payment"))) %>%
  mutate_at(vars(op_major_fin_src), function(x) factor(x, levels=c(1:4, 9), labels=c("Household income/ savings",
                                                                                     "Borrowings",
                                                                                     "Sale of physical assets",
                                                                                     "Contributions from friends and relatives",
                                                                                     "Other sources"))) %>%
  mutate_at(vars(op_trt_place), function(x) factor(x, levels=c(1:5), labels=c("Same district (rural area)",
                                                                              "Same district (urban area)",
                                                                              "Within state different district (rural area)",
                                                                              "Within state different district (urban area)",
                                                                              "Other state")))

####total outpatient info
outpatient <- block8 %>% 
  bind_cols(
    block9 %>% dplyr::select(-block8id,-level,-filler,-slno_ailment,-slno_member,-age,-nss,-nsc,-mult)) %>%
  dplyr::filter(outpatient_case==1)

#converting age to numeric
outpatient$age_num <- as.numeric(outpatient$age) 
 outpatient['age']=outpatient['age_num'] 
outpatient <- outpatient %>%
  dplyr::select(-age_num)

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

