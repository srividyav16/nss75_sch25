####Demographic data
R75250L03 <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L03.RDS")) %>% 
  mutate(male = case_when(V6 == 1 ~ 1,
                          TRUE ~ 0),
         age = V7,
         sampleweight = case_when(V22 == V23 ~ V24/100,
                                  TRUE ~ V24/200),
         
         ) %>% 
  mutate(marital_status = case_when(V8 == 1 ~ 'Never married',
                                    V8 == 2 ~ 'Currently married',
                                    V8 == 3 ~ ' Widowed',
                                    TRUE    ~ 'Divorced/Separated')) %>%
  mutate(education = case_when(V9 == 01 ~ 'Not literate',
                               V9 == 02 ~ 'Literate without any schooling',
                               V9 == 03 ~ 'Literate without formal schooling: through NFEC',
                               V9 == 04 ~ 'Literate through TLC/AEC',
                               V9 == 05 ~ 'Others',
                               V9 == 06 ~ 'Literate with formal schooling: below primary',
                               V9 == 07 ~ 'Primary',
                               V9 == 08 ~ 'Upper primary/middle',
                               V9 == 10 ~ 'Secondary',
                               V9 == 11 ~ 'Higher secondary',
                               V9 == 12 ~'Diploma /certificate course (upto secondary)',
                               V9 == 13 ~ 'Diploma /certificate course (higher secondary)',
                               V9 == 14 ~ 'Diploma /certificate course (graduation and above)',
                               V9 == 15 ~ 'Graduate',
                               TRUE ~ 'Post graduate and above')) %>%
  mutate(hosp_yn = case_when(V11 == 1 ~ 'Yes',
                             TRUE ~ 'No')) %>%
    mutate(chronic_yn = case_when(V16 == 1 ~ 'Yes',
                                  TRUE ~ 'No')) %>%
    separate(col=V1,into=c("sl_no","hhid"),sep=3,remove = FALSE)

# All of V1 in R7525L03 are 34 characters
table(str_length(R75250L03$V1))

R75250L01 <- readRDS(paste0(path_nsso_paper,"/working/nss75 sch25/rds/R75250L01.RDS")) %>% 
  mutate(hhid = paste0(V2,V3,V4,
                       V5,V6,V7,
                       V8,V9,V10,
                       V11,V12,V13,
                       V14,V15,V16)) %>%
           rename(stratum = V9,
                  hamlet = V14,
                  psu = V15)
         
         
merged <- R75250L01 %>%
  dplyr::select(stratum,hamlet,psu,hhid) %>% 
  left_join(R75250L03 %>% 
              dplyr::select(hhid,sl_no,male,age,marital_status,education, hosp_yn, chronic_yn, sampleweight),
            by = "hhid") %>% 
  mutate(sampleweight = case_when(is.na(sampleweight) ~ median(sampleweight,na.rm=TRUE),
                                  TRUE ~ sampleweight))

saveRDS(merged,'C:/NSS75SCH25/ns25p01_merged.RDS')

# Survey weighting ------------
require(survey) # Common package. Has survey regressions etc
require(srvyr) # Extension of survey package for dplyr type syntax

merged_svy <- as_survey_design(merged,ids = psu,
                               strata=stratum,
                               weights=sampleweight,
                               nest=TRUE) %>%
  group_by(chronic_yn, marital_status, education) %>%
  srvyr::summarise(count =n(),
                   male_pct = survey_mean(male,na.rm=TRUE,vartype = "ci"),
                   age = survey_mean(age,na.rm=TRUE,vartype="ci"))
                   



