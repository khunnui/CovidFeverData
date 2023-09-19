#------------------------------------------------------------------------------#
# Description: Program for prepare data frames and gt tables for Covid-Fever   #
#              dashboard https://dghp.shinyapps.io/COVID-Fever/  	  	         #
# Author:      hpy1                                                            #
# Created:     January 27, 2022                                                #
# Modified:    March 9, 2022                                                   #
#------------------------------------------------------------------------------#

library(labelled)
library(gtsummary)

# Output theme
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

date1    <- max(tblSection1$last_edit_date, na.rm = TRUE)
date2    <- max(tblSection2$last_edit_date, na.rm = TRUE)
date3    <- max(tblSection3$last_edit_date, na.rm = TRUE)
date4    <- max(tblSection4$last_edit_date, na.rm = TRUE)
date5    <- max(tblSection5$last_edit_date, na.rm = TRUE)
datepcr  <- max(LabPCRResult_l$approvedate, na.rm = TRUE)
ddate    <- max(c(date1, date2, date3, date4, date5, datepcr), na.rm = TRUE)
source(paste0(code_folder, "/Functions.R"))

#-------------------------------------------------------------------------------
# Screening page
#-------------------------------------------------------------------------------

df_scrw <- tblSection1 %>%
  mutate(scrdate = floor_date(s1screendate, "week", week_start = 1)) %>%
  group_by(province, hospital, scrdate) %>%
  tally()

df_scrm <- tblSection1 %>%
  mutate(scrdate = floor_date(s1screendate, "month")) %>%
  group_by(province, hospital, scrdate) %>%
  tally()

df_scrage0 <- tblSection1 %>%
  summarize(
    n = n(),
    min = min(s1age_year, na.rm = TRUE),
    q1 = quantile(s1age_year, 0.25, na.rm = TRUE),
    median = median(s1age_year, na.rm = TRUE),
    mean = round(mean(s1age_year, na.rm = TRUE), 1),
    q3 = quantile(s1age_year, 0.75, na.rm = TRUE),
    max = max(s1age_year, na.rm = TRUE)
  )

df_scrage1 <- tblSection1 %>%
  group_by(province) %>%
  summarize(
    n = n(),
    min = min(s1age_year, na.rm = TRUE),
    q1 = quantile(s1age_year, 0.25, na.rm = TRUE),
    median = median(s1age_year, na.rm = TRUE),
    mean = round(mean(s1age_year, na.rm = TRUE), 1),
    q3 = quantile(s1age_year, 0.75, na.rm = TRUE),
    max = max(s1age_year, na.rm = TRUE)
  )

df_scrage2 <- tblSection1 %>%
  group_by(hospital) %>%
  summarize(
    n = n(),
    min = min(s1age_year, na.rm = TRUE),
    q1 = quantile(s1age_year, 0.25, na.rm = TRUE),
    median = median(s1age_year, na.rm = TRUE),
    mean = round(mean(s1age_year, na.rm = TRUE), 1),
    q3 = quantile(s1age_year, 0.75, na.rm = TRUE),
    max = max(s1age_year, na.rm = TRUE)
  )

df_scrage <- tblSection1 %>%
  group_by(province, hospital, agegroup) %>%
  tally()

df_scrgender <- tblSection1 %>%
  group_by(province, hospital, s1gender) %>%
  tally()

#-------------------------------------------------------------------------------
# Enrollment page
#-------------------------------------------------------------------------------

df_eliw <- tblSection1 %>%
  filter(s1eligible == TRUE) %>% # Eligible only
  mutate(scrdate = floor_date(s1screendate, "week", week_start = 1)) %>%
  group_by(province, hospital, rps, scrdate) %>%
  tally()

df_elim <- tblSection1 %>%
  filter(s1eligible == TRUE) %>% # Eligible only
  mutate(scrdate = floor_date(s1screendate, "month")) %>%
  group_by(province, hospital, rps, scrdate) %>%
  tally()

df_enrw <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "week", week_start = 1)) %>%
  group_by(province, hospital, rps, enrdate, finalresult) %>%
  tally()

df_enrm <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "month")) %>%
  group_by(province, hospital, rps, enrdate, finalresult) %>%
  tally()

df_pos3wk <- CFMast %>%
  filter(testdate > Sys.Date() - 22) %>%
  group_by(province, hospital, rps, finalresult) %>%
  tally()

df_enrage <- CFMast %>%
  group_by(province, hospital, rps, agegroup) %>%
  tally()

df_enrgender <- CFMast %>%
  group_by(province, hospital, rps, s1gender) %>%
  tally()

df_enrocc <- CFMast %>%
  mutate(s34occupation = replace(s34occupation, s34occupation == "Other farmer", "Farmer")) %>%
  group_by(province, hospital, rps, s34occupation) %>%
  tally()

#-------------------------------------------------------------------------------
# Diagnosis page
#-------------------------------------------------------------------------------

df_dx <- CFMast %>%
  select(province,
         hospital,
         rps,
         finalresult,
         s2dxfever:s2dxother,
         s2dxmeningitis) %>%
  droplevels() %>% 
  rename_with( ~ str_replace(., "s2dx", ""), starts_with("s2dx")) %>%
  rename(
    Fever                      = fever,
    Abscess                    = abscess,
    "Common cold"              = comcold,
    HIV                        = hiv,
    Dengue                     = dengue,
    "Bacterial infection"      = bacinfect,
    Dyspepsia                  = dyspepsia,
    Shock                      = shock,
    Cancer                     = cancer,
    "Heart diseases"           = heartdis,
    Rash                       = rash,
    Melioidosis                = melioidosis,
    Malaria                    = malaria,
    Diabetes                   = diabetes,
    Anemia                     = anemia,
    "Abdominal pain"           = abpain,
    UTI                        = uti,
    Thalassemia                = thalassemia,
    Fatigue                    = fatigue,
    Arthritis                  = arthritis,
    Sepsis                     = sepsis,
    Cellulitis                 = cellulitis,
    "Cerebrovascular accident" = cerebacci,
    Hepatitis                  = hepatitis,
    "Chikungunya"              = chikun,
    TB                         = tb,
    Hypertension               = hypertension,
    URI                        = uri,
    "Gastrointestinal tract infection" = gastroinfect,
    "Renal diseases"           = renal,
    "Viral infection"          = viralinfect,
    Headache                   = headache,
    "Alteration of conscious"  = alterconscious,
    SIRS                       = sirs,
    COPD                       = copd,
    "Electrolyte imbalance"    = elecimbalance,
    Leptospirosis              = leptospirosis,
    Influenza                  = influenza,
    "Scrub typhus"             = scrubtyphus,
    Other                      = other,
    Meningitis                 = meningitis
  )

ls_dx    <- get_sum_data(df_dx)
ls_dx_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom"))
ls_dx_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom"))
ls_dx_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram"))
ls_dx_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom"))
ls_dx_t  <- get_sum_data(df_dx %>% filter(province == "Tak"))
ls_dx_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot"))
ls_dx_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang"))
ls_dx_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang"))

ls_dx_rps    <- get_sum_data(df_dx %>% filter(rps == TRUE))
ls_dx_rps_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom" & rps == TRUE))
ls_dx_rps_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
ls_dx_rps_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
ls_dx_rps_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom" & rps == TRUE))
ls_dx_rps_t  <- get_sum_data(df_dx %>% filter(province == "Tak" & rps == TRUE))
ls_dx_rps_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot" & rps == TRUE))
ls_dx_rps_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang" & rps == TRUE))
ls_dx_rps_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

ls_dx_norps    <- get_sum_data(df_dx %>% filter(rps == FALSE))
ls_dx_norps_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom" & rps == FALSE))
ls_dx_norps_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
ls_dx_norps_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
ls_dx_norps_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom" & rps == FALSE))
ls_dx_norps_t  <- get_sum_data(df_dx %>% filter(province == "Tak" & rps == FALSE))
ls_dx_norps_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot" & rps == FALSE))
ls_dx_norps_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang" & rps == FALSE))
ls_dx_norps_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

# gt_dx    <- create_sum_table(ls_dx$df_sum, '', 'Diagnoses', ls_dx$N0, ls_dx$N1, ls_dx$N2)

#-------------------------------------------------------------------------------
# Clinical Sign
#-------------------------------------------------------------------------------

df_sign <- CFMast %>%
  select(
    province,
    hospital,
    rps,
    finalresult,
    s2temp,
    s32headache,
    s32neckstiff,
    s32tiredness,
    s32malaise,
    s32chills,
    s32eyepain,
    s32redeyes,
    s32yelloweyes,
    s32nosebleeding,
    s32hyposmia,
    s32dysgeusia,
    s32musclepain,
    s32jointpain,
    s32redjoints,
    s32bonepain,
    s32backpain,
    s32chestpain,
    s32noappetite,
    s32nausea,
    s32vomiting,
    s32bloodvomit,
    s32abdominalpain,
    s32diarrhea,
    s32bloodstool,
    s32bloodurine,
    s32dysuria,
    s32paleskin,
    s32rash,
    s32bruise,
    s32seizures,
    s32other
  ) %>%
  droplevels() %>%
  mutate(s2temp = ifelse(s2temp >= 38, TRUE, FALSE)) %>%
  rename_with( ~ str_replace(., "s32", ""), starts_with("s32")) %>%
  rename(
    "Temperature >= 38.0 C" = s2temp,
    Headache                = headache,
    "Stiff neck"            = neckstiff,
    Tiredness               = tiredness,
    Malaise                 = malaise,
    Chills                  = chills,
    "Eye pain"              = eyepain,
    "Red eyes"              = redeyes,
    "Yellow eyes"           = yelloweyes,
    "Nose bleeding"         = nosebleeding,
    Hyposmia                = hyposmia,
    Dysgeusia               = dysgeusia,
    "Muscle pain"           = musclepain,
    "Joint pain"            = jointpain,
    "Red joints"            = redjoints,
    "Bone pain"             = bonepain,
    "Back pain"             = backpain,
    "Chest pain"            = chestpain,
    "No appetite"           = noappetite,
    Nausea                  = nausea,
    Vomiting                = vomiting,
    "Blood vomitting"       = bloodvomit,
    "Abdominal pain"        = abdominalpain,
    Diarrhea                = diarrhea,
    "Blood stool"           = bloodstool,
    "Blood urine"           = bloodurine,
    Dysuria                 = dysuria,
    "Pale skin"             = paleskin,
    Rash                    = rash,
    Bruise                  = bruise,
    Seizures                = seizures,
    Other                   = other
  )

ls_sign    <- get_sum_data(df_sign)
ls_sign_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom"))
ls_sign_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom"))
ls_sign_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram"))
ls_sign_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom"))
ls_sign_t  <- get_sum_data(df_sign %>% filter(province == "Tak"))
ls_sign_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot"))
ls_sign_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang"))
ls_sign_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang"))

ls_sign_rps    <- get_sum_data(df_sign %>% filter(rps == TRUE))
ls_sign_rps_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom" & rps == TRUE))
ls_sign_rps_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
ls_sign_rps_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
ls_sign_rps_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom" & rps == TRUE))
ls_sign_rps_t  <- get_sum_data(df_sign %>% filter(province == "Tak" & rps == TRUE))
ls_sign_rps_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot" & rps == TRUE))
ls_sign_rps_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang" & rps == TRUE))
ls_sign_rps_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

ls_sign_norps    <- get_sum_data(df_sign %>% filter(rps == FALSE))
ls_sign_norps_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom" & rps == FALSE))
ls_sign_norps_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
ls_sign_norps_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
ls_sign_norps_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom" & rps == FALSE))
ls_sign_norps_t  <- get_sum_data(df_sign %>% filter(province == "Tak" & rps == FALSE))
ls_sign_norps_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot" & rps == FALSE))
ls_sign_norps_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang" & rps == FALSE))
ls_sign_norps_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

df_signBox <- CFMast %>%
  select(cfid,
         province,
         hospital,
         rps,
         s5covidpos,
         s5intub,
         s5dischargetype) %>%
  filter(s5covidpos == 1) %>%
  group_by(province, hospital, rps, s5intub, s5dischargetype) %>%
  tally()

#-------------------------------------------------------------------------------
# Underlying Page
#-------------------------------------------------------------------------------

df_un <- CFMast %>%
  select(
    province,
    hospital,
    rps,
    finalresult,
    s35diabetes,
    s35obesity:s35cancer,
    s35hiv:s35othchronic,
    s35hissmoke:s35pregnancy
  ) %>%
  droplevels() %>% 
  rename_with( ~ str_replace(., "s35", ""), starts_with("s35")) %>%
  rename(
    Diabetes = diabetes,
    Obesity = obesity,
    Hypertension = hypertension,
    "Heart diseases" = heartdisease,
    Asthma = asthma,
    COPD = copd,
    Cancer = cancer,
    HIV = hiv,
    "Immunodeficiency" = immunodef,
    "History of TB" = histb,
    "Active TB" = activetb,
    Liver = liver,
    Thyroid = thyroid,
    Thalassemia = thalassemia,
    Anemia = anemia,
    Renal = renal,
    "Chrolesterol" = chroles,
    "Cerebrovascular diseases" = cerebro,
    "Other chronic diseases" = othchronic,
    "History of smoking" = hissmoke,
    "Current smoking" = cursmoke,
    "History of alcohol consumption" = histalcohol,
    "Current of alcohol consumption" = curalcohol,
    Pregnancy = pregnancy
  )

ls_un    <- get_sum_data(df_un)
ls_un_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom"))
ls_un_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom"))
ls_un_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram"))
ls_un_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom"))
ls_un_t  <- get_sum_data(df_un %>% filter(province == "Tak"))
ls_un_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot"))
ls_un_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang"))
ls_un_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang"))

ls_un_rps    <- get_sum_data(df_un %>% filter(rps == TRUE))
ls_un_rps_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom" & rps == TRUE))
ls_un_rps_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
ls_un_rps_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
ls_un_rps_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom" & rps == TRUE))
ls_un_rps_t  <- get_sum_data(df_un %>% filter(province == "Tak" & rps == TRUE))
ls_un_rps_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot" & rps == TRUE))
ls_un_rps_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang" & rps == TRUE))
ls_un_rps_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

ls_un_norps    <- get_sum_data(df_un %>% filter(rps == FALSE))
ls_un_norps_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom" & rps == FALSE))
ls_un_norps_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
ls_un_norps_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
ls_un_norps_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom" & rps == FALSE))
ls_un_norps_t  <- get_sum_data(df_un %>% filter(province == "Tak" & rps == FALSE))
ls_un_norps_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot" & rps == FALSE))
ls_un_norps_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang"))
ls_un_norps_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang"))

#-------------------------------------------------------------------------------
#Risk Factor page
#-------------------------------------------------------------------------------

df_rf <- CFMast %>%
  select(
    province,
    hospital,
    rps,
    finalresult,
    s33suspectedcovid19:s33visithos,
    s33visticrowded:s33travelthai,
    s33travelinter
  ) %>%
  droplevels() %>% 
  mutate(
    "Contact COVID-19 cases" = ifelse(s33suspectedcovid19 == 'Yes', TRUE, FALSE),
    "Contact febrile patients" = ifelse(
      s33febrilehousehold == 'Yes' |
        s33febrilecoworker == 'Yes' |
        s33febrileneighbor == 'Yes',
      TRUE,
      FALSE
    ),
    "Contact persons with respiratory symptoms" = ifelse(s33rps == 'Yes', TRUE, FALSE),
    "Contact pneumonia patients" = ifelse(s33pneumonia == 'Yes', TRUE, FALSE),
    "Contact healthcare workers" = s33healthcareper | s33visithos,
    "Visit crowded places" = s33visticrowded | s33partpeople,
    "History of travel" = s33travelthai | s33travelinter
  ) %>%
  select(
    -c(
      s33suspectedcovid19:s33visithos,
      s33visticrowded:s33travelthai,
      s33travelinter
    )
  )

ls_rf    <- get_sum_data(df_rf)
ls_rf_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom"))
ls_rf_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom"))
ls_rf_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram"))
ls_rf_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom"))
ls_rf_t  <- get_sum_data(df_rf %>% filter(province == "Tak"))
ls_rf_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot"))
ls_rf_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang"))
ls_rf_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang"))

ls_rf_rps    <- get_sum_data(df_rf %>% filter(rps == TRUE))
ls_rf_rps_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom" & rps == TRUE))
ls_rf_rps_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
ls_rf_rps_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
ls_rf_rps_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom" & rps == TRUE))
ls_rf_rps_t  <- get_sum_data(df_rf %>% filter(province == "Tak" & rps == TRUE))
ls_rf_rps_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot" & rps == TRUE))
ls_rf_rps_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang" & rps == TRUE))
ls_rf_rps_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

ls_rf_norps    <- get_sum_data(df_rf %>% filter(rps == FALSE))
ls_rf_norps_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom" & rps == FALSE))
ls_rf_norps_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
ls_rf_norps_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
ls_rf_norps_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom" & rps == FALSE))
ls_rf_norps_t  <- get_sum_data(df_rf %>% filter(province == "Tak" & rps == FALSE))
ls_rf_norps_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot" & rps == FALSE))
ls_rf_norps_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang" & rps == FALSE))
ls_rf_norps_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

#-------------------------------------------------------------------------------
# Vaccination page
#-------------------------------------------------------------------------------

df_vac <- CFMast %>%
  mutate(
    vac = factor(
      # Patients were considered fully vaccinated if completed at least 1 month prior to enrol
      if_else(s1enrolldate - fulltime < 30, 1, cv),
      levels = c(0:3),
      labels = c(
        'Unvaccinated',
        'Partially vaccinated',
        'Fully vaccinated',
        'Unknown'
      )
    ),
    finalresult = fct_na_value_to_level(finalresult, level = "Unknown")
  ) %>%
  group_by(province, hospital, rps, vac, finalresult) %>%
  tally()

#-------------------------------------------------------------------------------
# Atk page
#-------------------------------------------------------------------------------
df_atk <- CFMast %>% 
  filter(s33atk == 'Yes') %>%
  select(cfid,
         province,
         hospital,
         rps,
         s33atk,
         s33atkresult1,
         s33atkresult2) %>%
  mutate(atkresult = factor(
    case_when(
      s33atkresult1 == 'Positive' | s33atkresult2 == 'Positive' ~ 'Positive',
      s33atkresult1 == 'Negative' | s33atkresult2 == 'Negative' ~ 'Negative',
      TRUE                                                      ~ 'Unknown'
    ),
    levels = c('Positive', 'Negative', 'Unknown')
  )) %>%
  group_by(province, hospital, rps, atkresult) %>%
  tally()

#-------------------------------------------------------------------------------
# SARS-Cov2 detection page
#-------------------------------------------------------------------------------

df_lab <- LabPCRResult_l %>%
  left_join(tblSection1 %>% select(cfid, rps), by="cfid") %>% 
  group_by(province, hospital, rps, spectype, finalresult_fac) %>%
  tally() %>%
  rename(finalresult = finalresult_fac)

df_labenr <- LabPCRResult_l %>%
  inner_join(CFMast %>% select(cfid, rps), by='cfid') %>%   
  group_by(province, hospital, rps, spectype, finalresult_fac) %>%
  tally() %>%
  rename(finalresult = finalresult_fac)

df_labpos <- LabPCRResult_l %>%
  left_join(tblSection1 %>% select(cfid, rps), by="cfid") %>% 
  # Select only 4 columns (ID, type, result, and test dates)
  select(cfid,
         province,
         hospital,
         rps,
         spectype,
         finalresult) %>%
  # All specimen types in one line
  pivot_wider(names_from = "spectype",
              values_from = "finalresult") %>%
  filter(`NP/OP swab` == 1 | `Nasal swab` == 1 | Saliva == 1) %>%
  mutate(
    specimens = case_when(
      `NP/OP swab`   == 1 & 
        `Nasal swab` == 1 & 
        `Saliva`     == 1   ~ "NP/OP, nasal, saliva",
      `NP/OP swab`   == 1 & 
        `Nasal swab` == 1   ~ "NP/OP, nasal",
      `NP/OP swab`   == 1 & 
        `Saliva`     == 1   ~ "NP/OP, saliva",
      `Nasal swab`   == 1 & 
        `Saliva`     == 1   ~ "Nasal, saliva",
      `NP/OP swab`   == 1   ~ "NP/OP",
      `Nasal swab`   == 1   ~ "Nasal",
      `Saliva`       == 1   ~ "Saliva",
      TRUE                  ~ "other"
    )
  ) %>%
  group_by(province, hospital, rps, specimens) %>%
  tally()

df_labposenr <- LabPCRResult_l %>%
  inner_join(CFMast %>% select(cfid, rps), by='cfid') %>%   
  # Select only 4 columns (ID, type, result, and test dates)
  select(cfid,
         province,
         hospital,
         rps,
         spectype,
         finalresult) %>%
  # All specimen types in one line
  pivot_wider(names_from = "spectype",
              values_from = "finalresult") %>%
  filter(`NP/OP swab` == 1 | `Nasal swab` == 1 | Saliva == 1) %>%
  mutate(
    specimens = case_when(
      `NP/OP swab`   == 1 & 
        `Nasal swab` == 1 & 
        `Saliva`     == 1   ~ "NP/OP, nasal, saliva",
      `NP/OP swab`   == 1 & 
        `Nasal swab` == 1   ~ "NP/OP, nasal",
      `NP/OP swab`   == 1 & 
        `Saliva`     == 1   ~ "NP/OP, saliva",
      `Nasal swab`   == 1 & 
        `Saliva`     == 1   ~ "Nasal, saliva",
      `NP/OP swab`   == 1   ~ "NP/OP",
      `Nasal swab`   == 1   ~ "Nasal",
      `Saliva`       == 1   ~ "Saliva",
      TRUE                  ~ "other"
    )
  ) %>%
  group_by(province, hospital, rps, specimens) %>%
  tally()

#-------------------------------------------------------------------------------
# Serology testing page
#-------------------------------------------------------------------------------
dfsero1a <- 
  tblSection1 %>% filter(!is.na(cfid) & s1enroll5th == TRUE) %>%
  left_join(LabPCRFinal, by = 'cfid') %>%
  left_join(LabSero %>% filter(spectype == 'A'), by = 'cfid') %>%
  # Create or modify variables as necessary
  mutate(
    finalresult = droplevels(finalresult)  # Drop unused level (Indeterminate)
  ) %>% 
  # Select only variables to be used
  select(
    province,
    hospital,
    finalresult,
    igminterpret,
    igginterpret,
    iggquantiinterpret
  ) %>%
  # Set variable labels to be displayed in output
  set_variable_labels(
    igminterpret = 'IgM',
    igginterpret = 'IgG-N',
    iggquantiinterpret = 'IgG-S'
  )

dfsero1b <-
  tblSection1 %>% filter(!is.na(cfid) & s1enroll5th == TRUE) %>%
  left_join(LabPCRFinal, by = 'cfid') %>%
  inner_join(LabSero %>%
               filter(spectype == 'A') %>%
               filter(!is.na(igminterpret) | !is.na(igginterpret) | !is.na(iggquantiinterpret)),
             by = 'cfid') %>%
  left_join(tblSection3, by = 'cfid') %>%
  # Create or modify variables as necessary
  mutate(
    agegrp = cut(
      s1age_year,
      breaks = c(0, 17, 59, Inf),
      labels = c('2-17', '18-59', '>=60'),
      include.lowest = TRUE
    ),
    comorbid = rowSums(
      across(c(s35diabetes:s35pregnancy) & where(is.logical)),
      na.rm = TRUE) >= 1,
    jj = rowSums(across(num_range("s33cvname", 1:10)) == 2, na.rm = TRUE),
    vv = rowSums(across(num_range("s33cvname", 1:10)) == 1 |
                 across(num_range("s33cvname", 1:10)) == 7, na.rm = TRUE),
    mr = rowSums(across(num_range("s33cvname", 1:10)) == 3 |
                 across(num_range("s33cvname", 1:10)) == 4, na.rm = TRUE),
    iv = rowSums(across(num_range("s33cvname", 1:10)) == 5 |
                 across(num_range("s33cvname", 1:10)) == 6, na.rm = TRUE),
    ot = rowSums(across(num_range("s33cvname", 1:10)) == 9 |
                 across(num_range("s33cvname", 1:10)) == 10, na.rm = TRUE),
    cv = factor(
      case_when(
        s33covidvaccine == FALSE ~ 0,
        jj >= 1 | vv >= 2 | mr >= 2 | iv >= 2 ~ 2,
        vv == 1 | mr == 1 | iv == 1 ~ 1,
        ot >= 1 ~ 3,
        TRUE ~ NA_real_
      ),
      levels = 0:3,
      labels = c('None', 'Not fully vaccinated', 'Fully vaccinated', 'Vaccinated but no information')
    ),
    cvdate_l = pmax(s33cvdate1, s33cvdate2, s33cvdate3, s33cvdate4, s33cvdate5, s33cvdate6, na.rm = TRUE),
    cvtime = as.numeric(difftime(s1enrolldate, cvdate_l, units = 'days'))
  ) %>%
  # Select only variables to be used
  select(
    province,
    hospital,
    finalresult,
    igminterpret,
    igginterpret,
    iggquantiinterpret,
    agegrp,
    comorbid,
    s35obesity,
    s35histalcohol,
    s35curalcohol,
    s35hissmoke,
    s35hypertension,
    s35cursmoke,
    s35diabetes,
    s35chroles,
    s35asthma,
    s35anemia,
    cv,
    cvtime,
    s33hascovid
  ) %>%
  # Set variable labels to be displayed in output
  set_variable_labels(
    agegrp          = 'Age group',
    comorbid        = 'Comorbidity',
    s35obesity      = 'Obesity',
    s35histalcohol  = 'History of alcohol consumption',
    s35curalcohol   = 'Current alcohol consumption',
    s35hissmoke     = 'History of smoking',
    s35hypertension = 'Hypertension',
    s35cursmoke     = 'Current smoking',
    s35diabetes     = 'Diabetes',
    s35chroles      = 'Chrolesterol',
    s35asthma       = 'Asthma',
    s35anemia       = 'Anemia',
    cv              = 'Covid vaccine',
    cvtime          = 'Time since last vaccine dose',
    s33hascovid     = 'Previous infected with COVID-19'
  )

dfsero2a <-
  tblSection1 %>% filter(!is.na(cfid)) %>%
  semi_join(LabPCRFinal %>% filter(finalresult == 'Positive'), by = 'cfid') %>%
  left_join(LabSero_l, by = 'cfid') %>%
  left_join(tblSection3, by = 'cfid') %>%
  # Create or modify variables as necessary
  mutate(
    agegrp = cut(
      s1age_year,
      breaks = c(0, 17, 59, Inf),
      labels = c('2-17', '18-59', '>=60'),
      include.lowest = TRUE
    ),
    comorbid = rowSums(
      across(c(s35diabetes:s35pregnancy) & where(is.logical)),
      na.rm = TRUE) >= 1,
    az = rowSums(across(num_range("s33cvname", 1:10)) == 1, na.rm = TRUE),
    jj = rowSums(across(num_range("s33cvname", 1:10)) == 2, na.rm = TRUE),
    md = rowSums(across(num_range("s33cvname", 1:10)) == 3, na.rm = TRUE),
    pz = rowSums(across(num_range("s33cvname", 1:10)) == 4, na.rm = TRUE),
    sv = rowSums(across(num_range("s33cvname", 1:10)) == 5, na.rm = TRUE),
    sp = rowSums(across(num_range("s33cvname", 1:10)) == 6, na.rm = TRUE),
    sn = rowSums(across(num_range("s33cvname", 1:10)) == 7, na.rm = TRUE),
    ot = rowSums(across(num_range("s33cvname", 1:10)) == 9 |
                   across(num_range("s33cvname", 1:10)) == 10, na.rm = TRUE),
    cv = factor(
      case_when(
        s33covidvaccine == FALSE ~ 0,
        az + sn >= 2 | jj >= 1 | md + pz >= 2 | sv + sp >= 2 ~ 2,
        az + sn == 1 | md + pz == 1 | sv + sp == 1 ~ 1,
        ot >= 1 ~ 3,
        TRUE ~ NA_real_
      ),
      levels = 0:3,
      labels = c('None', 'Not fully vaccinated', 'Fully vaccinated', 'Vaccinated but no information')
    ),
    numdose = cut(
      s33cvamount,
      breaks = c(0, 1, 2, Inf),
      labels = c('< 2 doses', '2 doses', '> 2 doses'),
      include.lowest = TRUE
    ),
    cvdate_l = pmax(s33cvdate1, s33cvdate2, s33cvdate3, s33cvdate4, s33cvdate5, s33cvdate6, na.rm = TRUE),
    cvtime = as.numeric(difftime(s1enrolldate, cvdate_l, units = 'days')),
    
    # JJ
    jjdate1 = pmin(
      if_else(s33cvname1 == 2, s33cvdate1, NA_Date_),
      if_else(s33cvname2 == 2, s33cvdate2, NA_Date_),
      if_else(s33cvname3 == 2, s33cvdate3, NA_Date_),
      if_else(s33cvname4 == 2, s33cvdate4, NA_Date_),
      if_else(s33cvname5 == 2, s33cvdate5, NA_Date_),
      if_else(s33cvname6 == 2, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # Viral vector 1
    vvdate1 = pmin(
      if_else(s33cvname1 %in% c(1, 7), s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% c(1, 7), s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% c(1, 7), s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% c(1, 7), s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% c(1, 7), s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% c(1, 7), s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # Viral vector 2
    vvdate2 = pmin(
      if_else(s33cvname1 %in% c(1, 7) & s33cvdate1 > vvdate1, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% c(1, 7) & s33cvdate2 > vvdate1, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% c(1, 7) & s33cvdate3 > vvdate1, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% c(1, 7) & s33cvdate4 > vvdate1, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% c(1, 7) & s33cvdate5 > vvdate1, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% c(1, 7) & s33cvdate6 > vvdate1, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # mRNA 1
    mrdate1 = pmin(
      if_else(s33cvname1 %in% 3:4, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 3:4, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 3:4, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 3:4, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 3:4, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 3:4, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # mRNA 2
    mrdate2 = pmin(
      if_else(s33cvname1 %in% 3:4 & s33cvdate1 > mrdate1, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 3:4 & s33cvdate2 > mrdate1, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 3:4 & s33cvdate3 > mrdate1, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 3:4 & s33cvdate4 > mrdate1, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 3:4 & s33cvdate5 > mrdate1, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 3:4 & s33cvdate6 > mrdate1, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # inactivated virus 1
    ivdate1 = pmin(
      if_else(s33cvname1 %in% 5:6, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 5:6, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 5:6, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 5:6, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 5:6, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 5:6, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    # inactivated virus 2
    ivdate2 = pmin(
      if_else(s33cvname1 %in% 5:6 & s33cvdate1 > ivdate1, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 5:6 & s33cvdate2 > ivdate1, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 5:6 & s33cvdate3 > ivdate1, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 5:6 & s33cvdate4 > ivdate1, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 5:6 & s33cvdate5 > ivdate1, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 5:6 & s33cvdate6 > ivdate1, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    fulldate = pmin(jjdate1, vvdate2, mrdate2, ivdate2, na.rm = TRUE),
    fulltime = as.numeric(difftime(s1enrolldate, fulldate, units = 'days')),
    
    igm_o = if_else(rowSums(across(starts_with('igm_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative'),
    iggn_o = if_else(rowSums(across(starts_with('iggn_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative'),
    iggs_o = if_else(rowSums(across(starts_with('iggs_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative')
  ) %>%
  # Select only variables to be used
  select(
    igm_o,
    igm_11,
    igm_91,
    iggn_o,
    iggn_11,
    iggn_91,
    iggs_o,
    iggs_11,
    iggs_91,
    agegrp,
    s1gender,
    comorbid,
    cv,
    numdose,
    fulltime,
    cvtime,
    s33hascovid
  ) %>%
  # Set variable labels to be displayed in output
  set_variable_labels(
    agegrp          = 'Age group',
    s1gender        = 'Gender',
    comorbid        = 'Comorbidity',
    cv              = 'Covid vaccine',
    numdose         = 'Number of vaccine dose',
    fulltime        = 'Time since fully vaccinated', 
    cvtime          = 'Time since last vaccine dose',
    s33hascovid     = 'Previous infected with COVID-19'
  )
dfsero2b <-
  tblSection1 %>% filter(!is.na(cfid)) %>%
  semi_join(LabPCRFinal %>% filter(finalresult == 'Positive'), by = 'cfid') %>%
  left_join(LabSero_l, by = 'cfid') %>%
  left_join(tblSection3, by = 'cfid') %>%
  # Create or modify variables as necessary
  mutate(
    agegrp = cut(
      s1age_year,
      breaks = c(0, 17, 59, Inf),
      labels = c('2-17', '18-59', '>=60'),
      include.lowest = TRUE
    ),
    comorbid = rowSums(
      across(c(s35diabetes:s35pregnancy) & where(is.logical)),
      na.rm = TRUE) >= 1,
    az = rowSums(across(num_range("s33cvname", 1:10)) == 1, na.rm = TRUE),
    jj = rowSums(across(num_range("s33cvname", 1:10)) == 2, na.rm = TRUE),
    md = rowSums(across(num_range("s33cvname", 1:10)) == 3, na.rm = TRUE),
    pz = rowSums(across(num_range("s33cvname", 1:10)) == 4, na.rm = TRUE),
    sv = rowSums(across(num_range("s33cvname", 1:10)) == 5, na.rm = TRUE),
    sp = rowSums(across(num_range("s33cvname", 1:10)) == 6, na.rm = TRUE),
    sn = rowSums(across(num_range("s33cvname", 1:10)) == 7, na.rm = TRUE),
    ot = rowSums(across(num_range("s33cvname", 1:10)) == 9 |
                   across(num_range("s33cvname", 1:10)) == 10, na.rm = TRUE),
    cv = factor(
      case_when(
        s33covidvaccine == FALSE ~ 0,
        az + sn >= 2 | jj >= 1 | md + pz >= 2 | sv + sp >= 2 ~ 2,
        az + sn == 1 | md + pz == 1 | sv + sp == 1 ~ 1,
        ot >= 1 ~ 3,
        TRUE ~ NA_real_
      ),
      levels = 0:3,
      labels = c('None', 'Not fully vaccinated', 'Fully vaccinated', 'Vaccinated but no information')
    ),
    numdose = cut(
      s33cvamount,
      breaks = c(0, 1, 2, Inf),
      labels = c('< 2 doses', '2 doses', '> 2 doses'),
      include.lowest = TRUE
    ),
    tigsfold = iggsq_91/iggsq_11
    
  ) %>%
  # Select only variables to be used
  select(
    iggsq_11,
    iggsq_91,
    agegrp,
    s1gender,
    comorbid,
    cv,
    numdose,
    s33hascovid,
    tigsfold
  ) %>%
  # Set variable labels to be displayed in output
  set_variable_labels(
    agegrp          = 'Age group',
    s1gender        = 'Gender',
    comorbid        = 'Comorbidity',
    cv              = 'Covid vaccine',
    numdose         = 'Number of vaccine dose',
    s33hascovid     = 'Previous infected with COVID-19'
  )
#-------------------------------------------------------------------------------
# KAP page
#-------------------------------------------------------------------------------

df_kap1 <- CFMast %>%
  select(province, hospital, rps,
         s3604sickspread, s3615carelate:s3620) %>%
  rename(s3604 = s3604sickspread,
         s3615 = s3615carelate) %>% 
  pivot_longer(cols = s3604:s3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, rps, kap, scale) %>%
  tally()

df_kap2 <- CFMast %>%
  select(province, hospital, rps,
         s3610maskin, s3613maskout, s3621:s3622) %>%
  rename(s3610 = s3610maskin,
         s3613 = s3613maskout) %>% 
  pivot_longer(cols = s3610:s3622,
               names_to = "kap",
               values_to = "scale") %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, rps, kap, scale) %>%
  tally()

#-------------------------------------------------------------------------------
# Save data frames for dashboard in one data file (CFDashboard.RData)
#-------------------------------------------------------------------------------

save(
  list = c(
    "ddate",
    "df_scrw",
    "df_scrm",
    "df_scrage",
    "df_scrage0",
    "df_scrage1",
    "df_scrage2",
    "df_scrgender",
    "df_eliw",
    "df_elim",
    "df_enrw",
    "df_enrm",
    "df_pos3wk",
    "df_enrage",
    "df_enrgender",
    "df_enrocc",
    "ls_dx",
    "ls_dx_n",
    "ls_dx_n1",
    "ls_dx_n2",
    "ls_dx_n3",
    "ls_dx_t",
    "ls_dx_t1",
    "ls_dx_t2",
    "ls_dx_t3",
    "ls_dx_rps",
    "ls_dx_rps_n",
    "ls_dx_rps_n1",
    "ls_dx_rps_n2",
    "ls_dx_rps_n3",
    "ls_dx_rps_t",
    "ls_dx_rps_t1",
    "ls_dx_rps_t2",
    "ls_dx_rps_t3",
    "ls_dx_norps",
    "ls_dx_norps_n",
    "ls_dx_norps_n1",
    "ls_dx_norps_n2",
    "ls_dx_norps_n3",
    "ls_dx_norps_t",
    "ls_dx_norps_t1",
    "ls_dx_norps_t2",
    "ls_dx_norps_t3",
    "ls_sign",
    "ls_sign_n",
    "ls_sign_n1",
    "ls_sign_n2",
    "ls_sign_n3",
    "ls_sign_t",
    "ls_sign_t1",
    "ls_sign_t2",
    "ls_sign_t3",
    "ls_sign_rps",
    "ls_sign_rps_n",
    "ls_sign_rps_n1",
    "ls_sign_rps_n2",
    "ls_sign_rps_n3",
    "ls_sign_rps_t",
    "ls_sign_rps_t1",
    "ls_sign_rps_t2",
    "ls_sign_rps_t3",
    "ls_sign_norps",
    "ls_sign_norps_n",
    "ls_sign_norps_n1",
    "ls_sign_norps_n2",
    "ls_sign_norps_n3",
    "ls_sign_norps_t",
    "ls_sign_norps_t1",
    "ls_sign_norps_t2",
    "ls_sign_norps_t3",
    "df_signBox",
    "ls_un",
    "ls_un_n",
    "ls_un_n1",
    "ls_un_n2",
    "ls_un_n3",
    "ls_un_t",
    "ls_un_t1",
    "ls_un_t2",
    "ls_un_t3",
    "ls_un_rps",
    "ls_un_rps_n",
    "ls_un_rps_n1",
    "ls_un_rps_n2",
    "ls_un_rps_n3",
    "ls_un_rps_t",
    "ls_un_rps_t1",
    "ls_un_rps_t2",
    "ls_un_rps_t3",
    "ls_un_norps",
    "ls_un_norps_n",
    "ls_un_norps_n1",
    "ls_un_norps_n2",
    "ls_un_norps_n3",
    "ls_un_norps_t",
    "ls_un_norps_t1",
    "ls_un_norps_t2",
    "ls_un_norps_t3",
    "ls_rf",
    "ls_rf_n",
    "ls_rf_n1",
    "ls_rf_n2",
    "ls_rf_n3",
    "ls_rf_t",
    "ls_rf_t1",
    "ls_rf_t2",
    "ls_rf_t3",
    "ls_rf_rps",
    "ls_rf_rps_n",
    "ls_rf_rps_n1",
    "ls_rf_rps_n2",
    "ls_rf_rps_n3",
    "ls_rf_rps_t",
    "ls_rf_rps_t1",
    "ls_rf_rps_t2",
    "ls_rf_rps_t3",
    "ls_rf_norps",
    "ls_rf_norps_n",
    "ls_rf_norps_n1",
    "ls_rf_norps_n2",
    "ls_rf_norps_n3",
    "ls_rf_norps_t",
    "ls_rf_norps_t1",
    "ls_rf_norps_t2",
    "ls_rf_norps_t3",
    "df_vac",
    "df_atk",
    "df_lab",
    "df_labenr",
    "df_labpos",
    "df_labposenr",
    "dfsero1a",
    "dfsero1b",
    "dfsero2a",
    "dfsero2b",
    "df_kap1",
    "df_kap2"
  ),
  file = paste0(data_folder, "/CFDashboard.RData")
)
