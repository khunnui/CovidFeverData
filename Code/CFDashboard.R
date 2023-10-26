#------------------------------------------------------------------------------#
# Description: Program for prepare data frames and gt tables for Covid-Fever   #
#              dashboard https://dghp.shinyapps.io/COVID-Fever/  	  	         #
# Author:      hpy1                                                            #
# Created:     January 27, 2022                                                #
# Modified:    March 9, 2022                                                   #
#------------------------------------------------------------------------------#

library(labelled)
library(gtsummary)
library(gt)
#detach("package:gtExtras", unload = TRUE)
devtools::install_github("Nartlada/gtExtras")
library(gtExtras)
library(rstatix)

# Output theme
theme_gtsummary_compact()

date1    <- max(tblSection1$last_edit_date, na.rm = TRUE)
date2    <- max(tblSection2$last_edit_date, na.rm = TRUE)
date3    <- max(tblSection3$last_edit_date, na.rm = TRUE)
date4    <- max(tblSection4$last_edit_date, na.rm = TRUE)
date5    <- max(tblSection5$last_edit_date, na.rm = TRUE)
datepcr  <- max(LabPCRResult_l$approvedate, na.rm = TRUE)
ddate    <- max(c(date1, date2, date3, date4, date5, datepcr), na.rm = TRUE)

lcdate1    <- max(lcsec1$last_edit_date, na.rm = TRUE)
lcdate2    <- max(lcsec2$last_edit_date, na.rm = TRUE)
#lcdate3    <- max(tblSection3$last_edit_date, na.rm = TRUE)

lcddate    <- max(c(lcdate1, lcdate2), na.rm = TRUE)
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
  ) %>% 
  filter(finalresult %in% c('Positive', 'Negative'))

# ls_dx    <- get_sum_data(df_dx)
# ls_dx_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom"))
# ls_dx_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom"))
# ls_dx_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram"))
# ls_dx_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom"))
# ls_dx_t  <- get_sum_data(df_dx %>% filter(province == "Tak"))
# ls_dx_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot"))
# ls_dx_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang"))
# ls_dx_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang"))

# ls_dx_rps    <- get_sum_data(df_dx %>% filter(rps == TRUE))
# ls_dx_rps_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom" & rps == TRUE))
# ls_dx_rps_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
# ls_dx_rps_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
# ls_dx_rps_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom" & rps == TRUE))
# ls_dx_rps_t  <- get_sum_data(df_dx %>% filter(province == "Tak" & rps == TRUE))
# ls_dx_rps_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot" & rps == TRUE))
# ls_dx_rps_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang" & rps == TRUE))
# ls_dx_rps_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

# ls_dx_norps    <- get_sum_data(df_dx %>% filter(rps == FALSE))
# ls_dx_norps_n  <- get_sum_data(df_dx %>% filter(province == "Nakorn Phanom" & rps == FALSE))
# ls_dx_norps_n1 <- get_sum_data(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
# ls_dx_norps_n2 <- get_sum_data(df_dx %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
# ls_dx_norps_n3 <- get_sum_data(df_dx %>% filter(hospital == "That Phanom" & rps == FALSE))
# ls_dx_norps_t  <- get_sum_data(df_dx %>% filter(province == "Tak" & rps == FALSE))
# ls_dx_norps_t1 <- get_sum_data(df_dx %>% filter(hospital == "Mae Sot" & rps == FALSE))
# ls_dx_norps_t2 <- get_sum_data(df_dx %>% filter(hospital == "Umphang" & rps == FALSE))
# ls_dx_norps_t3 <- get_sum_data(df_dx %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

head = 'Diagnoses'

gt_dx    <- create_table(df_dx, '', head)
gt_dx_n  <- create_table(df_dx %>% filter(province == 'Nakorn Phanom'), 'Nakorn Phanom Province', head)
gt_dx_n1 <- create_table(df_dx %>% filter(hospital == 'Nakorn Phanom'), 'Nakorn Phanom Hospital', head)
gt_dx_n2 <- create_table(df_dx %>% filter(hospital == 'Sri Songkhram'), 'Sri Songkhram Hospital', head)
gt_dx_n3 <- create_table(df_dx %>% filter(hospital == 'That Phanom'), 'That Phanom Hospital', head)
gt_dx_t  <- create_table(df_dx %>% filter(province == 'Tak'), 'Tak Province', head)
gt_dx_t1 <- create_table(df_dx %>% filter(hospital == 'Mae Sot'), 'Mae Sot Hospital', head)
gt_dx_t2 <- create_table(df_dx %>% filter(hospital == 'Umphang'), 'Umphang Hospital', head)
gt_dx_t3 <- create_table(df_dx %>% filter(hospital == 'Tha Song Yang'), 'Tha Song Yang Hospital', head)

# rpstext = '(RPS Only)'
# gt_dx_rps    <- create_table(df_dx %>% filter(rps == TRUE), rpstext, head)
# gt_dx_rps_n  <- create_table(df_dx %>% filter(province == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Province", rpstext), head)
# gt_dx_rps_n1 <- create_table(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_dx_rps_n2 <- create_table(df_dx %>% filter(hospital == "Sri Songkhram" & rps == TRUE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_dx_rps_n3 <- create_table(df_dx %>% filter(hospital == "That Phanom" & rps == TRUE), paste("That Phanom Hospital", rpstext), head)
# gt_dx_rps_t  <- create_table(df_dx %>% filter(province == "Tak" & rps == TRUE), paste("Tak Province", rpstext), head)
# gt_dx_rps_t1 <- create_table(df_dx %>% filter(hospital == "Mae Sot" & rps == TRUE), paste("Mae Sot Hospital", rpstext), head)
# gt_dx_rps_t2 <- create_table(df_dx %>% filter(hospital == "Umphang" & rps == TRUE), paste("Umphang Hospital", rpstext), head)
# gt_dx_rps_t3 <- create_table(df_dx %>% filter(hospital == "Tha Song Yang" & rps == TRUE), paste("Tha Song Yang Hospital", rpstext), head)

# rpstext = '(Non-RPS Only)'
# gt_dx_norps    <- create_table(df_dx %>% filter(rps == FALSE), rpstext, head)
# gt_dx_norps_n  <- create_table(df_dx %>% filter(province == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Province", rpstext), head)
# gt_dx_norps_n1 <- create_table(df_dx %>% filter(hospital == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_dx_norps_n2 <- create_table(df_dx %>% filter(hospital == "Sri Songkhram" & rps == FALSE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_dx_norps_n3 <- create_table(df_dx %>% filter(hospital == "That Phanom" & rps == FALSE), paste("That Phanom Hospital", rpstext), head)
# gt_dx_norps_t  <- create_table(df_dx %>% filter(province == "Tak" & rps == FALSE), paste("Tak Province", rpstext), head)
# gt_dx_norps_t1 <- create_table(df_dx %>% filter(hospital == "Mae Sot" & rps == FALSE), paste("Mae Sot Hospital", rpstext), head)
# gt_dx_norps_t2 <- create_table(df_dx %>% filter(hospital == "Umphang" & rps == FALSE), paste("Umphang Hospital", rpstext), head)
# gt_dx_norps_t3 <- create_table(df_dx %>% filter(hospital == "Tha Song Yang" & rps == FALSE), paste("Tha Song Yang Hospital", rpstext), head)

#-------------------------------------------------------------------------------
# Clinical Sign
#-------------------------------------------------------------------------------

df_ss <- CFMast %>%
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
  ) %>% 
  filter(finalresult %in% c('Positive', 'Negative'))

df_ss_b <- 
  tblSection3 %>%
  semi_join(LabPCRFinal %>% filter(finalresult == 'Positive'), by = 'cfid') %>%
  select(s32headache:s32other, -c(ends_with("d"))) %>%
  rename_all(~stringr::str_replace(.,"^s32","")) %>% 
  mutate(   visit ='Baseline') %>% 
  replace(is.na(.), FALSE)

df_ss_f <- 
  tblSection8 %>% filter(s8isfu %in% c(1,3)) %>%
  semi_join(LabPCRFinal %>% filter(finalresult == 'Positive'), by = 'cfid') %>%
  select(s8headache:s8other) %>%
  rename_all(~stringr::str_replace(.,"^s8","")) %>% 
  mutate(    visit ='F/U') %>% 
  replace(is.na(.), FALSE)

# Create or modify variables as necessary

df_ss_bf <- rbind( df_ss_b,  df_ss_f) %>% 
  rename(
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

# ls_sign    <- get_sum_data(df_sign)
# ls_sign_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom"))
# ls_sign_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom"))
# ls_sign_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram"))
# ls_sign_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom"))
# ls_sign_t  <- get_sum_data(df_sign %>% filter(province == "Tak"))
# ls_sign_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot"))
# ls_sign_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang"))
# ls_sign_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang"))

# ls_sign_rps    <- get_sum_data(df_sign %>% filter(rps == TRUE))
# ls_sign_rps_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom" & rps == TRUE))
# ls_sign_rps_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
# ls_sign_rps_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
# ls_sign_rps_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom" & rps == TRUE))
# ls_sign_rps_t  <- get_sum_data(df_sign %>% filter(province == "Tak" & rps == TRUE))
# ls_sign_rps_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot" & rps == TRUE))
# ls_sign_rps_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang" & rps == TRUE))
# ls_sign_rps_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

# ls_sign_norps    <- get_sum_data(df_sign %>% filter(rps == FALSE))
# ls_sign_norps_n  <- get_sum_data(df_sign %>% filter(province == "Nakorn Phanom" & rps == FALSE))
# ls_sign_norps_n1 <- get_sum_data(df_sign %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
# ls_sign_norps_n2 <- get_sum_data(df_sign %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
# ls_sign_norps_n3 <- get_sum_data(df_sign %>% filter(hospital == "That Phanom" & rps == FALSE))
# ls_sign_norps_t  <- get_sum_data(df_sign %>% filter(province == "Tak" & rps == FALSE))
# ls_sign_norps_t1 <- get_sum_data(df_sign %>% filter(hospital == "Mae Sot" & rps == FALSE))
# ls_sign_norps_t2 <- get_sum_data(df_sign %>% filter(hospital == "Umphang" & rps == FALSE))
# ls_sign_norps_t3 <- get_sum_data(df_sign %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

head = 'Signs & Symptoms'

gt_ss    <- create_table(df_ss, '', head)
gt_ss_n  <- create_table(df_ss %>% filter(province == 'Nakorn Phanom'), 'Nakorn Phanom Province', head)
gt_ss_n1 <- create_table(df_ss %>% filter(hospital == 'Nakorn Phanom'), 'Nakorn Phanom Hospital', head)
gt_ss_n2 <- create_table(df_ss %>% filter(hospital == 'Sri Songkhram'), 'Sri Songkhram Hospital', head)
gt_ss_n3 <- create_table(df_ss %>% filter(hospital == 'That Phanom'), 'That Phanom Hospital', head)
gt_ss_t  <- create_table(df_ss %>% filter(province == 'Tak'), 'Tak Province', head)
gt_ss_t1 <- create_table(df_ss %>% filter(hospital == 'Mae Sot'), 'Mae Sot Hospital', head)
gt_ss_t2 <- create_table(df_ss %>% filter(hospital == 'Umphang'), 'Umphang Hospital', head)
gt_ss_t3 <- create_table(df_ss %>% filter(hospital == 'Tha Song Yang'), 'Tha Song Yang Hospital', head)

# rpstext = '(RPS Only)'
# gt_ss_rps    <- create_table(df_ss %>% filter(rps == TRUE), rpstext, head)
# gt_ss_rps_n  <- create_table(df_ss %>% filter(province == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Province", rpstext), head)
# gt_ss_rps_n1 <- create_table(df_ss %>% filter(hospital == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_ss_rps_n2 <- create_table(df_ss %>% filter(hospital == "Sri Songkhram" & rps == TRUE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_ss_rps_n3 <- create_table(df_ss %>% filter(hospital == "That Phanom" & rps == TRUE), paste("That Phanom Hospital", rpstext), head)
# gt_ss_rps_t  <- create_table(df_ss %>% filter(province == "Tak" & rps == TRUE), paste("Tak Province", rpstext), head)
# gt_ss_rps_t1 <- create_table(df_ss %>% filter(hospital == "Mae Sot" & rps == TRUE), paste("Mae Sot Hospital", rpstext), head)
# gt_ss_rps_t2 <- create_table(df_ss %>% filter(hospital == "Umphang" & rps == TRUE), paste("Umphang Hospital", rpstext), head)
# gt_ss_rps_t3 <- create_table(df_ss %>% filter(hospital == "Tha Song Yang" & rps == TRUE), paste("Tha Song Yang Hospital", rpstext), head)

# rpstext = '(Non-RPS Only)'
# gt_ss_norps    <- create_table(df_ss %>% filter(rps == FALSE), rpstext, head)
# gt_ss_norps_n  <- create_table(df_ss %>% filter(province == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Province", rpstext), head)
# gt_ss_norps_n1 <- create_table(df_ss %>% filter(hospital == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_ss_norps_n2 <- create_table(df_ss %>% filter(hospital == "Sri Songkhram" & rps == FALSE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_ss_norps_n3 <- create_table(df_ss %>% filter(hospital == "That Phanom" & rps == FALSE), paste("That Phanom Hospital", rpstext), head)
# gt_ss_norps_t  <- create_table(df_ss %>% filter(province == "Tak" & rps == FALSE), paste("Tak Province", rpstext), head)
# gt_ss_norps_t1 <- create_table(df_ss %>% filter(hospital == "Mae Sot" & rps == FALSE), paste("Mae Sot Hospital", rpstext), head)
# gt_ss_norps_t2 <- create_table(df_ss %>% filter(hospital == "Umphang" & rps == FALSE), paste("Umphang Hospital", rpstext), head)
# gt_ss_norps_t3 <- create_table(df_ss %>% filter(hospital == "Tha Song Yang" & rps == FALSE), paste("Tha Song Yang Hospital", rpstext), head)

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
  ) %>% 
  filter(finalresult %in% c('Positive', 'Negative'))

# ls_un    <- get_sum_data(df_un)
# ls_un_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom"))
# ls_un_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom"))
# ls_un_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram"))
# ls_un_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom"))
# ls_un_t  <- get_sum_data(df_un %>% filter(province == "Tak"))
# ls_un_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot"))
# ls_un_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang"))
# ls_un_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang"))

# ls_un_rps    <- get_sum_data(df_un %>% filter(rps == TRUE))
# ls_un_rps_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom" & rps == TRUE))
# ls_un_rps_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
# ls_un_rps_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
# ls_un_rps_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom" & rps == TRUE))
# ls_un_rps_t  <- get_sum_data(df_un %>% filter(province == "Tak" & rps == TRUE))
# ls_un_rps_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot" & rps == TRUE))
# ls_un_rps_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang" & rps == TRUE))
# ls_un_rps_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

# ls_un_norps    <- get_sum_data(df_un %>% filter(rps == FALSE))
# ls_un_norps_n  <- get_sum_data(df_un %>% filter(province == "Nakorn Phanom" & rps == FALSE))
# ls_un_norps_n1 <- get_sum_data(df_un %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
# ls_un_norps_n2 <- get_sum_data(df_un %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
# ls_un_norps_n3 <- get_sum_data(df_un %>% filter(hospital == "That Phanom" & rps == FALSE))
# ls_un_norps_t  <- get_sum_data(df_un %>% filter(province == "Tak" & rps == FALSE))
# ls_un_norps_t1 <- get_sum_data(df_un %>% filter(hospital == "Mae Sot" & rps == FALSE))
# ls_un_norps_t2 <- get_sum_data(df_un %>% filter(hospital == "Umphang"))
# ls_un_norps_t3 <- get_sum_data(df_un %>% filter(hospital == "Tha Song Yang"))

head = 'Underlying Conditions'

gt_un    <- create_table(df_un, '', head)
gt_un_n  <- create_table(df_un %>% filter(province == 'Nakorn Phanom'), 'Nakorn Phanom Province', head)
gt_un_n1 <- create_table(df_un %>% filter(hospital == 'Nakorn Phanom'), 'Nakorn Phanom Hospital', head)
gt_un_n2 <- create_table(df_un %>% filter(hospital == 'Sri Songkhram'), 'Sri Songkhram Hospital', head)
gt_un_n3 <- create_table(df_un %>% filter(hospital == 'That Phanom'), 'That Phanom Hospital', head)
gt_un_t  <- create_table(df_un %>% filter(province == 'Tak'), 'Tak Province', head)
gt_un_t1 <- create_table(df_un %>% filter(hospital == 'Mae Sot'), 'Mae Sot Hospital', head)
gt_un_t2 <- create_table(df_un %>% filter(hospital == 'Umphang'), 'Umphang Hospital', head)
gt_un_t3 <- create_table(df_un %>% filter(hospital == 'Tha Song Yang'), 'Tha Song Yang Hospital', head)

# rpstext = '(RPS Only)'
# gt_un_rps    <- create_table(df_un %>% filter(rps == TRUE), rpstext, head)
# gt_un_rps_n  <- create_table(df_un %>% filter(province == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Province", rpstext), head)
# gt_un_rps_n1 <- create_table(df_un %>% filter(hospital == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_un_rps_n2 <- create_table(df_un %>% filter(hospital == "Sri Songkhram" & rps == TRUE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_un_rps_n3 <- create_table(df_un %>% filter(hospital == "That Phanom" & rps == TRUE), paste("That Phanom Hospital", rpstext), head)
# gt_un_rps_t  <- create_table(df_un %>% filter(province == "Tak" & rps == TRUE), paste("Tak Province", rpstext), head)
# gt_un_rps_t1 <- create_table(df_un %>% filter(hospital == "Mae Sot" & rps == TRUE), paste("Mae Sot Hospital", rpstext), head)
# gt_un_rps_t2 <- create_table(df_un %>% filter(hospital == "Umphang" & rps == TRUE), paste("Umphang Hospital", rpstext), head)
# gt_un_rps_t3 <- create_table(df_un %>% filter(hospital == "Tha Song Yang" & rps == TRUE), paste("Tha Song Yang Hospital", rpstext), head)

# rpstext = '(Non-RPS Only)'
# gt_un_norps    <- create_table(df_un %>% filter(rps == FALSE), rpstext, head)
# gt_un_norps_n  <- create_table(df_un %>% filter(province == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Province", rpstext), head)
# gt_un_norps_n1 <- create_table(df_un %>% filter(hospital == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_un_norps_n2 <- create_table(df_un %>% filter(hospital == "Sri Songkhram" & rps == FALSE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_un_norps_n3 <- create_table(df_un %>% filter(hospital == "That Phanom" & rps == FALSE), paste("That Phanom Hospital", rpstext), head)
# gt_un_norps_t  <- create_table(df_un %>% filter(province == "Tak" & rps == FALSE), paste("Tak Province", rpstext), head)
# gt_un_norps_t1 <- create_table(df_un %>% filter(hospital == "Mae Sot" & rps == FALSE), paste("Mae Sot Hospital", rpstext), head)
# gt_un_norps_t2 <- create_table(df_un %>% filter(hospital == "Umphang" & rps == FALSE), paste("Umphang Hospital", rpstext), head)
# gt_un_norps_t3 <- create_table(df_un %>% filter(hospital == "Tha Song Yang" & rps == FALSE), paste("Tha Song Yang Hospital", rpstext), head)

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
  ) %>% 
  filter(finalresult %in% c('Positive', 'Negative'))

# ls_rf    <- get_sum_data(df_rf)
# ls_rf_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom"))
# ls_rf_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom"))
# ls_rf_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram"))
# ls_rf_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom"))
# ls_rf_t  <- get_sum_data(df_rf %>% filter(province == "Tak"))
# ls_rf_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot"))
# ls_rf_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang"))
# ls_rf_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang"))

# ls_rf_rps    <- get_sum_data(df_rf %>% filter(rps == TRUE))
# ls_rf_rps_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom" & rps == TRUE))
# ls_rf_rps_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == TRUE))
# ls_rf_rps_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram" & rps == TRUE))
# ls_rf_rps_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom" & rps == TRUE))
# ls_rf_rps_t  <- get_sum_data(df_rf %>% filter(province == "Tak" & rps == TRUE))
# ls_rf_rps_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot" & rps == TRUE))
# ls_rf_rps_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang" & rps == TRUE))
# ls_rf_rps_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang" & rps == TRUE))

# ls_rf_norps    <- get_sum_data(df_rf %>% filter(rps == FALSE))
# ls_rf_norps_n  <- get_sum_data(df_rf %>% filter(province == "Nakorn Phanom" & rps == FALSE))
# ls_rf_norps_n1 <- get_sum_data(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == FALSE))
# ls_rf_norps_n2 <- get_sum_data(df_rf %>% filter(hospital == "Sri Songkhram" & rps == FALSE))
# ls_rf_norps_n3 <- get_sum_data(df_rf %>% filter(hospital == "That Phanom" & rps == FALSE))
# ls_rf_norps_t  <- get_sum_data(df_rf %>% filter(province == "Tak" & rps == FALSE))
# ls_rf_norps_t1 <- get_sum_data(df_rf %>% filter(hospital == "Mae Sot" & rps == FALSE))
# ls_rf_norps_t2 <- get_sum_data(df_rf %>% filter(hospital == "Umphang" & rps == FALSE))
# ls_rf_norps_t3 <- get_sum_data(df_rf %>% filter(hospital == "Tha Song Yang" & rps == FALSE))

head = 'Risk Factors'

gt_rf    <- create_table(df_rf, '', head)
gt_rf_n  <- create_table(df_rf %>% filter(province == 'Nakorn Phanom'), 'Nakorn Phanom Province', head)
gt_rf_n1 <- create_table(df_rf %>% filter(hospital == 'Nakorn Phanom'), 'Nakorn Phanom Hospital', head)
gt_rf_n2 <- create_table(df_rf %>% filter(hospital == 'Sri Songkhram'), 'Sri Songkhram Hospital', head)
gt_rf_n3 <- create_table(df_rf %>% filter(hospital == 'That Phanom'), 'That Phanom Hospital', head)
gt_rf_t  <- create_table(df_rf %>% filter(province == 'Tak'), 'Tak Province', head)
gt_rf_t1 <- create_table(df_rf %>% filter(hospital == 'Mae Sot'), 'Mae Sot Hospital', head)
gt_rf_t2 <- create_table(df_rf %>% filter(hospital == 'Umphang'), 'Umphang Hospital', head)
gt_rf_t3 <- create_table(df_rf %>% filter(hospital == 'Tha Song Yang'), 'Tha Song Yang Hospital', head)

# rpstext = '(RPS Only)'
# gt_rf_rps    <- create_table(df_rf %>% filter(rps == TRUE), rpstext, head)
# gt_rf_rps_n  <- create_table(df_rf %>% filter(province == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Province", rpstext), head)
# gt_rf_rps_n1 <- create_table(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == TRUE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_rf_rps_n2 <- create_table(df_rf %>% filter(hospital == "Sri Songkhram" & rps == TRUE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_rf_rps_n3 <- create_table(df_rf %>% filter(hospital == "That Phanom" & rps == TRUE), paste("That Phanom Hospital", rpstext), head)
# gt_rf_rps_t  <- create_table(df_rf %>% filter(province == "Tak" & rps == TRUE), paste("Tak Province", rpstext), head)
# gt_rf_rps_t1 <- create_table(df_rf %>% filter(hospital == "Mae Sot" & rps == TRUE), paste("Mae Sot Hospital", rpstext), head)
# gt_rf_rps_t2 <- create_table(df_rf %>% filter(hospital == "Umphang" & rps == TRUE), paste("Umphang Hospital", rpstext), head)
# gt_rf_rps_t3 <- create_table(df_rf %>% filter(hospital == "Tha Song Yang" & rps == TRUE), paste("Tha Song Yang Hospital", rpstext), head)

# rpstext = '(Non-RPS Only)'
# gt_rf_norps    <- create_table(df_rf %>% filter(rps == FALSE), rpstext, head)
# gt_rf_norps_n  <- create_table(df_rf %>% filter(province == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Province", rpstext), head)
# gt_rf_norps_n1 <- create_table(df_rf %>% filter(hospital == "Nakorn Phanom" & rps == FALSE), paste("Nakorn Phanom Hospital", rpstext), head)
# gt_rf_norps_n2 <- create_table(df_rf %>% filter(hospital == "Sri Songkhram" & rps == FALSE), paste("Sri Songkhram Hospital", rpstext), head)
# gt_rf_norps_n3 <- create_table(df_rf %>% filter(hospital == "That Phanom" & rps == FALSE), paste("That Phanom Hospital", rpstext), head)
# gt_rf_norps_t  <- create_table(df_rf %>% filter(province == "Tak" & rps == FALSE), paste("Tak Province", rpstext), head)
# gt_rf_norps_t1 <- create_table(df_rf %>% filter(hospital == "Mae Sot" & rps == FALSE), paste("Mae Sot Hospital", rpstext), head)
# gt_rf_norps_t2 <- create_table(df_rf %>% filter(hospital == "Umphang" & rps == FALSE), paste("Umphang Hospital", rpstext), head)
# gt_rf_norps_t3 <- create_table(df_rf %>% filter(hospital == "Tha Song Yang" & rps == FALSE), paste("Tha Song Yang Hospital", rpstext), head)

#-------------------------------------------------------------------------------
# Vaccination page
#-------------------------------------------------------------------------------

df_vac <- CFMast %>%
  mutate(
    vac = factor(
      # Patients were considered fully vaccinated if completed at least 1 month prior to enrol
      case_when(
        cv == 2 & fulltime < 30 ~ 1, 
        TRUE                    ~ cv
      ),
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

df_vac2 <- LabPCRFinal %>%
  left_join(tblSection1 %>%  select(cfid, province, hospital, rps), by='cfid') %>% 
  left_join(tblSection3 %>% select(cfid, cvtime, fulltime),
            by = "cfid") %>%
  mutate(finalresult = droplevels(finalresult)) %>%
  select(finalresult, cvtime, fulltime, province,   hospital,  rps) 


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

#-------------------------------------------------------------------------------
# Laboratory testing page
#-------------------------------------------------------------------------------

df_cbc <-
  LabPCRFinal %>%
  left_join(tblSection1 %>%  select(cfid, province, hospital, rps), by='cfid') %>% 
  left_join(tblSection4) %>%
  mutate(finalresult = droplevels(finalresult)) %>%
  select(
    finalresult,
    s4hematocrit,
    s4plateletx10,
    s4wbccountx10,
    s4neutrophil,
    s4lymphocyte,
    s4monocyte,
    s4eosinophil,
    s4basophil,
    s4bun,
    s4creatinine,
    s4ast,
    s4alt,
    s4albumin,
    s4lactate,
    s4procal,
    s4creprotein, 
    province,
    hospital,
    rps
  ) %>%
  set_variable_labels(
    s4hematocrit  = 'Hematocrit',
    s4plateletx10 = 'Platelet',
    s4wbccountx10 = 'WBC',
    s4neutrophil  = 'Neutrophil',
    s4lymphocyte  = 'Lymphocyte',
    s4monocyte    = 'Monocyte',
    s4eosinophil  = 'Eosinophil',
    s4basophil    = 'Basophil',
    s4bun         = 'Blood urea nitrogen (mg/dL)',
    s4creatinine  = 'Creatinine (mg/dL)',
    s4ast         = 'Aspartate aminotransferase (iu/L)',
    s4alt         = 'Alanine aminotransferase (iu/L)',
    s4albumin     = 'Albumin (g/dL)',
    s4lactate     = 'Lactate (mmol/L)',
    s4procal      = 'Procalcitonin (mg/mL)',
    s4creprotein  = 'C-reactive protein (mg/L)'
  )

df_cul <-
  LabPCRFinal %>%
  left_join(tblSection1 %>%  select(cfid, province, hospital, rps), by='cfid') %>% 
  left_join(tblSection4) %>%
  mutate(
    finalresult = droplevels(finalresult),
    o1 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '10'), na.rm = TRUE) > 0,
    o2 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '47'), na.rm = TRUE) > 0,
    o3 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '84'), na.rm = TRUE) > 0,
    o4 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '103'), na.rm = TRUE) > 0,
    o5 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '108'), na.rm = TRUE) > 0,
    o6 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '111'), na.rm = TRUE) > 0,
    o7 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '117'), na.rm = TRUE) > 0,
    o8 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '147'), na.rm = TRUE) > 0,
    o9 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '148'), na.rm = TRUE) > 0,
    o10 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '263'), na.rm = TRUE) > 0,
    o11 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '353'), na.rm = TRUE) > 0,
    o12 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '365'), na.rm = TRUE) > 0,
    o13 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '395'), na.rm = TRUE) > 0,
    o14 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '459'), na.rm = TRUE) > 0,
    o15 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '998'), na.rm = TRUE) > 0,
    o16 = rowSums(across(ends_with("org1") | ends_with("org2") | ends_with("org3"),
                        ~ . == '999'), na.rm = TRUE) > 0
  ) %>%
  select (finalresult, o1:o16, province,  hospital,  rps) %>% 
  set_variable_labels(
    o1  = 'Aerobic Gram Positive Cocci',
    o2  = 'Burkholderia pseudomallei',
    o3  = 'Corynebacterium spp.',
    o4  = 'Enterobacter cloacae',
    o5  = 'Enterobacter species',
    o6  = 'Enterococcus faecalis',
    o7  = 'Escherichia coli',
    o8  = 'Klebsiella pneumoniae',
    o9  = 'Klebsiella species',
    o10 = 'Pseudomonas aeruginosa',
    o11 = 'Shigella species',
    o12 = 'Staphylococcus cohnii',
    o13 = 'Streptococcus agalactiae',
    o14 = 'Yeast',
    o15 = 'Mixed Growth',
    o16 = 'Other'
  )

#-------------------------------------------------------------------------------
# Serology testing page
#-------------------------------------------------------------------------------

df_sero1a <- 
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

df_sero1b <-
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
      na.rm = TRUE) >= 1
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

df_sero2a <-
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
    numdose = cut(
      s33cvamount,
      breaks = c(0, 1, 2, Inf),
      labels = c('< 2 doses', '2 doses', '> 2 doses'),
      include.lowest = TRUE
    ),
    igm_o = if_else(rowSums(across(starts_with('igm_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative'),
    iggn_o = if_else(rowSums(across(starts_with('iggn_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative'),
    iggs_o = if_else(rowSums(across(starts_with('iggs_')) == 'Positive', na.rm = TRUE) > 0, 'Positive', 'Negative')
  ) %>%
  # Select only variables to be used
  select(
    province,
    hospital,
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

df_sero2b <-
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
    province,
    hospital,
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
  mutate(
    variant = factor(ifelse(s1enrolldate < '2022-1-1', 1, 2), labels = c('Jun-Dec 2021', 'Jan 2022 - May 2023'))) %>% 
  select(province, hospital, rps, variant,
         s3604sickspread, s3615carelate:s3620) %>%
  rename(s3604 = s3604sickspread,
         s3615 = s3615carelate) %>% 
  pivot_longer(cols = s3604:s3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, rps, variant, kap, scale) %>%
  tally()

df_kap2 <- CFMast %>%
  mutate(
    variant = factor(ifelse(s1enrolldate < '2022-1-1', 1, 2), labels = c('Jun-Dec 2021', 'Jan 2022 - May 2023'))) %>% 
  select(province, hospital, rps,variant,
         s3610maskin, s3613maskout, s3621:s3622) %>%
  rename(s3610 = s3610maskin,
         s3613 = s3613maskout) %>% 
  pivot_longer(cols = s3610:s3622,
               names_to = "kap",
               values_to = "scale") %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, rps, variant, kap, scale) %>%
  tally()


df_kap3 <-
  tblSection1 %>% filter(!is.na(cfid)) %>%
  left_join(tblSection3, by = 'cfid') %>%
  # Select only variables to be used
  select(province, hospital, rps, s1enrolldate,
         s3604sickspread, s3615carelate:s3620) %>%
  rename(s3604 = s3604sickspread,
         s3615 = s3615carelate) %>%
  mutate(
    variant = factor(ifelse(s1enrolldate < '2022-1-1', 1, 2), labels = c('Jun-Dec 2021', 'Jan 2022 - May 2023'))) %>% 
  # Set variable labels to be displayed in output
  set_variable_labels(
    's3604' = 'Only sick people with symptoms can spread the disease',
    's3615' = 'Sought care later than usual because of COVID-19',
    's3616' = 'Afraid of being quarantined after close contact with COVID-19 patient',
    's3617' = 'Afraid to seek care out of fear of being tested/isolated',
    's3618' = 'Always wearing mask in public is a good thing to do',
    's3619' = 'Always practicing social distancing is a good thing to do',
    's3620' = 'Patients should disclose their exposure to COVID-19 and their symptoms'
      )

df_kap4 <-
  tblSection1 %>% filter(!is.na(cfid)) %>%
  left_join(tblSection3, by = 'cfid') %>%
  # Select only variables to be used
  select(province, hospital, rps, s1enrolldate,
          s3610maskin, s3613maskout, s3621:s3622) %>%
  rename(
         s3610 = s3610maskin,
         s3613 = s3613maskout) %>%
  mutate(
    variant = factor(ifelse(s1enrolldate < '2022-1-1', 1, 2), labels = c('Jun-Dec 2021', 'Jan 2022 - May 2023'))) %>% 
  # Set variable labels to be displayed in output
  set_variable_labels(
    's3610' = 'During the past 2 weeks, did you wear a mask at home?',
    's3613' = 'Did you wear a mask when you went outside in crowded areas?',
    's3621' = 'Do you practice social distancing in your household?',
    's3622' = 'Do you practice social distancing outside of your residence?'
    
  )



#-------------------------------------------------------------------------------
#Long COVID data
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#Long COVID section1 no. patient
#-------------------------------------------------------------------------------

df_lc1 <- lcsec1 %>%
  select(cfid, province, period) %>%
  left_join(lcsec2 %>% filter(l2period == 1) %>% select(cfid, fu1 = l2period), by = 'cfid') %>%
  mutate(fu1 = !is.na(fu1)) %>%
  left_join(lcsec2 %>% filter(l2period == 2) %>% select(cfid, fu2 = l2period), by = 'cfid') %>%
  mutate(fu2 = !is.na(fu2)) %>%
  left_join(lcsec2 %>% filter(l2period == 3) %>% select(cfid, fu3 = l2period), by = 'cfid') %>%
  mutate(fu3 = !is.na(fu3)) %>%
  left_join(lcsec2 %>% filter(l2period == 4) %>% select(cfid, fu4 = l2period), by = 'cfid') %>% 
  mutate(fu4 = !is.na(fu4)) %>% 
  select(-cfid) %>% 
  set_variable_labels(
    fu1 = 'Follow up # 1',
    fu2 = 'Follow up # 2',
    fu3 = 'Follow up # 3',
    fu4 = 'Follow up # 4'
  )



df_lc2 <- lcsec1 %>%
  select(l1severegrade, province) %>%
  mutate(
    l1severegrade = factor(l1severegrade,levels=c(1:4),labels = c('Mild','Moderate', 'Severe','Critical'))
  ) %>% 
  set_variable_labels(
    l1severegrade = 'Severeity'
  )


df_lc3 <- lcsec1 %>%
  select(l1cshock:l1csid, province) %>%
  mutate(
    across(c(l1cshock:l1csid),
           function(f) {
             ifelse(f == 0 | is.na(f),2,f)
           }),
    across(c(l1cshock:l1csid),
           function(f) {
             factor(f,levels = 1:2,labels = c('Yes','No'))
           }),
  ) %>% 
  set_variable_labels(
    l1cshock = 'Shock',
    l1cseizure =   'Seizure'   ,
    l1cmeningitis = 'Meningitis',
    l1ctransfusion = 'Transfusion',
    l1ccararrhy =   'Cardiac arrhythmia',
    l1ccararrest = 'Cardiac arrest',
    l1cards = 'Acute respiratory distress syndrome (ARDS)',
    l1cisstroke = 'Stroke ischemic stroke',
    l1cinstroke = 'Stroke intracerebral haemorrhage',
    l1cstroke = 'Stroke (unspecified type)',
    l1cmentaldis   = 'Mental health disorders',
    l1cbacteremia   = 'Bacteremia',
    l1cendocarditis = 'endocarditis',
    l1cmyocarditis   = 'Myocarditis/pericarditis',
    l1cari        = 'Acute renal injury',
    l1cpancreatitis = 'Pancreatitis',
    l1ccardiomyopathy = 'Cardiomyopathy',
    l1cpulemb        = 'Pulmonary embolism',
    l1cdvt      = 'Deep vein thrombosis',
    l1csid = 'Steroid induced diabetes'
  )

df_lc4 <- lcsec1 %>%
  select(l1inuri :l1mucormycosis, province) %>%
  mutate(
    across(c(l1inuri :l1mucormycosis),
           function(f) {
             ifelse(f == 0 | is.na(f),2,f)
           }),
    across(c(l1inuri :l1mucormycosis),
           function(f) {
             factor(f,levels = 1:2,labels = c('Yes','No'))
           }),
  ) %>% 
  set_variable_labels(
    l1inuri  ='URI',
    l1inuti   ='Urinary Tract Infection',
    l1inbone     ='Bone and Joint Infections'   ,  
    l1incns     ='Central nervous system Infections',
    l1ingastro      ='Gastrointestinal Infections' ,
    l1inlri   ='LRI'  ,
    l1inskin    ='Skin and Soft Tissue Infection',
    l1incardio     ='Cardiovascular Infections',
    l1inblood     ='Bloodstream Infections'   , 
    l1instds     ='Sexual transmitted disease',
    l1mucormycosis ='Mucormycosis'
 
  )
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
    "gt_dx",
    "gt_dx_n",
    "gt_dx_n1",
    "gt_dx_n2",
    "gt_dx_n3",
    "gt_dx_t",
    "gt_dx_t1",
    "gt_dx_t2",
    "gt_dx_t3",
    # "gt_dx_rps",
    # "gt_dx_rps_n",
    # "gt_dx_rps_n1",
    # "gt_dx_rps_n2",
    # "gt_dx_rps_n3",
    # "gt_dx_rps_t",
    # "gt_dx_rps_t1",
    # "gt_dx_rps_t2",
    # "gt_dx_rps_t3",
    # "gt_dx_norps",
    # "gt_dx_norps_n",
    # "gt_dx_norps_n1",
    # "gt_dx_norps_n2",
    # "gt_dx_norps_n3",
    # "gt_dx_norps_t",
    # "gt_dx_norps_t1",
    # "gt_dx_norps_t2",
    # "gt_dx_norps_t3",
    "df_signBox",
    "gt_ss",
    "gt_ss_n",
    "gt_ss_n1",
    "gt_ss_n2",
    "gt_ss_n3",
    "gt_ss_t",
    "gt_ss_t1",
    "gt_ss_t2",
    "gt_ss_t3",
    "df_ss_bf",
    # "gt_ss_rps",
    # "gt_ss_rps_n",
    # "gt_ss_rps_n1",
    # "gt_ss_rps_n2",
    # "gt_ss_rps_n3",
    # "gt_ss_rps_t",
    # "gt_ss_rps_t1",
    # "gt_ss_rps_t2",
    # "gt_ss_rps_t3",
    # "gt_ss_norps",
    # "gt_ss_norps_n",
    # "gt_ss_norps_n1",
    # "gt_ss_norps_n2",
    # "gt_ss_norps_n3",
    # "gt_ss_norps_t",
    # "gt_ss_norps_t1",
    # "gt_ss_norps_t2",
    # "gt_ss_norps_t3",
    "gt_un",
    "gt_un_n",
    "gt_un_n1",
    "gt_un_n2",
    "gt_un_n3",
    "gt_un_t",
    "gt_un_t1",
    "gt_un_t2",
    "gt_un_t3",
    # "gt_un_rps",
    # "gt_un_rps_n",
    # "gt_un_rps_n1",
    # "gt_un_rps_n2",
    # "gt_un_rps_n3",
    # "gt_un_rps_t",
    # "gt_un_rps_t1",
    # "gt_un_rps_t2",
    # "gt_un_rps_t3",
    # "gt_un_norps",
    # "gt_un_norps_n",
    # "gt_un_norps_n1",
    # "gt_un_norps_n2",
    # "gt_un_norps_n3",
    # "gt_un_norps_t",
    # "gt_un_norps_t1",
    # "gt_un_norps_t2",
    # "gt_un_norps_t3",
    "gt_rf",
    "gt_rf_n",
    "gt_rf_n1",
    "gt_rf_n2",
    "gt_rf_n3",
    "gt_rf_t",
    "gt_rf_t1",
    "gt_rf_t2",
    "gt_rf_t3",
    # "gt_rf_rps",
    # "gt_rf_rps_n",
    # "gt_rf_rps_n1",
    # "gt_rf_rps_n2",
    # "gt_rf_rps_n3",
    # "gt_rf_rps_t",
    # "gt_rf_rps_t1",
    # "gt_rf_rps_t2",
    # "gt_rf_rps_t3",
    # "gt_rf_norps",
    # "gt_rf_norps_n",
    # "gt_rf_norps_n1",
    # "gt_rf_norps_n2",
    # "gt_rf_norps_n3",
    # "gt_rf_norps_t",
    # "gt_rf_norps_t1",
    # "gt_rf_norps_t2",
    # "gt_rf_norps_t3",
    "df_vac", 
    "df_vac2",
    "df_atk",
    "df_lab",
    "df_labpos",
    "df_cbc",
    "df_cul",
    "df_sero1a",
    "df_sero1b",
    "df_sero2a",
    "df_sero2b",
    "df_kap1",
    "df_kap2",
    "df_kap3",
    "df_kap4",
    "lcddate",
    "df_lc1",
    "df_lc2",
    "df_lc3",
    "df_lc4"),
  file = paste0(data_folder, "/CFDashboard.RData")
)



