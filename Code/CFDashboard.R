#------------------------------------------------------------------------------#
# Description: Program for prepare data frames and gt tables for Covid-Fever   #
#              dashboard https://dghp.shinyapps.io/COVID-Fever/  	  	         #
# Author:      hpy1                                                            #
# Created:     January 27, 2022                                                #
# Modified:    March 9, 2022                                                   #
#------------------------------------------------------------------------------#

library(sf)

ddate <- max(tblSection1$s1screendate, na.rm = TRUE)
shapefile_folder <- paste0(getwd(), "/Maps/")
source(paste0(code_folder, "/Functions.R"))

#-------------------------------------------------------------------------------
# Study sites #
#-------------------------------------------------------------------------------

world        <- st_read(paste0(shapefile_folder, "World_Countries.shp"),
                        stringsAsFactors = FALSE)
th_province  <- st_read(paste0(shapefile_folder, "ThailandProvince.shp"),
                        stringsAsFactors = FALSE)
th_district  <- st_read(paste0(shapefile_folder, "Amphoe77.shp"), 
                        stringsAsFactors = FALSE)

world <- world %>%
  mutate(
    my_nudge_x = case_when(ISO == 'MM' ~ 1.7,
                           ISO == 'MY' ~ -0.1,
                           TRUE ~ 0),
    my_nudge_y = case_when(ISO == 'MM' ~ 1,
                           ISO == 'MY' ~ 1.59,
                           TRUE ~ 0),
    COUNTRY = ifelse(ISO == 'TH', NA, COUNTRY)
  )
tak_district <- th_district %>% filter(ProvID == 63)
np_district  <- th_district %>% filter(ProvID == 48)

cf_province  <- th_province %>% filter(ProvNum %in% c(48, 63))
cf_tak       <- tak_district %>% filter(AmphoeID %in% c("05", "06", "08"))
cf_np        <- np_district %>% filter(AmphoeID %in% c("01", "05", "08"))

map_cf_province <- ggplot() +
  geom_sf(data = world,
          fill = "gray95",
          size = 0.5) +
  geom_sf(data = th_province,
          fill = "grey80",
          size = 0.5) +
  geom_sf(data = cf_province,
          fill = "#dd4b39") +
  geom_sf_text(data = world,
               aes(label = COUNTRY),
               nudge_x=world$my_nudge_x,nudge_y=world$my_nudge_y,
               size = 4) +
  geom_sf_text(data = cf_province, 
               aes(label = ProvName),
               size = 4) +
  coord_sf(xlim = c(97, 106),
           ylim = c(6, 20),
           expand = TRUE) + 
  theme_void()

map_cf_tak <- ggplot() +
  geom_sf(data = tak_district, 
          fill = "grey95") +
  geom_sf(data = cf_tak, aes(fill = AmphoeE)) +
  geom_sf_text(data = cf_tak,
            aes(label = AmphoeE),
            size = 4) +
  coord_sf(xlim = c(371000, 544000),
           ylim = c(1688000, 1965000)) +
  scale_fill_manual(values = c("#f8de7e", "pink", "#a1caf1")) +
  theme_void() +
  theme(legend.position = "none")

map_cf_np <- ggplot() +
  geom_sf(data = np_district, 
          fill = "grey95") +
  geom_sf(data = cf_np, aes(fill = AmphoeE)) +
  geom_sf_text(data = cf_np,
            aes(label = AmphoeE),
            size = 4) +
  coord_sf(xlim = c(1026500, 1123500),
           ylim = c(1870000, 1995000)) +
  scale_fill_manual(values = c('#ffcba4', '#d8bfd8', '#ace1af')) +
  theme_void() +
  theme(legend.position = "none")

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
  group_by(province, hospital, scrdate) %>%
  tally()

df_elim <- tblSection1 %>%
  filter(s1eligible == TRUE) %>% # Eligible only
  mutate(scrdate = floor_date(s1screendate, "month")) %>%
  group_by(province, hospital, scrdate) %>%
  tally()

df_enrw <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "week", week_start = 1)) %>%
  group_by(province, hospital, enrdate, finalresult) %>%
  tally()

df_enrm <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "month")) %>%
  group_by(province, hospital, enrdate, finalresult) %>%
  tally()

df_pos3wk <- CFMast %>%
  filter(testdate > Sys.Date() - 22) %>%
  group_by(province, hospital, finalresult) %>%
  tally()

df_enrage <- CFMast %>%
  group_by(province, hospital, agegroup) %>%
  tally()

df_enrgender <- CFMast %>%
  group_by(province, hospital, s1gender) %>%
  tally()

df_enrocc <- CFMast %>%
  mutate(s34occupation = replace(s34occupation, s34occupation == "Other farmer", "Farmer")) %>%
  group_by(province, hospital, s34occupation) %>%
  tally()

#-------------------------------------------------------------------------------
# Diagnosis page
#-------------------------------------------------------------------------------

df_dx <- CFMast %>%
  select(province,
         hospital,
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

gt_dx    <- create_sumtable(df_dx, "Diagnoses")
gt_dx_n  <- create_sumtable(df_dx %>% filter(province == "Nakorn Phanom"), "Diagnoses")
gt_dx_t  <- create_sumtable(df_dx %>% filter(province == "Tak"), "Diagnoses")
gt_dx_n1 <- create_sumtable(df_dx %>% filter(hospital == "Nakorn Phanom"), "Diagnoses")
gt_dx_n2 <- create_sumtable(df_dx %>% filter(hospital == "Sri Songkhram"), "Diagnoses")
gt_dx_n3 <- create_sumtable(df_dx %>% filter(hospital == "That Phanom"), "Diagnoses")
gt_dx_t1 <- create_sumtable(df_dx %>% filter(hospital == "Mae Sot"), "Diagnoses")
gt_dx_t2 <- create_sumtable(df_dx %>% filter(hospital == "Umphang"), "Diagnoses")
gt_dx_t3 <- create_sumtable(df_dx %>% filter(hospital == "Tha Song Yang"), "Diagnoses")

#-------------------------------------------------------------------------------
# Clinical Sign
#-------------------------------------------------------------------------------

df_sign <- CFMast %>%
  select(
    province,
    hospital,
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

gt_sign    <- create_sumtable(df_sign, "Signs & Symptoms")
gt_sign_n  <- create_sumtable(df_sign %>% filter(province == "Nakorn Phanom"), "Signs & Symptoms")
gt_sign_t  <- create_sumtable(df_sign %>% filter(province == "Tak"), "Signs & Symptoms")
gt_sign_n1 <- create_sumtable(df_sign %>% filter(hospital == "Nakorn Phanom"), "Signs & Symptoms")
gt_sign_n2 <- create_sumtable(df_sign %>% filter(hospital == "Sri Songkhram"), "Signs & Symptoms")
gt_sign_n3 <- create_sumtable(df_sign %>% filter(hospital == "That Phanom"), "Signs & Symptoms")
gt_sign_t1 <- create_sumtable(df_sign %>% filter(hospital == "Mae Sot"), "Signs & Symptoms")
gt_sign_t2 <- create_sumtable(df_sign %>% filter(hospital == "Umphang"), "Signs & Symptoms")
gt_sign_t3 <- create_sumtable(df_sign %>% filter(hospital == "Tha Song Yang"), "Signs & Symptoms")

df_signBox <- CFMast %>%
  select(cfid,
         province,
         hospital,
         s5covidpos,
         s5intub,
         s5dischargetype) %>%
  filter(s5covidpos == 1) %>%
  group_by(province, hospital, s5intub, s5dischargetype) %>%
  tally()

#-------------------------------------------------------------------------------
# Underlying Page
#-------------------------------------------------------------------------------

df_un <- CFMast %>%
  select(
    province,
    hospital,
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

gt_un    <- create_sumtable(df_un, "Underlying Conditions")
gt_un_n  <- create_sumtable(df_un %>% filter(province == "Nakorn Phanom"), "Underlying Conditions")
gt_un_t  <- create_sumtable(df_un %>% filter(province == "Tak"), "Underlying Conditions")
gt_un_n1 <- create_sumtable(df_un %>% filter(hospital == "Nakorn Phanom"), "Underlying Conditions")
gt_un_n2 <- create_sumtable(df_un %>% filter(hospital == "Sri Songkhram"), "Underlying Conditions")
gt_un_n3 <- create_sumtable(df_un %>% filter(hospital == "That Phanom"), "Underlying Conditions")
gt_un_t1 <- create_sumtable(df_un %>% filter(hospital == "Mae Sot"), "Underlying Conditions")
gt_un_t2 <- create_sumtable(df_un %>% filter(hospital == "Umphang"), "Underlying Conditions")
gt_un_t3 <- create_sumtable(df_un %>% filter(hospital == "Tha Song Yang"), "Underlying Conditions")

#-------------------------------------------------------------------------------
#Risk Factor page
#-------------------------------------------------------------------------------

df_rf <- CFMast %>%
  select(
    province,
    hospital,
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

gt_rf    <- create_sumtable(df_rf, "Risk Factors")
gt_rf_n  <- create_sumtable(df_rf %>% filter(province == "Nakorn Phanom"), "Risk Factors")
gt_rf_t  <- create_sumtable(df_rf %>% filter(province == "Tak"), "Risk Factors")
gt_rf_n1 <- create_sumtable(df_rf %>% filter(hospital == "Nakorn Phanom"), "Risk Factors")
gt_rf_n2 <- create_sumtable(df_rf %>% filter(hospital == "Sri Songkhram"), "Risk Factors")
gt_rf_n3 <- create_sumtable(df_rf %>% filter(hospital == "That Phanom"), "Risk Factors")
gt_rf_t1 <- create_sumtable(df_rf %>% filter(hospital == "Mae Sot"), "Risk Factors")
gt_rf_t2 <- create_sumtable(df_rf %>% filter(hospital == "Umphang"), "Risk Factors")
gt_rf_t3 <- create_sumtable(df_rf %>% filter(hospital == "Tha Song Yang"), "Risk Factors")

#-------------------------------------------------------------------------------
# Vaccination page
#-------------------------------------------------------------------------------

df_vac <- CFMast %>%
  mutate(s33covidvaccine = factor(ifelse(is.na(s33covidvaccine), "Unknown", as.character(s33covidvaccine)),
                                  levels = c('Vaccinated','Unvaccinated','Unknown')),
         finalresult = ifelse(!is.na(finalresult), as.character(finalresult), 'Unknown')) %>%
  group_by(province, hospital, s33covidvaccine, finalresult) %>%
  tally()

#-------------------------------------------------------------------------------
# Atk page
#-------------------------------------------------------------------------------
df_atk <- CFMast %>% 
  filter(s33atk == 'Yes') %>%
  select(cfid,
         province,
         hospital,
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
  group_by(province, hospital, atkresult) %>%
  tally()

#-------------------------------------------------------------------------------
# SARS-Cov2 detection page
#-------------------------------------------------------------------------------

df_lab <- LabPCRResult_l %>%
  group_by(province, hospital, spectype, finalresult_fac) %>%
  tally() %>%
  rename(finalresult = finalresult_fac)

df_labpos <- LabPCRResult_l %>%
  # Select only 4 columns (ID, type, result, and test dates)
  select(cfid,
         province,
         hospital,
         spectype,
         finalresult) %>%
  # All specimen types in one line
  pivot_wider(names_from = "spectype",
              values_from = "finalresult") %>%
  filter(`NP+OP swab` == 1 | `Nasal swab` == 1 | Saliva == 1) %>%
  mutate(
    specimens = case_when(
      `NP+OP swab`   == 1 &
        `Nasal swab` == 1 & `Saliva` == 1 ~ "NP/OP, nasal, saliva",
      `NP+OP swab`   == 1 &
        `Nasal swab` == 1                ~ "NP/OP, nasal",
      `NP+OP swab`   == 1 &
        `Saliva`     == 1                ~ "NP/OP, saliva",
      `Nasal swab`   == 1 &
        `Saliva`     == 1                ~ "Nasal, saliva",
      `NP+OP swab`   == 1                                    ~ "NP/OP",
      `Nasal swab`   == 1                                    ~ "Nasal",
      `Saliva`       == 1                                    ~ "Saliva",
      TRUE                                                   ~ "other"
    )
  ) %>%
  group_by(province, hospital, specimens) %>%
  tally()

#-------------------------------------------------------------------------------
# KAP page
#-------------------------------------------------------------------------------

df_kap1 <- CFMast %>%
  select(province, hospital,
         s3604sickspread, s3615carelate:s3620) %>%
  rename(s3604 = s3604sickspread,
         s3615 = s3615carelate) %>% 
  pivot_longer(cols = s3604:s3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, kap, scale) %>%
  tally()

df_kap2 <- CFMast %>%
  select(province, hospital,
         s3610maskin, s3613maskout, s3621:s3622) %>%
  rename(s3610 = s3610maskin,
         s3613 = s3613maskout) %>% 
  pivot_longer(cols = s3610:s3622,
               names_to = "kap",
               values_to = "scale") %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, kap, scale) %>%
  tally()

#-------------------------------------------------------------------------------
# Save data frames for dashboard in one data file (CFDashboard.RData)
#-------------------------------------------------------------------------------

save(
  list = c(
    "ddate",
    "map_cf_province",
    "map_cf_np",
    "map_cf_tak",
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
    "gt_sign",
    "gt_sign_n",
    "gt_sign_n1",
    "gt_sign_n2",
    "gt_sign_n3",
    "gt_sign_t",
    "gt_sign_t1",
    "gt_sign_t2",
    "gt_sign_t3",
    "df_signBox",
    "gt_un",
    "gt_un_n",
    "gt_un_n1",
    "gt_un_n2",
    "gt_un_n3",
    "gt_un_t",
    "gt_un_t1",
    "gt_un_t2",
    "gt_un_t3",
    "gt_rf",
    "gt_rf_n",
    "gt_rf_n1",
    "gt_rf_n2",
    "gt_rf_n3",
    "gt_rf_t",
    "gt_rf_t1",
    "gt_rf_t2",
    "gt_rf_t3",
    "df_vac",
    "df_atk",
    "df_lab",
    "df_labpos",
    "df_kap1",
    "df_kap2"
  ),
  file = paste0(data_folder, "/CFDashboard.RData")
)

