ddate <- max(tblSection1$s1screendate, na.rm = TRUE)

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
  select(cfid,
         province,
         hospital,
         finalresult,
         s2dxfever:s2dxother,
         s2dxmeningitis) %>%
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
  pivot_longer(cols = Fever:Meningitis,
               names_to = "diagnosis",
               values_to = "y") %>%
  group_by(province, hospital, finalresult, diagnosis) %>%
  tally(wt = y)

#-------------------------------------------------------------------------------
# Clinical Sign
#-------------------------------------------------------------------------------
df_sign <- CFMast %>%
  select(
    cfid,
    province,
    hospital,
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
    s32other,
    finalresult
  ) %>%
  mutate(s2temp = ifelse(s2temp >= 38, 1, 0)) %>%
  rename_with(~ str_replace(., "s32", ""), starts_with("s32")) %>%
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
  pivot_longer(
    cols = `Temperature >= 38.0 C`:Other,
    names_to = "signs",
    values_to = "y"
  ) %>%
  group_by(province, hospital, finalresult, signs) %>%
  tally(wt = y)

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
    cfid,
    province,
    hospital,
    s35diabetes,
    s35obesity:s35cancer,
    s35hiv:s35othchronic,
    s35hissmoke:s35pregnancy,
    finalresult
  ) %>%
  rename_with(~ str_replace(., "s35", ""), starts_with("s35")) %>%
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
  pivot_longer(cols = Diabetes:Pregnancy,
               names_to = "underlying",
               values_to = "y") %>%
  group_by(underlying, hospital, province, finalresult) %>%
  summarise(n = sum(y[y == 1], na.rm = TRUE))

#-------------------------------------------------------------------------------
#Risk Factor page
#-------------------------------------------------------------------------------

df_rf <- CFMast %>%
  select(
    cfid,
    province,
    hospital,
    s33suspectedcovid19:s33visithos,
    s33visticrowded:s33travelthai,
    s33travelinter,
    finalresult
  ) %>%
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
  pivot_longer(
    cols = "Contact COVID-19 cases":"History of travel",
    names_to = "risk",
    values_to = "y"
  ) %>%
  group_by(province, hospital, finalresult, risk) %>%
  tally(wt = y)

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
  pivot_longer(cols = s3604sickspread:s3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(province, hospital, kap, scale) %>%
  tally()

df_kap2 <- CFMast %>%
  select(province, hospital,
         s3610maskin, s3613maskout, s3621:s3622) %>%
  pivot_longer(cols = s3610maskin:s3622,
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
    "df_dx",
    "df_un",
    "df_rf",
    "df_sign",
    "df_signBox",
    "df_vac",
    "df_atk",
    "df_lab",
    "df_labpos",
    "df_kap1",
    "df_kap2"
  ),
  file = paste0(data_folder, "/CFDashboard.RData") 
)
