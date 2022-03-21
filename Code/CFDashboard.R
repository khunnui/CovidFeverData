ddate <- max(tblSection1$S1ScreenDate, na.rm = TRUE)

# Screening page
df_scrw <- tblSection1 %>%
  mutate(scrdate = floor_date(S1ScreenDate, "week", week_start = 1)) %>%
  group_by(Province, S1HospitalID, scrdate) %>% 
  tally() %>% 
  ungroup()
df_scrm <- tblSection1 %>%
  mutate(scrdate = floor_date(S1ScreenDate, "month")) %>%
  group_by(Province, S1HospitalID, scrdate) %>% 
  tally() %>% 
  ungroup()
df_scrage0 <- tblSection1 %>%
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  )
df_scrage1 <- tblSection1 %>%
  group_by(Province) %>% 
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  ) %>% 
  ungroup()
df_scrage2 <- tblSection1 %>%
  group_by(S1HospitalID) %>% 
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  ) %>% 
  ungroup()
df_scrgender <- tblSection1 %>%
  group_by(Province, S1HospitalID, S1Gender) %>% 
  tally() %>% 
  ungroup()
df_screnrol <- tblSection1 %>%
  group_by(Province, S1HospitalID, OLDCF, CF_Enrol) %>% 
  tally() %>% 
  ungroup()

# Enrollment page
df_eli <- tblSection1 %>%
  filter(S1Eligible == 1) %>% # Eligible only
  mutate(scrdate = floor_date(S1ScreenDate, "month")) %>% 
  group_by(Province, S1HospitalID, scrdate) %>% 
  tally() %>% 
  ungroup()
df_enr <- tblSection1 %>%
  filter(!is.na(S1EnrollDate)) %>%
  left_join(LabPCRResult_w, by = "CFID") %>%
  mutate(enrdate = floor_date(S1EnrollDate, "month")) %>% 
  group_by(Province, S1HospitalID, enrdate, FinalResult) %>% 
  tally() %>%
  ungroup()
df_pos3wk <- tblSection1 %>%
  left_join(LabPCRResult_w, by = "CFID") %>%
  filter(!is.na(S1EnrollDate), 
         TestDate > Sys.Date() - 22) %>%
  group_by(Province, S1HospitalID, FinalResult) %>% 
  tally() %>% 
  ungroup()
df_enrage <- tblSection1 %>%
  filter(!is.na(S1EnrollDate)) %>%
  group_by(Province, S1HospitalID, agegroup) %>% 
  tally() %>% 
  ungroup()
df_enrgender <- tblSection1 %>%
  filter(!is.na(S1EnrollDate)) %>%
  group_by(Province, S1HospitalID, S1Gender) %>% 
  tally() %>% 
  ungroup()
df_enrocc <- tblSection3 %>%
  mutate(S34Occupation = replace(S34Occupation, S34Occupation == "Other farmer", "Farmer")) %>%
  group_by(Province, S1HospitalID, S34Occupation) %>% 
  tally() %>% 
  ungroup()

# Diagnosis page
df_dx <- tblSection2 %>%
  left_join(LabPCRResult_w, by = "CFID") %>%
  select(CFID, Province, S1HospitalID, FinalResult,
         S2DxFever:S2DxOther, S2DxMeningitis) %>%
  rename_with(~ str_replace(., "S2Dx", ""), starts_with("S2Dx")) %>%
  rename(
    "Common Cold" = ComCold,
    "Bacterial Infection" = BacInfect,
    "Heart Diseases" = HeartDis,
    "Abdominal Pain" = AbPain,
    "Cerebrovascular Accident" = CerebAcci,
    "Chikungunya" = Chikun,
    "Gastrointestinal Tract Infection" = GastroInfect,
    "Renal Diseases" = Renal,
    "Viral Infection" = ViralInfect,
    "Alteration of Conscious" = AlterConscious,
    "Electrolyte Imbalance" = ElecImbalance,
    "Scrub Typhus" = ScrubTyphus
  ) %>%
  pivot_longer(cols = Fever:Meningitis,
               names_to = "Diagnosis",
               values_to = "y") %>%
  group_by(Province, S1HospitalID, FinalResult, Diagnosis) %>%
  tally(wt = y) 

# Underlying Page
df_un <- tblSection3 %>%
  select(CFID, Province,S1HospitalID, S35Diabetes,
    S35Obesity:S35Cancer,
    S35HIV:S35OthChronic,
    S35HisSmoke:S35Pregnancy
  ) %>%
  left_join(LabPCRResult_w %>% select(CFID, FinalResult), by = "CFID") %>%
  rename_with( ~ str_replace(., "S35", ""), starts_with("S35")) %>%
  rename(
    "Heart Diseases" = HeartDisease,
    "Immunodeficiency" = Immunodef,
    "History of TB" = HisTB,
    "Active TB" = ActiveTB,
    "Chrolesterol" = Chroles,
    "Cerebrovascular Diseases" = Cerebro,
    "Other Chronic Diseases" = OthChronic,
    "History of smoking" = HisSmoke,
    "Current smoking" = CurSmoke,
    "History of alcohol consumption" = HistAlcohol,
    "Current of alcohol consumption" = CurAlcohol
  ) %>%
  pivot_longer(
    cols = Diabetes:Pregnancy,
    names_to = "Underlying",
    values_to = "y"
  ) %>%
  group_by(Underlying, S1HospitalID, Province, FinalResult) %>%
  tally(wt = y)

#Risk Factor page
df_rf <- tblSection3 %>%
  select(
    CFID,
    Province,S1HospitalID,
    S33SuspectedCOVID19:S33VisitHos,
    S33VistiCrowded:S33TravelThai,
    S33TravelInter
  ) %>%
  left_join(LabPCRResult_w %>% select(CFID, FinalResult), by = "CFID") %>%
  mutate(
    "Contact COVID-19 cases" = ifelse(S33SuspectedCOVID19 == 1, 1, 0),
    "Contact febrile patients" = ifelse(S33FebrileHousehold == 1 |
                                          S33FebrileCoWorker == 1 |
                                          S33FebrileNeighbor == 1, 1, 0),
    "Contact persons with respiratory symptoms" = ifelse(S33RPS == 1, 1, 0),
    "Contact pneumonia patients" = ifelse(S33Pneumonia == 1, 1, 0),
    "Contact healthcare workers" = ifelse(S33HealthCarePer == 1 |
                                            S33VisitHos == 1, 1, 0),
    "Visit crowded places" = ifelse(S33VistiCrowded == 1 |
                                      S33PartPeople == 1, 1, 0),
    "History of travel" = ifelse(S33TravelThai == 1 |
                                   S33TravelInter, 1, 0)
  ) %>%
  select(-c(S33SuspectedCOVID19:S33VisitHos, S33VistiCrowded:S33TravelThai, S33TravelInter)) %>%
  pivot_longer(
    cols = "Contact COVID-19 cases":"History of travel",
    names_to = "Risk",
    values_to = "y"
  ) %>%
  group_by(Risk, Province, S1HospitalID,FinalResult) %>%
  tally(wt = y)

# Clinical Sign
df_sign <- tblSection3 %>%
  select(
    CFID,
    Province,
    S1HospitalID,
    S32Headache
    ,S32NeckStiff
    ,S32Tiredness
    ,S32Malaise
    ,S32Chills
    ,S32EyePain
    ,S32RedEyes
    ,S32YellowEyes
    ,S32NoseBleeding
    ,S32Hyposmia
    ,S32Dysgeusia
    ,S32MusclePain
    ,S32JointPain
    ,S32RedJoints
    ,S32BonePain
    ,S32BackPain
    ,S32ChestPain
    ,S32NoAppetite
    ,S32Nausea
    ,S32Vomiting
    ,S32BloodVomit
    ,S32AbdominalPain
    ,S32Diarrhea
    ,S32BloodStool
    ,S32BloodUrine
    ,S32Dysuria
    ,S32PaleSkin
    ,S32Rash
    ,S32Bruise
    ,S32Seizures
    ,S32Other
  ) %>%
  left_join(tblSection2 %>% select(CFID, S2Temp), by = "CFID") %>%
  mutate(S2Temp = ifelse(S2Temp >= 38, 1, 0)) %>%
  rename_with( ~ str_replace(., "S32", ""), starts_with("S32"))%>%
  rename(
    "Temperature >= 38.0 C" = S2Temp,
    "Stiff neck" = NeckStiff,
    "Eye pain" = EyePain,
    "Red eyes" = RedEyes,
    "Yellow eyes" = YellowEyes,
    "Muscle pain" = MusclePain,
    "Joint pain" =JointPain,
    "Red joints" = RedJoints,
    "Nose bleeding" = NoseBleeding,
    "Bone pain" = BonePain,
    "Back pain" = BackPain,
    "Chest pain" = ChestPain,
    "Pale skin" = PaleSkin,
    "No appetite" = NoAppetite,
    "Blood vomitting" = BloodVomit,
    "Abdominal pain" =AbdominalPain,
    "Blood stool"= BloodStool,
    "Blood urine" = BloodUrine) %>% 
  left_join(LabPCRResult_w %>% select(CFID, FinalResult), by = "CFID") %>%
  pivot_longer(
    cols = Headache:"Temperature >= 38.0 C",
    names_to = "Signs",
    values_to = "y"
  ) %>%
  group_by(Signs, Province, S1HospitalID, FinalResult) %>%
  tally(wt = y)

# Vaccination page
df_vac <-tblSection3 %>%
  left_join(LabPCRResult_w, by = "CFID") %>%
  group_by(Province, S1HospitalID, S33CovidVaccine,FinalResult) %>%
  tally() %>% 
  ungroup()
  
# Atk page
df_atk <-
  filter(tblSection3, S33ATK == 1 ) %>%
  select(CFID,    Province,  S1HospitalID,S33ATK, S33ATKResult1, S33ATKResult2)%>%
  mutate(FinalResult = case_when(S33ATKResult1 == 1~'Positive',
                                 S33ATKResult2 == 1~'Positive',
                                 S33ATKResult1 == 2~'Negative',
                                 S33ATKResult1 == 3~'Unknown',
                                 is.na(S33ATKResult1)~'Missing'
  )) %>% 
  group_by(Province, S1HospitalID, FinalResult) %>%
    tally() 

# Lab
df_lab <- LabPCRResult_l %>% 
  group_by(SpecType, FinalResult) %>% 
  tally()

# KAP page
df_kap1 <- tblSection3 %>%
  select(Province, S1HospitalID,
         S3604SickSpread, S3615CareLate:S3620) %>%
  pivot_longer(cols = S3604SickSpread:S3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(Province, S1HospitalID, kap, scale) %>%
  tally()

df_kap2 <- tblSection3 %>%
  select(Province, S1HospitalID,
         S3610MaskIn, S3613MaskOut, S3621:S3622) %>%
  pivot_longer(cols = S3610MaskIn:S3622,
               names_to = "kap",
               values_to = "scale") %>%
  filter(!is.na(scale)) %>%
  group_by(Province, S1HospitalID, kap, scale) %>%
  tally()

## Save data frames for dashboard in one data file (CFDashboard.RData) for later use ----------
save(
  list = c(
    "ddate",
    "df_scrw",
    "df_scrm",
    "df_scrage0",
    "df_scrage1",
    "df_scrage2",
    "df_scrgender",
    "df_screnrol",
    "df_eli",
    "df_enr",
    "df_pos3wk",
    "df_enrage",
    "df_enrgender",
    "df_enrocc",
    "df_dx",
    "df_un",
    "df_rf",
    "df_sign",
    "df_vac",
    "df_atk",
    "df_lab",
    "df_kap1",
    "df_kap2"
  ),
  file = paste0(data_folder, "/CFDashboard.RData")
)
