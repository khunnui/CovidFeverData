ddate <- max(tblSection1$S1ScreenDate, na.rm = TRUE)

#-------------------------------------------------------------------------------
# Screening page
#-------------------------------------------------------------------------------

df_scrw <- tblSection1 %>%
  mutate(scrdate = floor_date(S1ScreenDate, "week", week_start = 1)) %>%
  group_by(Province, Hospital, scrdate) %>%
  tally()

df_scrm <- tblSection1 %>%
  mutate(scrdate = floor_date(S1ScreenDate, "month")) %>%
  group_by(Province, Hospital, scrdate) %>%
  tally()

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
  )

df_scrage2 <- tblSection1 %>%
  group_by(Hospital) %>%
  summarize(
    n = n(),
    min = min(S1Age_Year, na.rm = TRUE),
    q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
    median = median(S1Age_Year, na.rm = TRUE),
    mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
    q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
    max = max(S1Age_Year, na.rm = TRUE)
  )

df_scrgender <- tblSection1 %>%
  group_by(Province, Hospital, S1Gender) %>%
  tally()

df_screnrol <- tblSection1 %>%
  group_by(Province, Hospital, OLDCF, CF_Enrol) %>%
  tally()

#-------------------------------------------------------------------------------
# Enrollment page
#-------------------------------------------------------------------------------

df_eliw <- tblSection1 %>%
  filter(S1Eligible == 'Yes') %>% # Eligible only
  mutate(scrdate = floor_date(S1ScreenDate, "week", week_start = 1)) %>%
  group_by(Province, Hospital, scrdate) %>%
  tally()

df_elim <- tblSection1 %>%
  filter(S1Eligible == 'Yes') %>% # Eligible only
  mutate(scrdate = floor_date(S1ScreenDate, "month")) %>%
  group_by(Province, Hospital, scrdate) %>%
  tally()

df_enrw <- CFMast %>%
  mutate(enrdate = floor_date(S1EnrollDate, "week", week_start = 1)) %>%
  group_by(Province, Hospital, enrdate, FinalResult) %>%
  tally()

df_enrm <- CFMast %>%
  mutate(enrdate = floor_date(S1EnrollDate, "month")) %>%
  group_by(Province, Hospital, enrdate, FinalResult) %>%
  tally()

df_pos3wk <- CFMast %>%
  filter(TestDate > Sys.Date() - 22) %>%
  group_by(Province, Hospital, FinalResult) %>%
  tally()

df_enrage <- CFMast %>%
  group_by(Province, Hospital, agegroup) %>%
  tally()

df_enrgender <- CFMast %>%
  group_by(Province, Hospital, S1Gender) %>%
  tally()

df_enrocc <- CFMast %>%
  mutate(S34Occupation = replace(S34Occupation, S34Occupation == "Other farmer", "Farmer")) %>%
  group_by(Province, Hospital, S34Occupation) %>%
  tally()

#-------------------------------------------------------------------------------
# Diagnosis page
#-------------------------------------------------------------------------------

df_dx <- CFMast %>%
  select(CFID,
         Province,
         Hospital,
         FinalResult,
         S2DxFever:S2DxOther,
         S2DxMeningitis) %>%
  rename_with( ~ str_replace(., "S2Dx", ""), starts_with("S2Dx")) %>%
  rename(
    "Common cold" = ComCold,
    "Bacterial infection" = BacInfect,
    "Heart diseases" = HeartDis,
    "Abdominal pain" = AbPain,
    "Cerebrovascular accident" = CerebAcci,
    "Chikungunya" = Chikun,
    "Gastrointestinal tract infection" = GastroInfect,
    "Renal diseases" = Renal,
    "Viral infection" = ViralInfect,
    "Alteration of conscious" = AlterConscious,
    "Electrolyte imbalance" = ElecImbalance,
    "Scrub typhus" = ScrubTyphus
  ) %>%
  pivot_longer(cols = Fever:Meningitis,
               names_to = "Diagnosis",
               values_to = "y") %>%
  group_by(Province, Hospital, FinalResult, Diagnosis) %>%
  tally(wt = y)

#-------------------------------------------------------------------------------
# Underlying Page
#-------------------------------------------------------------------------------
df_un <- CFMast %>%
  select(
    CFID,
    Province,
    Hospital,
    S35Diabetes,
    S35Obesity:S35Cancer,
    S35HIV:S35OthChronic,
    S35HisSmoke:S35Pregnancy,
    FinalResult
  ) %>%
  rename_with(~ str_replace(., "S35", ""), starts_with("S35")) %>%
  rename(
    "Heart diseases" = HeartDisease,
    "Immunodeficiency" = Immunodef,
    "History of TB" = HisTB,
    "Active TB" = ActiveTB,
    "Chrolesterol" = Chroles,
    "Cerebrovascular diseases" = Cerebro,
    "Other chronic diseases" = OthChronic,
    "History of smoking" = HisSmoke,
    "Current smoking" = CurSmoke,
    "History of alcohol consumption" = HistAlcohol,
    "Current of alcohol consumption" = CurAlcohol
  ) %>%
  pivot_longer(cols = Diabetes:Pregnancy,
               names_to = "Underlying",
               values_to = "y") %>%
  group_by(Underlying, Hospital, Province, FinalResult) %>%
  summarise(n = sum(y[y == 1], na.rm = TRUE))

#-------------------------------------------------------------------------------
#Risk Factor page
#-------------------------------------------------------------------------------

df_rf <- CFMast %>%
  select(
    CFID,
    Province,
    Hospital,
    S33SuspectedCOVID19:S33VisitHos,
    S33VistiCrowded:S33TravelThai,
    S33TravelInter,
    FinalResult
  ) %>%
  mutate(
    "Contact COVID-19 cases" = ifelse(S33SuspectedCOVID19 == 1, 1, 0),
    "Contact febrile patients" = ifelse(
      S33FebrileHousehold == 1 |
        S33FebrileCoWorker == 1 |
        S33FebrileNeighbor == 1,
      1,
      0
    ),
    "Contact persons with respiratory symptoms" = ifelse(S33RPS == 1, 1, 0),
    "Contact pneumonia patients" = ifelse(S33Pneumonia == 1, 1, 0),
    "Contact healthcare workers" = ifelse(S33HealthCarePer == 1 |
                                            S33VisitHos == 1, 1, 0),
    "Visit crowded places" = ifelse(S33VistiCrowded == 1 |
                                      S33PartPeople == 1, 1, 0),
    "History of travel" = ifelse(S33TravelThai == 1 |
                                   S33TravelInter, 1, 0)
  ) %>%
  select(
    -c(
      S33SuspectedCOVID19:S33VisitHos,
      S33VistiCrowded:S33TravelThai,
      S33TravelInter
    )
  ) %>%
  pivot_longer(
    cols = "Contact COVID-19 cases":"History of travel",
    names_to = "Risk",
    values_to = "y"
  ) %>%
  group_by(Risk, Province, Hospital, FinalResult) %>%
  tally(wt = y)

#-------------------------------------------------------------------------------
# Clinical Sign
#-------------------------------------------------------------------------------
df_sign <- CFMast %>%
  select(
    CFID,
    Province,
    Hospital,
    S2Temp,
    S32Headache,
    S32NeckStiff,
    S32Tiredness,
    S32Malaise,
    S32Chills,
    S32EyePain,
    S32RedEyes,
    S32YellowEyes,
    S32NoseBleeding,
    S32Hyposmia,
    S32Dysgeusia,
    S32MusclePain,
    S32JointPain,
    S32RedJoints,
    S32BonePain,
    S32BackPain,
    S32ChestPain,
    S32NoAppetite,
    S32Nausea,
    S32Vomiting,
    S32BloodVomit,
    S32AbdominalPain,
    S32Diarrhea,
    S32BloodStool,
    S32BloodUrine,
    S32Dysuria,
    S32PaleSkin,
    S32Rash,
    S32Bruise,
    S32Seizures,
    S32Other,
    FinalResult
  ) %>%
  mutate(S2Temp = ifelse(S2Temp >= 38, 1, 0)) %>%
  rename_with(~ str_replace(., "S32", ""), starts_with("S32")) %>%
  rename(
    "Temperature >= 38.0 C" = S2Temp,
    "Stiff neck" = NeckStiff,
    "Eye pain" = EyePain,
    "Red eyes" = RedEyes,
    "Yellow eyes" = YellowEyes,
    "Muscle pain" = MusclePain,
    "Joint pain" = JointPain,
    "Red joints" = RedJoints,
    "Nose bleeding" = NoseBleeding,
    "Bone pain" = BonePain,
    "Back pain" = BackPain,
    "Chest pain" = ChestPain,
    "Pale skin" = PaleSkin,
    "No appetite" = NoAppetite,
    "Blood vomitting" = BloodVomit,
    "Abdominal pain" = AbdominalPain,
    "Blood stool" = BloodStool,
    "Blood urine" = BloodUrine
  ) %>%
  pivot_longer(
    cols = `Temperature >= 38.0 C`:Other,
    names_to = "Signs",
    values_to = "y"
  ) %>%
  group_by(Signs, Province, Hospital, FinalResult) %>%
  tally(wt = y)

df_signBox <- CFMast %>%
  select(CFID,
         Province,
         Hospital,
         S5CovidPos,
         S5Intub,
         S5DishargeType) %>%
  filter(S5CovidPos == 1) %>%
  group_by(Province, Hospital, S5Intub, S5DishargeType) %>%
  tally()

#-------------------------------------------------------------------------------
# Vaccination page
#-------------------------------------------------------------------------------

df_vac <- CFMast %>%
  group_by(Province, Hospital, S33CovidVaccine, FinalResult) %>%
  tally()

#-------------------------------------------------------------------------------
# Atk page
#-------------------------------------------------------------------------------
df_atk <- CFMast %>% 
  filter(S33ATK == 1) %>%
  select(CFID,
         Province,
         Hospital,
         S33ATK,
         S33ATKResult1,
         S33ATKResult2) %>%
  mutate(FinalResult = factor(
    case_when(
      S33ATKResult1 == 1 | S33ATKResult2 == 1 ~ 'Positive',
      S33ATKResult1 == 2 |
        S33ATKResult2 == 2 ~ 'Negative',
      TRUE                                    ~
        'Unknown'
    ),
    levels = c('Positive', 'Negative', 'Unknown')
  )) %>%
  group_by(Province, Hospital, FinalResult) %>%
  tally()

#-------------------------------------------------------------------------------
# SARS-Cov2 detection page
#-------------------------------------------------------------------------------

df_lab <- LabPCRResult_l %>%
  group_by(Province, Hospital, SpecType, FinalResult_fac) %>%
  tally() %>%
  rename(FinalResult = FinalResult_fac)

df_labpos <- LabPCRResult_l %>%
  # Select only 4 columns (ID, type, result, and test dates)
  select(CFID,
         Province,
         Hospital,
         SpecType,
         FinalResult) %>%
  # All specimen types in one line
  pivot_wider(names_from = "SpecType",
              values_from = "FinalResult") %>%
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
  group_by(Province, Hospital, specimens) %>%
  tally()

#-------------------------------------------------------------------------------
# KAP page
#-------------------------------------------------------------------------------

df_kap1 <- CFMast %>%
  select(Province, Hospital,
         S3604SickSpread, S3615CareLate:S3620) %>%
  pivot_longer(cols = S3604SickSpread:S3620,
               names_to = "kap",
               values_to = "scale") %>%
  #mutate(scale = fct_rev(scale)) %>%
  filter(!is.na(scale)) %>%
  group_by(Province, Hospital, kap, scale) %>%
  tally()

df_kap2 <- CFMast %>%
  select(Province, Hospital,
         S3610MaskIn, S3613MaskOut, S3621:S3622) %>%
  pivot_longer(cols = S3610MaskIn:S3622,
               names_to = "kap",
               values_to = "scale") %>%
  filter(!is.na(scale)) %>%
  group_by(Province, Hospital, kap, scale) %>%
  tally()

#-------------------------------------------------------------------------------
# Save data frames for dashboard in one data file (CFDashboard.RData)
#-------------------------------------------------------------------------------

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
