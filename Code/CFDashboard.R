# Screening page
df_scr <- tblSection1 %>%
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
  left_join(LabPCRResult, by = "CFID") %>%
  mutate(enrdate = floor_date(S1EnrollDate, "month")) %>% 
  group_by(Province, S1HospitalID, enrdate, FinalResult) %>% 
  tally() %>%
  ungroup()
df_pos3wk <- tblSection1 %>%
  left_join(LabPCRResult, by = "CFID") %>%
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
df_vac <-tblSection3 %>%
  left_join(LabPCRResult, by = "CFID") %>%
  group_by(Province, S1HospitalID, S33CovidVaccine,FinalResult) %>%
  tally() %>% 
  ungroup()
  
df_kap1 <-tblSection3 %>%
  select(Province, S1HospitalID, 
         S3604SickSpread, S3615CareLate:S3620) %>% 
  pivot_longer(
    cols = S3604SickSpread:S3620,
    names_to = "kap",
    values_to = "scale"
  ) %>%
  filter(!is.na(scale)) %>% 
  group_by(Province, S1HospitalID, kap, scale) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(scale)

df_kap2 <-tblSection3 %>%
  select(Province, S1HospitalID, 
         S3610MaskIn, S3613MaskOut, S3621:S3622) %>% 
  pivot_longer(
    cols = S3610MaskIn:S3622,
    names_to = "kap",
    values_to = "scale"
  ) %>%
  filter(!is.na(scale)) %>% 
  group_by(Province, S1HospitalID, kap, scale) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(scale)

## Save data frames for dashboard in one data file (CFDashboard.RData) for later use ----------
save(
  list = c(
    "df_scr",
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
    "df_vac",
    "df_kap1",
    "df_kap2"
  ),
  file = paste0(data_folder, "/CFDashboard.RData")
)
