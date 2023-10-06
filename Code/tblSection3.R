#-------------------------------------------------------------------------------
# tblSection3
# 3/7/2022
#-------------------------------------------------------------------------------
library(labelled)
set_vax_range <- function(edate, vdate, range) {

  ifelse(!is.na(vdate),
         cut(as.numeric(edate - vdate), breaks = c(-1, 31, 183, 365, Inf)),
         range)
  
}

tblSection3 <- tblSection3 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  rename(last_edit_date = `_lasteditdate`) %>% 
  
  # Delete unused columns
  select(-c(starts_with("_"), remarks)) %>%
  
  # Remove rows without CFID
  filter(cfid != '__-____-_') %>%
  
  # Get enrollment date from section 1
  left_join(tblSection1 %>% select(cfid, s1enrolldate, s1feveronsetdate), by = 'cfid') %>% 
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(matches("date") & !matches('daterange'), as.Date),
    
    # Recode 0 to missing
    across(c(
      s33suspectedcovid19:s33pneumonia,
      matches('range'),
      s33cvdoc,
      s33atk,
      s33atkresult1,
      s33atktestby1,
      s33atkresult2,
      s33atktestby2,
      s34occupation,
      s36education,
      s36wherelive,
      s3604sickspread,
      s3606:s3607,
      s3610maskin,
      s3612crowd,
      s3613maskout,
      s3615carelate:s3622
    ),
    function(f) {
      ifelse(f == 0, NA, f)
    }),
    
    # Recode 1, 2 to TRUE, FALSE
    across(c(
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
      s33healthcareper,
      s33visithos,
      s33visticrowded:s33travelthai,
      s33travelinter,
      s33hascovid,
      s33covidvaccine,
      s33influvaccine,
      s35diabetes,
      s35obesity:s35cancer,
      s35hiv:s35othchronic,
      s35hissmoke:s35pregnancy,
      s3624stopwork,
      s3625reiative,
      s3630,
      s3631
    ),
    function(f) {
      f[!f %in% c(1, 2)] <- NA
      f == 1
    }),
    
    # Convert ATK date range from text to factor
    across(matches('atkrange'),
           function(f) {
             factor(
               recode(
                 substr(f, 1, 1),
                 '<' = 1,
                 '7' = 2,
                 '>' = 3
               ),
               levels = c(1:3),
               labels = c('<7 Day', '7-14 Day', '>14 Day')
             )
           }),
    
    # Calculate s32anysymptom
    s32anysymptom = s32headache |
      s32neckstiff |
      s32tiredness |
      s32malaise |
      s32chills |
      s32eyepain |
      s32redeyes |
      s32yelloweyes |
      s32nosebleeding |
      s32hyposmia |
      s32dysgeusia |
      s32musclepain |
      s32jointpain |
      s32redjoints |
      s32bonepain |
      s32backpain |
      s32chestpain |
      s32noappetite |
      s32nausea |
      s32vomiting |
      s32bloodvomit |
      s32abdominalpain |
      s32diarrhea |
      s32bloodstool |
      s32bloodurine |
      s32dysuria |
      s32paleskin |
      s32rash |
      s32bruise |
      s32seizures |
      s32other,
    
    # Calcualte s33anyrisk
    s33anyrisk = s33suspectedcovid19 |
      s33febrilehousehold |
      s33febrilecoworker |
      s33febrileneighbor |
      s33rps |
      s33pneumonia |
      s33healthcareper |
      s33visithos |
      s33visticrowded |
      s33partpeople |
      s33travelthai |
      s33travelinter,
    
    # Calculate s35anycomorbid
    s35anycomorbid = s35diabetes |
      s35obesity |
      s35hypertension |
      s35heartdisease |
      s35asthma |
      s35copd |
      s35cancer |
      s35hiv |
      s35immunodef |
      s35histb |
      s35activetb |
      s35liver |
      s35thyroid |
      s35thalassemia |
      s35anemia |
      s35renal |
      s35chroles |
      s35cerebro |
      s35othchronic |
      s35hissmoke |
      s35cursmoke |
      s35histalcohol |
      s35curalcohol |
      s35pregnancy,
    
    # Factor categorical variables
    across(c(s33suspectedcovid19:s33pneumonia, s3606, s33atk),
           function(f) {
             factor(f,
                    levels = c(1:3),
                    labels = c('Yes',
                               'No',
                               'Not known'))
           }),
    s33hascovidrange =
      factor(
        s33hascovidrange,
        levels = c(1:4),
        labels = c('1-3 Month', '4-6 Month', '7-12 Month', '> 1 Year')
      ),
    # s33covidvaccine = factor(
    #   s33covidvaccine,
    #   levels = c(TRUE, FALSE),
    #   labels = c("Vaccinated", "Unvaccinated")
    # ),
    s33cvdoc =
      factor(
        s33cvdoc,
        levels = c(1:2),
        labels = c('Verbal', 'Vaccine book')
      ),
    across(matches('daterange'),
           function(f) {
             factor(
               f,
               levels = c(1:4),
               labels = c('<1 Month', '1-6 Month', '7-12 Month', '> 1 Year')
             )
           }),
    across(matches('atkresult'),
           function(f) {
             factor(f,
                    levels = c(1:3),
                    labels = c('Positive', 'Negative', 'UnKnown'))
           }),
    across(matches('atktestby'),
           function(f) {
             factor(f,
                    levels = c(1:2),
                    labels = c('Self', 'Medical person'))
           }),
    s34occupation = factor(
      s34occupation,
      levels = c(1:30, 99),
      labels = c(
        'Farmer',
        'Tour leader',
        'Doctor',
        'Nurse',
        'Fish in the pond',
        'Slaughterhouse',
        'Driver',
        'Maid',
        'Other farmer',
        'Garbage man',
        'Veterinary',
        'Foreign workers',
        'Fisherman',
        'Drain cleaner',
        'Civil servant',
        'Student',
        'Ranch',
        'Restaurant',
        'Forest guard',
        'Retired',
        'Monk',
        'Forester/Gardener',
        'Contractor',
        'Business',
        'Preschooler',
        'Merchant',
        'Unemployed',
        'Company employee',
        'Factory worker',
        'Mall/entertainment',
        'Other'
      )
    ),
    s36education = factor(
      s36education,
      levels = c(1:7),
      labels = c(
        '< Primary',
        'Primary',
        'Secondary',
        'High',
        'Bachelor',
        '> Bachelor',
        'Other'
      )
    ),
    s36wherelive = factor(
      s36wherelive,
      levels = c(1:3),
      labels = c('Outside municipal', 'Inside municipal', 'Unknown')
    ),
    across(c(s3604sickspread, s3615carelate:s3620),
           function(f) {
             factor(
               f,
               levels = c(5:1),
               labels = c(
                 'Strongly disagree',
                 'Disagree',
                 'Neither agree/disagree',
                 'Agree',
                 'Strongly agree'
               )
             )
           }),
    s3607 = factor(
      s3607,
      levels = c(1:6),
      labels = c(
        'Indifferent',
        'Feel scared',
        'Feel so scared',
        'Should not be serious',
        'Scared but not anything serious',
        'Other'
      )
    ),
    across(c(s3610maskin, s3613maskout, s3621:s3622),
           function(f) {
             factor(
               f,
               levels = c(5:1),
               labels = c('Never',
                          'Rarely',
                          'Occasionally',
                          'Most of the time',
                          'Always')
             )
           }),
    s3612crowd = factor(
      s3612crowd,
      levels = c(1:4),
      labels = c(
        'Daily/almost daily',
        '> 7 days/past 2 wks',
        '<= 7 days/past 2 weeks',
        'Never'
      )
    ),
    jj = rowSums(across(num_range("s33cvname", 1:10)) == 2, na.rm = TRUE),  # J&J
    vv = rowSums(across(num_range("s33cvname", 1:10)) == 1 |
                 across(num_range("s33cvname", 1:10)) == 7, na.rm = TRUE),  # viral vector
    mr = rowSums(across(num_range("s33cvname", 1:10)) == 3 |
                 across(num_range("s33cvname", 1:10)) == 4, na.rm = TRUE),  # mRNA
    iv = rowSums(across(num_range("s33cvname", 1:10)) == 5 |
                 across(num_range("s33cvname", 1:10)) == 6, na.rm = TRUE),  # inactivated virus
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
      labels = c('Not vaccinated', 'Not fully vaccinated', 'Fully vaccinated', 'Vaccinated but no information')
    ),
    cvdate_l = pmax(s33cvdate1, s33cvdate2, s33cvdate3, s33cvdate4, s33cvdate5, s33cvdate6, na.rm = TRUE),
    cvtime = as.numeric(difftime(s1feveronsetdate, cvdate_l, units = 'days')),
    jjdate1 = pmin(
      if_else(s33cvname1 == 2, s33cvdate1, NA_Date_),
      if_else(s33cvname2 == 2, s33cvdate2, NA_Date_),
      if_else(s33cvname3 == 2, s33cvdate3, NA_Date_),
      if_else(s33cvname4 == 2, s33cvdate4, NA_Date_),
      if_else(s33cvname5 == 2, s33cvdate5, NA_Date_),
      if_else(s33cvname6 == 2, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    vvdate1 = pmin(
      if_else(s33cvname1 %in% c(1, 7), s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% c(1, 7), s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% c(1, 7), s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% c(1, 7), s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% c(1, 7), s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% c(1, 7), s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    vvdate2 = pmin(
      if_else(s33cvname1 %in% c(1, 7) & s33cvdate1 > vvdate1, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% c(1, 7) & s33cvdate2 > vvdate1, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% c(1, 7) & s33cvdate3 > vvdate1, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% c(1, 7) & s33cvdate4 > vvdate1, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% c(1, 7) & s33cvdate5 > vvdate1, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% c(1, 7) & s33cvdate6 > vvdate1, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    mrdate1 = pmin(
      if_else(s33cvname1 %in% 3:4, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 3:4, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 3:4, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 3:4, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 3:4, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 3:4, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    mrdate2 = pmin(
      if_else(s33cvname1 %in% 3:4 & s33cvdate1 > mrdate1, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 3:4 & s33cvdate2 > mrdate1, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 3:4 & s33cvdate3 > mrdate1, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 3:4 & s33cvdate4 > mrdate1, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 3:4 & s33cvdate5 > mrdate1, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 3:4 & s33cvdate6 > mrdate1, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
    ivdate1 = pmin(
      if_else(s33cvname1 %in% 5:6, s33cvdate1, NA_Date_),
      if_else(s33cvname2 %in% 5:6, s33cvdate2, NA_Date_),
      if_else(s33cvname3 %in% 5:6, s33cvdate3, NA_Date_),
      if_else(s33cvname4 %in% 5:6, s33cvdate4, NA_Date_),
      if_else(s33cvname5 %in% 5:6, s33cvdate5, NA_Date_),
      if_else(s33cvname6 %in% 5:6, s33cvdate6, NA_Date_),
      na.rm = TRUE
    ),
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
    fulltime = as.numeric(difftime(s1feveronsetdate, fulldate, units = 'days')),
    
    s33cvdaterange1 = set_vax_range(s1enrolldate, s33cvdate1, s33cvdaterange1), 
    s33cvdaterange2 = set_vax_range(s1enrolldate, s33cvdate2, s33cvdaterange2), 
    s33cvdaterange3 = set_vax_range(s1enrolldate, s33cvdate3, s33cvdaterange3), 
    s33cvdaterange4 = set_vax_range(s1enrolldate, s33cvdate4, s33cvdaterange4), 
    s33cvdaterange5 = set_vax_range(s1enrolldate, s33cvdate5, s33cvdaterange5), 
    s33cvdaterange6 = set_vax_range(s1enrolldate, s33cvdate6, s33cvdaterange6), 
    s33cvdaterange7 = set_vax_range(s1enrolldate, s33cvdate7, s33cvdaterange7), 
    s33cvdaterange8 = set_vax_range(s1enrolldate, s33cvdate8, s33cvdaterange8), 
    s33cvdaterange9 = set_vax_range(s1enrolldate, s33cvdate9, s33cvdaterange9), 
    s33cvdaterange10 = set_vax_range(s1enrolldate, s33cvdate10, s33cvdaterange10)
  ) %>% 
  set_variable_labels(
    cvtime  = 'Time since last dose (days)',
    fulltime = "Time since fully vaccinated (days)"
  ) %>% 
  # Remove variable from section 1
  select(-s1enrolldate,-s1feveronsetdate)
