#-------------------------------------------------------------------------------
# tblSection3
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection3 <- tblSection3 %>%
  
  # Delete unused columns
  select(-starts_with("_")) %>%
  
  # Remove rows without CFID
  filter(CFID != '__-____-_') %>%
  
  mutate(
    
    # Convert datetime to date
    across(matches("Date") & !matches("DateRange"), as.Date),
    
    # Recode 0 to missing
    across(c(S32Headache,
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
             S33SuspectedCOVID19:S33VisitHos,
             S33VistiCrowded:S33TravelThai,
             S33TravelInter,
             S33CovidVaccine,
             S33InfluVaccine,
             S35Diabetes,
             S35Obesity:S35Cancer,
             S35HIV:S35OthChronic,
             S35HisSmoke:S35Pregnancy,
             S3604SickSpread,
             S3606:S3607,
             S3610MaskIn,
             S3612Crowd,
             S3613MaskOut,
             S3615CareLate:S3622,
             S3624StopWork,
             S3625ReIative), 
           function(f) {ifelse(f == 0, NA, f)}),
    
    # Calculate s32anysymptom
    s32anysymptom = pmin(S32Headache,
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
                         na.rm = TRUE),
    
    # Calcualte s33anyrisk
    s33anyrisk = pmin(S33SuspectedCOVID19,
                      S33FebrileHousehold,
                      S33FebrileCoWorker,
                      S33FebrileNeighbor,
                      S33RPS,
                      S33Pneumonia,  
                      S33HealthCarePer,
                      S33VisitHos,
                      S33VistiCrowded,
                      S33PartPeople,
                      S33TravelThai,
                      S33TravelInter,
                      na.rm = TRUE),
    
    # Calculate s35anycomorbid
    s35anycomorbid = pmin(S35Diabetes,
                          S35Obesity,
                          S35Hypertension,
                          S35HeartDisease,
                          S35Asthma,
                          S35COPD,
                          S35Cancer,
                          S35HIV, 
                          S35Immunodef,
                          S35HisTB,
                          S35ActiveTB,
                          S35Liver,
                          S35Thyroid,
                          S35Thalassemia,
                          S35Anemia,
                          S35Renal,
                          S35Chroles,
                          S35Cerebro, 
                          S35OthChronic,
                          S35HisSmoke,
                          S35CurSmoke,
                          S35HistAlcohol, 
                          S35CurAlcohol,
                          na.rm = TRUE),
    
    # Factor categorical variables
    S34Occupation = factor(
      S34Occupation,
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
    S33CovidVaccine = factor(
      S33CovidVaccine,
      levels = c(1, 2),
      labels = c("Vaccinated", "Unvaccinated")
    ),
    across(
      c(S3604SickSpread, S3615CareLate:S3620),
      function(f) {
        factor(
          f,
          levels = c(1:5),
          labels = c('Strongly agree',
                     'Agree',
                     'Neither agree/disagree',
                     'Disagree',
                     'Strongly disagree')
        )
      }
    ),
    across(
      c(S3610MaskIn, S3613MaskOut, S3621:S3622),
      function(f) {
        factor(
          f,
          levels = c(1:5),
          labels = c('Always',
                     'Most of the time',
                     'Occasionally',
                     'Rarely',
                     'Never')
        )
      }
    )
  )
