#-------------------------------------------------------------------------------
# tblSection4
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection4 <- tblSection4 %>% 

  # Delete unused columns
  select(-starts_with("_")) %>%
  
  mutate(
    
    # Convert datetime to date
    across(matches("Date") & !matches("DateRange"), as.Date),
    
    # Convert test results to numeric
    S4DengueIgMResult = as.numeric(substr(S4DengueIgMResult,1,1)),
    S4DengueIgGResult = as.numeric(substr(S4DengueIgGResult,1,1)),
    S4DengueNSResult  = as.numeric(substr(S4DengueNSResult,1,1)),
    S4MelioResult     = as.numeric(substr(S4MelioResult,1,1)),
    S4LeptoResult     = as.numeric(substr(S4LeptoResult,1,1)),
    S4WFResult        = as.numeric(substr(S4WFResult,1,1)),
    S4WidalResult     = as.numeric(substr(S4WidalResult,1,1)),  
    S4Med1            = as.numeric(substr(S4Med1,1,2)),
    S4Med2            = as.numeric(substr(S4Med2,1,2)),
    S4Med3            = as.numeric(substr(S4Med3,1,2)),
    S4Med4            = as.numeric(substr(S4Med4,1,2)),
    S4Med5            = as.numeric(substr(S4Med5,1,2)),
    S4Med6            = as.numeric(substr(S4Med6,1,2)),
    S4Med7            = as.numeric(substr(S4Med7,1,2)),
    S4Med8            = as.numeric(substr(S4Med8,1,2)),
    S4Med9            = as.numeric(substr(S4Med9,1,2)),
    S4Med10           = as.numeric(substr(S4Med10,1,2)),
    
    # Recode 0 to missing
    across(c(S4CBlood,
             S4CUrine,
             S4CSputum,
             S4CCSF,
             S4COth,
             S4Med1:S4Med10,
             S4CBloodOrg1:S4CBloodOrg3,
             S4CUrineOrg1:S4CUrineOrg3,
             S4CSputumOrg1:S4CSputumOrg3,
             S4CCSFOrg1:S4CCSFOrg3,
             S4COthOrg1:S4COthOrg3), 
           function(f) {ifelse(f == 0, NA, f)}),
    
    # Standardize Platelets/WBCCount to one unit
    S4Plateletx10 = ifelse(!is.na(S4Platelets), S4Platelets/1000, S4Plateletx10),
    S4WBCCountx10 = ifelse(!is.na(S4WBCCount), S4WBCCount/1000, S4WBCCountx10)

  )