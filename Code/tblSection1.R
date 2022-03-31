#-------------------------------------------------------------------------------
# tblSection1
# Created 3/7/2022
# Modified 3/30/2022
#-------------------------------------------------------------------------------
tblSection1 <- tblSection1 %>%
  
  # Delete unused columns
  select(-c(starts_with("_"), VarifyID)) %>%
  
  # Create/convert columns
  mutate(
    
    # Set CFID '__-____-_' to missing
    CFID = ifelse(CFID == '__-____-_', NA, CFID),
    PreCFID = ifelse(PreCFID == '__-____-_', NA, PreCFID),

    # Create EnrolledPT
    EnrolledPT = factor(substr(EnrolledPT, 1, 1),
                        levels = c("1", "2", "3"),
                        labels = c("Follow up", "Not improve", "Transfered")),
    
    # Convert datetime to date
    across(c(matches("Date"), BloodCollectDT), as.Date),

    # Recode 0 in Y/N (1/2) and multiple choices (1,2,3...) to missing
    across(c(S1IsTransfer,
             S1Fever,
             S1FeverHis:S1SputumProd,
             S1Return30:S1AgreePart,
             S1IsCommu,
             S1ReasonNo,
             S1ConsentBy,
             NP:Blood), 
           function(f) {ifelse(f == 0, NA, f)}),
    
    # Create CF_enrol and OLDCF based on a having a CFID
    CF_Enrol = ifelse(!is.na(CFID), 1, 2),
    OLDCF = ifelse(!is.na(PreCFID), 1, 2),
    
    # Create Province for HospitalID
    Province = factor(ifelse(S1HospitalID %in% c(9, 11, 16), 'Nakorn Phanom', 'Tak'),
                      levels = c('Nakorn Phanom', 'Tak')),
    
    # Rename and factor HospitalID
    Hospital = factor(S1HospitalID,
                      levels = c(9, 11, 16, 21, 22, 23),
                      labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang")),
    
    # Create agegroup
    #S1Age_Year = ifelse(S1Age_Year < 100, S1Age_Year, NA),
    agegroup = cut(S1Age_Year,
                   breaks = c(-Inf, 4, 15, 30, 45, 60, Inf),
                   labels = c("<5", "5-15", "16-30", "31-45", "46-60", ">60")),
    
    # Factor categorical variables
    S1Gender = factor(S1Gender,
                      levels = c(1, 2),
                      labels = c("Male","Female")),
    across(c(S1IsTransfer,
             S1MetPT,
             S1Fever,
             S1FeverHis:S1SputumProd,
             S1Return30:S1AgreePart,
             S1IsCommu,
             S1ConsentChild,
             S1Enroll5th,
             S1IsSample,
             NP:Nasal,
             Blood,
             CF_Enrol,
             OLDCF),
           function(f) {factor(f,
                              levels = c(1, 2),
                              labels = c("Yes","No"))})
    
  ) %>% 
  
  # Delete unused columns
  select(-S1HospitalID)
