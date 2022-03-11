#-------------------------------------------------------------------------------
# tblSection1
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection1 <- tblSection1 %>%
  # Delete unused columns
  select(-starts_with("_")) %>%
  # Create/convert columns
  mutate(Province = ifelse(S1HospitalID %in% c(9, 11, 16), "Nakorn Phanom", "Tak"),
         # Factorize and label categorical columns
         S1HospitalID = factor(S1HospitalID,
                               levels = c(9,11,16,21,22,23),
                               labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang")),
         S1Gender = factor(S1Gender,
                           levels = c(1,2),
                           labels = c("Male","Female")),
         EnrolledPT = factor(as.numeric(substr(EnrolledPT, 1, 1)),
                             levels = c(1,2,3),
                             labels = c("Follow up","Not improve","Transfered")),
         # Set age > 100 years to missing
         S1Age_Year = ifelse(S1Age_Year < 100, S1Age_Year, NA),
         # Create age group
         agegroup = cut(
           S1Age_Year,
           breaks = c(-Inf, 4, 15, 30, 45, 60, Inf),
           labels = c("<5", "5-15", "16-30", "31-45", "46-60", ">60")),
         # Set CFID '__-____-_' to missing
         CFID = ifelse(CFID == '__-____-_', NA, CFID),
         PreCFID = ifelse(PreCFID == '__-____-_', NA, PreCFID),
         # Convert datetime to date
         across(matches("Date"), as.Date),
         BloodCollectDT = as.Date(BloodCollectDT),
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
         # Recode 2 in Y/N (1/2) to 0
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
                  Blood),
                function(f) {ifelse(f == 2, 0, f)}), 
         # Create CF_enrol based on a having a CFID
         CF_Enrol = factor(ifelse(!is.na(CFID), 1, 0),
                           levels = c(0,1),
                           labels = c('No','Yes')),
         OLDCF = factor(ifelse(!is.na(PreCFID), 1, 0),
                        levels = c(0,1),
                        labels = c('No','Yes')))
