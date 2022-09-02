#-------------------------------------------------------------------------------
# tblSection1
# Created 3/7/2022
#-------------------------------------------------------------------------------
tblSection1 <- tblSection1 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  
  # Delete unused columns
  select(-c(s1rn, starts_with("_"), varifyid)) %>%
  
  # Create/convert columns
  mutate(
    
    # set cfid '__-____-_' to missing
    cfid = ifelse(cfid == '__-____-_', NA, cfid),
    precfid = ifelse(precfid == '__-____-_', NA, precfid),
    
    # create province for hospitalid
    province = factor(
      ifelse(s1hospitalid %in% c(9, 11, 16), 1, 2),
      levels = c(1, 2),
      labels = c('Nakorn Phanom', 'Tak')
    ),
    
    # Rename and factor hospitalid
    hospital = factor(
      s1hospitalid,
      levels = c(9, 11, 16, 21, 22, 23),
      labels = c(
        "Nakorn Phanom",
        "Sri Songkhram",
        "That Phanom",
        "Mae Sot",
        "Umphang",
        "Tha Song Yang"
      )
    ),
    
    # Convert datetime to date
    across(c(matches("date"), bloodcollectdt), as.Date),
    
    # recode 0 in multiple choices (1,2,3...) to missing
    across(c(s1gender,
             s1nationality,
             s1consentby,
             s1reasonno,
             saliva),
           function(f) {
             ifelse(f == 0, NA, f)
           }),
    
    # Recode (1,2) in y/n to (TRUE,FALSE)
    across(c(
      s1istransfer,
      s1metpt,
      s1fever,
      s1feverhis:s1sputumprod,
      s1return30:s1agreepart,
      s1iscommu,
      s1consentchild,
      s1enroll5th,
      s1issample,
      np:nasal,
      blood
    ),
    function(f) {
      f[!f %in% c(1, 2)] <- NA
      f == 1
    }),
    
    # Convert enrolledpt
    enrolledpt = factor(
      substr(enrolledpt, 1, 1),
      levels = c("1", "2", "3"),
      labels = c("Follow up", "Not improve", "Transfered")
    ),
    
    # Create agegroup
    #s1age_year = ifelse(s1age_year < 100, s1age_year, NA),
    agegroup = cut(
      s1age_year,
      breaks = c(-Inf, 4, 15, 30, 45, 60, Inf),
      labels = c("<5", "5-15", "16-30", "31-45", "46-60", ">60")
    ),
    
    # Factor categorical variables
    s1gender = factor(
      s1gender,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    s1nationality = factor(
      s1nationality,
      levels = c(1, 2, 3, 4, 5, 6, 7),
      labels = c('Thai', 'Laos', 'Myanmar', 'Karen', 'Khmer', 'Viet', 'Other')
    ),
    s1consentby = factor(
      s1consentby,
      levels = c(1, 2, 3),
      labels = c('Patient', 'Guardian', 'Relative')
    ),
    s1reasonno = factor(
      s1reasonno,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8),
      labels = c(
        'No interest',
        'No time',
        'Fear',
        'No benefit',
        'Not want to test',
        'Legal parent not available',
        'Staff not available',
        'Other'
      )
    ),
    saliva = factor(
      saliva,
      levels = c(1, 2, 3),
      labels = c('Yes', 'Yes but not enough', 'No')
    )
    
  ) %>%
  
  # delete unused columns
  select(-s1hospitalid)
