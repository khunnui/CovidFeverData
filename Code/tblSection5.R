#-------------------------------------------------------------------------------
# tblSection5
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection5 <- tblSection5 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  
  # Delete unused columns
  select(-c(starts_with("_"), remarks)) %>%
  
  # Remove rows without CFID
  filter(cfid != '__-____-_') %>%
  
  # Create/convert columns
  mutate(
    # Convert datetime to date
    across(matches("date"), as.Date),
    
    # Recode 0 to missing
    across(c(
      s5isadmit,
      matches('s5medadmin'),
      s5intub,
      s5dischargetype,
      s5dischargestatus
    ),
    function(f) {
      ifelse(f == 0, NA, f)
    }),
    
    # Recode 1, 2 to TRUE, FALSE
    across(c(
      s5covidpos,
      s5intub,
      s5ddengue:s5ddtropinf
    ),
    function(f) {
      f[!f %in% c(1, 2)] <- NA
      f == 1
    }),
    
    # Factor categorical variables
    s5isadmit = factor(
      s5isadmit,
      levels = c(1:7),
      labels = c(
        'Yes',
        'Yes - refer from other hosp',
        'No - but admit in project hosp',
        'No home quarantine',
        'No State Quarantine',
        'No admit in other hospital',
        'No Other'
      )
    ),
    across(matches('medadmin'),
           function(f) {
             factor(f,
                    levels = c(1:3),
                    labels = c('Oral', 'IV fluid', 'IM'))
           })
    
  )
