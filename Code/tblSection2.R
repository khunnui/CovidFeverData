#-------------------------------------------------------------------------------
# tblSection2
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection2 <- tblSection2 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  
  # Delete unused columns
  select(-c(starts_with("_"), remarks)) %>%
  
  # Remove rows without CFID
  filter(cfid != '__-____-_') %>%
  
  # Create/convert columns
  mutate(
    
    # Recode 999 to missing
    across(s2temp:s2pulse, function(f) {
      ifelse(f == 999, NA, f)
    }),
    
    # Factor categorical variable
    s2levconsciou = factor(
      s2levconsciou,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        'Conscious',
        'Confuse',
        'Stupor/Lethargy/Drowsy',
        'Semicoma',
        'Coma'
      )
    )
    
  )