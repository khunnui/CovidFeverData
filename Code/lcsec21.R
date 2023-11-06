#-------------------------------------------------------------------------------
# lcsec2
# Created 10/6/2023
#-------------------------------------------------------------------------------
lcsec21 <- lcsec21 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  
 
  # Get enrollment date and province from tblSection1
  left_join(tblSection1 %>% select (cfid, s1enrolldate, province), by = 'cfid') %>%
  # Delete unused columns
  select(-c(starts_with("_"))) %>%
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(where(is.POSIXct), ~ as.Date(.))
    
  )
