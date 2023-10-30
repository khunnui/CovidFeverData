#-------------------------------------------------------------------------------
# lcsec2
# Created 10/6/2023
#-------------------------------------------------------------------------------
lcsec2 <- lcsec2 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  rename(last_edit_date = `_lasteditdate`) %>% 

 
  # Get enrollment date and province from tblSection1
  left_join(tblSection1 %>% select (cfid, s1enrolldate, province), by = 'cfid') %>%
  # Delete unused columns
  select(-c(starts_with("_"))) %>%
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(where(is.POSIXct), ~ as.Date(.))
    
  )
