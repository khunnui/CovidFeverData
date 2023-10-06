#-------------------------------------------------------------------------------
# lcsec1
# Created 10/6/2023
#-------------------------------------------------------------------------------
lcsec1 <- lcsec1 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  rename(last_edit_date = `_lasteditdate`) %>% 

  # Delete unused columns
  select(-c(starts_with("_"))) %>%
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(where(is.POSIXct), ~ as.Date(.))
    
  )
