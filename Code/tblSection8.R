#-------------------------------------------------------------------------------
# tblSection8
# 3/7/2022
#-------------------------------------------------------------------------------

set_vax_range <- function(edate, vdate, range) {

  ifelse(!is.na(vdate),
         cut(as.numeric(edate - vdate), breaks = c(-1, 31, 183, 365, Inf)),
         range)
  
}

tblSection8 <- tblSection8 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  rename(last_edit_date = `_lasteditdate`) %>% 
  
  # Delete unused columns
  select(-c(starts_with("_"), remarks)) %>%
  
  # Remove rows without CFID
  filter(cfid != '__-____-_') %>%
  
  # Get enrollment date from section 1
  left_join(tblSection1 %>% select(cfid, s1enrolldate), by = 'cfid') %>% 
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(matches("date") & !matches('daterange'), as.Date),
    
  
    
    # Recode 1, 2 to TRUE, FALSE
    across(c(
      s8headache,
      s8neckstiff,
      s8tiredness,
      s8malaise,
      s8chills,
      s8eyepain,
      s8redeyes,
      s8yelloweyes,
      s8nosebleeding,
      s8hyposmia,
      s8dysgeusia,
      s8musclepain,
      s8jointpain,
      s8redjoints,
      s8bonepain,
      s8backpain,
      s8chestpain,
      s8noappetite,
      s8nausea,
      s8vomiting,
      s8bloodvomit,
      s8abdominalpain,
      s8diarrhea,
      s8bloodstool,
      s8bloodurine,
      s8dysuria,
      s8paleskin,
      s8rash,
      s8bruise,
      s8seizures,
      s8other
       ),
    function(f) {
      f[!f %in% c(1, 2)] <- NA
      f == 1
    })
  )
