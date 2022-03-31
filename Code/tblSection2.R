#-------------------------------------------------------------------------------
# tblSection2
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection2 <- tblSection2 %>%
  
  # Delete unused columns
  select(-starts_with("_")) %>%
  
  # Remove rows without CFID
  filter(CFID != '__-____-_') %>%
  
  mutate(
    
    # Recode 999 to missing
    across(S2Temp:S2Pulse, function(f) {ifelse(f == 999, NA, f)}),
    
    # Factor categorical variable
    S2LevConsciou = factor(S2LevConsciou,
                           levels = c(1, 2, 3),
                           labels = c('Conscious', 'Confuse', 'Stupor/Lethargy/Drowsy'))

  )