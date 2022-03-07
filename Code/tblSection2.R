#-------------------------------------------------------------------------------
# tblSection2
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection2 <- tblSection2 %>%
  # Delete unused columns
  select(-starts_with("_")) %>%
  # Remove rows without CFID
  filter(CFID != '__-____-_') %>%
  # Convert 999 to missing
  mutate(Province = ifelse(substr(CFID,1,2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
         across(S2Temp:S2Pulse,
                function(f) {ifelse(f == 999, NA, f)}))