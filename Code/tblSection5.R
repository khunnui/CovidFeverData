#-------------------------------------------------------------------------------
# tblSection5
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection5 <- tblSection5 %>%
  # Delete unused columns
  select(-starts_with("_")) %>%
  mutate(Province = ifelse(substr(CFID,1,2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
         S1HospitalID = factor(as.integer(substr(CFID,1,2)),
                               levels = c(9,11,16,21,22,23),
                               labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang"))
         )