#-------------------------------------------------------------------------------
# tblSection1
# 3/7/2022
#-------------------------------------------------------------------------------
LabSero <- LabSero %>%
  filter(substr(SpecimenID, 1, 2) %in% c('09', '11', '16', '21', '22', '23')) %>% 
  # Create/convert columns
  mutate(
    CFID = substr(SpecimenID, 1, 9),
    Province = ifelse(substr(CFID,1,2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
    S1HospitalID = factor(as.integer(substr(CFID,1,2)),
                          levels = c(9,11,16,21,22,23),
                          labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang")),
    # Convert datetime to date
    across(matches("Date"), as.Date),
    SpecType = substr(SpecimenID,11,12)
  )

    
