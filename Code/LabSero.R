#-------------------------------------------------------------------------------
# tblSection1
# 3/7/2022
#-------------------------------------------------------------------------------
LabSero <- LabSero %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%

  filter(substr(specimenid, 1, 2) %in% c('09', '11', '16', '21', '22', '23')) %>% 
  # Create/convert columns
  mutate(
    cfid = substr(specimenid, 1, 9),
    province = ifelse(substr(cfid,1,2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
    s1hospitalid = factor(as.integer(substr(cfid,1,2)),
                          levels = c(9,11,16,21,22,23),
                          labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang")),
    # Convert datetime to date
    across(matches("date"), as.Date),
    SpecType = substr(specimenid,11,12)
  )

    
