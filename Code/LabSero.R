#-------------------------------------------------------------------------------
# LabSero
# 3/7/2022
#-------------------------------------------------------------------------------
LabSero <- LabSero %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%

  # Remove unused column
  select(-hospital) %>% 
  # Filter out invalid data
  filter(!is.na(igmcoi) | !is.na(iggcoi) | !is.na(iggquanti)) %>% 

  # Create/convert columns
  mutate(
    cfid = substr(specimenid, 1, 9),
    # Convert datetime to date
    across(matches("date"), as.Date),
    spectype = substr(specimenid,11,12)
  )

LabSero_l <- LabSero %>%
  select(
    cfid,
    spectype,
    igm = igminterpret,
    iggn = igginterpret,
    iggsq = iggquanti,
    iggs = iggquantiinterpret
  ) %>%
  pivot_wider(
    names_from = "spectype",
    values_from = c("igm", "iggn", "iggsq", "iggs"),
    names_vary = 'slowest'
  )
