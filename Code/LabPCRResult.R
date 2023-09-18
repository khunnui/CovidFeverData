#-------------------------------------------------------------------------------
# LabPCRResult
# 3/7/2022
#-------------------------------------------------------------------------------
LabPCRResult_l <- LabPCRResult %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%

  # Create cfid from first 9 characters of SpecimenID
  mutate(
    cfid = substr(specimenid, 1, 9),
    province = factor(
      ifelse(substr(cfid, 1, 2) %in% c('09', '11', '16'), 1, 2),
      levels = c(1, 2),
      labels = c('Nakorn Phanom', 'Tak')
    ),
    hospital = factor(
      as.integer(substr(cfid, 1, 2)),
      levels = c(9, 11, 16, 21, 22, 23),
      labels = c(
        "Nakorn Phanom",
        "Sri Songkhram",
        "That Phanom",
        "Mae Sot",
        "Umphang",
        "Tha Song Yang"
      )
    ),
    across(matches("date"), as.Date),
    spectype = factor(
      spectype,
      levels = c(1, 4, 7),
      labels = c("NP/OP swab", "Nasal swab", "Saliva")
    ),
    finalresult = na_if(finalresult, 0),
    finalresult_fac = factor(
      finalresult,
      levels = c(1, 3, 2),
      labels = c("Positive", "Negative", "Inconclusive")
    )
  )

LabPCRFinal <- LabPCRResult_l %>%
  # Select only 4 columns (ID, type, result, and test dates)
  select(cfid,
         spectype,
         finalresult,
         testdate_p,
         testdate_i,
         testdate_i2) %>%
  # All specimen types in one line
  pivot_wider(
    names_from = "spectype",
    values_from = c("finalresult", "testdate_p", "testdate_i", "testdate_i2")
  ) %>%
  # Final PCR result based on all specimen types
  mutate(
    finalresult = factor(
      pmin(
        `finalresult_NP/OP swab`,
        `finalresult_Nasal swab`,
        `finalresult_Saliva`,
        na.rm = TRUE
      ),
      levels = c(1, 3, 2),
      labels = c("Positive", "Negative", "Inconclusive")
    ),
    pcrtests = paste0(ifelse(!is.na(`finalresult_NP/OP swab`),`finalresult_NP/OP swab`,'.'),
                      ifelse(!is.na(`finalresult_Nasal swab`),`finalresult_Nasal swab`,'.'),
                                    ifelse(!is.na(`finalresult_Saliva`),`finalresult_Saliva`,'.')),
    testdate = pmin(
      `testdate_p_NP/OP swab`,
      `testdate_p_Nasal swab`,
      `testdate_p_Saliva`,
      `testdate_i_NP/OP swab`,
      `testdate_i_Nasal swab`,
      `testdate_i_Saliva`,
      `testdate_i2_NP/OP swab`,
      `testdate_i2_Nasal swab`,
      `testdate_i2_Saliva`,
      na.rm = TRUE
    )
  ) %>%
  select(
    -c(`finalresult_NP/OP swab`:`finalresult_Saliva`,
      `testdate_p_NP/OP swab`:`testdate_i2_Saliva`)
  )
