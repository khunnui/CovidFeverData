#-------------------------------------------------------------------------------
# LabPCRResult
# 3/7/2022
#-------------------------------------------------------------------------------
LabPCRResult_l <- LabPCRResult %>%
  # Create CFID from first 9 characters of SpecimenID
  mutate(
    CFID = substr(SpecimenID, 1, 9),
    Province = ifelse(substr(CFID, 1, 2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
    S1HospitalID = factor(
      as.integer(substr(CFID, 1, 2)),
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
    across(matches("Date"), as.Date),
    SpecType = factor(
      SpecType,
      levels = c(1, 4, 7),
      labels = c("NP+OP swab", "Nasal swab", "Saliva")
    ),
    FinalResult_fac = factor(
      FinalResult,
      levels = c(1, 2, 3),
      labels = c("Positive", "Inconclusive", "Negative")
    )
  )

LabPCRResult_w <- LabPCRResult_l %>%
  # Select only 4 columns (ID, type, result, and test dates)
  select(CFID,
         SpecType,
         FinalResult,
         TestDate_P,
         TestDate_I,
         TestDate_I2) %>%
  # All specimen types in one line
  pivot_wider(
    names_from = "SpecType",
    values_from = c("FinalResult", "TestDate_P", "TestDate_I", "TestDate_I2")
  ) %>%
  # Final PCR result based on all specimen types
  mutate(
    FinalResult = factor(
      pmin(
        `FinalResult_NP+OP swab`,
        `FinalResult_Nasal swab`,
        `FinalResult_Saliva`,
        na.rm = TRUE
      ),
      levels = c(1, 2, 3),
      labels = c("Positive", "Inconclusive", "Negative")
    ),
    TestDate = pmin(
      `TestDate_P_NP+OP swab`,
      `TestDate_P_Nasal swab`,
      `TestDate_P_Saliva`,
      `TestDate_I_NP+OP swab`,
      `TestDate_I_Nasal swab`,
      `TestDate_I_Saliva`,
      `TestDate_I2_NP+OP swab`,
      `TestDate_I2_Nasal swab`,
      `TestDate_I2_Saliva`,
      na.rm = TRUE
    )
  ) %>%
  select(
    -c(
      `FinalResult_NP+OP swab`:`FinalResult_Saliva`,
      `TestDate_P_NP+OP swab`:`TestDate_I2_Saliva`
    )
  )
