#-------------------------------------------------------------------------------
# LabPCRResult
# 3/7/2022
#-------------------------------------------------------------------------------
# LabPCRResult_l <- LabPCRResult %>%
#   # Create CFID from first 9 characters of SpecimenID
#   mutate(CFID = substr(SpecimenID, 1, 9),
#          across(matches("Date"), as.Date),
#          FinalResult = factor(FinalResult,
#                               levels = c(1, 2, 3),
#                               labels = c("Positive", "Inconclusive", "Negative"),
#                               ordered = TRUE))
LabPCRResult_l <- LabPCRResult %>%
  # Create CFID from first 9 characters of SpecimenID
  mutate(CFID = substr(SpecimenID, 1, 9),
         across(matches("Date"), as.Date))

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
      pmin(FinalResult_1, FinalResult_4, FinalResult_7, na.rm = TRUE),
      levels = c(3, 2, 1),
      labels = c("Negative", "Inconclusive", "Positive")
    ),
    TestDate = pmin(
      TestDate_P_1,
      TestDate_P_4,
      TestDate_P_7,
      TestDate_I_1,
      TestDate_I_4,
      TestDate_I_7,
      TestDate_I2_1,
      TestDate_I2_4,
      TestDate_I2_7,
      na.rm = TRUE
    )
  ) %>%
  select(-c(TestDate_P_1:TestDate_I2_7))
