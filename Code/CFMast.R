#------------------------------------------------------------------------------#
# Get files to merge by CFID                                                   #
# Section1 - enrolled only                                                     #
# Section2                                                                     #
# Section3                                                                     #
# Section4                                                                     #
# Section5                                                                     #
# LabPCRResult                                                                 #
#------------------------------------------------------------------------------#
library(tidyverse)
library(dplyr)

CFMast <- filter(tblSection1, !is.na(cfid)) %>%
  left_join(tblSection2, by='cfid') %>%
  left_join(tblSection3, by='cfid') %>%
  left_join(tblSection4, by='cfid') %>%
  left_join(tblSection5, by='cfid') %>%
  left_join(LabPCRFinal, by='cfid')
