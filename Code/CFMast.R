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

set_vax_range <- function(edate, vdate, range) {

  ifelse(!is.na(vdate),
                cut(as.numeric(edate - vdate), breaks = c(-1, 31, 183, 365, Inf)),
                range)

}

CFMast <- filter(tblSection1, !is.na(cfid) & cfid != '09-9001-0') %>%
  left_join(tblSection2, by='cfid') %>%
  left_join(tblSection3, by='cfid') %>%
  left_join(tblSection4, by='cfid') %>%
  left_join(tblSection5, by='cfid') %>%
  left_join(LabPCRFinal, by='cfid') %>% 
  mutate(
    n_covax = rowSums(!is.na(.[c('s33cvname1','s33cvname2','s33cvname3','s33cvname4')])),
    s33cvdaterange1 = set_vax_range(s1enrolldate, s33cvdate1, s33cvdaterange1), 
    s33cvdaterange2 = set_vax_range(s1enrolldate, s33cvdate2, s33cvdaterange2), 
    p_cvdaterange = case_when(
      s33cvname1 == 2     ~ s33cvdaterange1,
      n_covax %in% c(2:4) ~ s33cvdaterange2)
  )
