#-------------------------------------------------------------------------------
# tblSection3
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection3 <- tblSection3 %>%
  select(-starts_with("_")) %>%
  mutate(Province = ifelse(substr(CFID,1,2) %in% c('09', '11', '16'), "Nakorn Phanom", "Tak"),
         S1HospitalID = factor(as.integer(substr(CFID,1,2)),
                               levels = c(9,11,16,21,22,23),
                               labels = c("Nakorn Phanom","Sri Songkhram","That Phanom","Mae Sot","Umphang","Tha Song Yang")),
         across(matches("Date") & !matches("DateRange"), as.Date),
         S34Occupation = factor(S34Occupation,
                                levels = c(1:30,99),
                                labels = c('Farmer',
                                           'Tour leader',
                                           'Doctor',
                                           'Nurse',
                                           'Fish in the pond',
                                           'Slaughterhouse',
                                           'Driver',
                                           'Maid',
                                           'Other farmer',
                                           'Garbage man',
                                           'Veterinary',
                                           'Foreign workers',
                                           'Fisherman',
                                           'Drain cleaner',
                                           'Civil servant',
                                           'Student',
                                           'Ranch',
                                           'Restaurant',
                                           'Forest guard',
                                           'Retired',
                                           'Monk',
                                           'Forester/Gardener',
                                           'Contractor',
                                           'Business',
                                           'Preschooler',
                                           'Merchant',
                                           'Unemployed',
                                           'Company employee',
                                           'Factory worker',
                                           'Mall/entertainment',
                                           'Other')),
        
         # Recode 2 in Y/N (1/2) to 0
         across(c(S32Headache:S32Other,S33ATK,S33CovidVaccine,S35Diabetes, S35Obesity:S35Cancer, S35HIV:S35OthChronic, S35HisSmoke:S35Pregnancy),
                function(f) {ifelse(f == 2, 0, f)}),
         S33CovidVaccine = factor(S33CovidVaccine, levels=c(1,0), labels = c("Vaccinated","Unvaccinated"))
         )
