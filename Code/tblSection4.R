#-------------------------------------------------------------------------------
# tblSection4
# 3/7/2022
#-------------------------------------------------------------------------------
tblSection4 <- tblSection4 %>%
  
  # Rename all column names to lowercase
  rename_all(tolower) %>%
  
  # Delete unused columns
  select(-starts_with("_")) %>%
  
  # Remove rows without CFID
  filter(cfid != '__-____-_') %>%
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(matches("date"), as.Date),
    
    # Convert test results to numeric
    s4dengueigmresult = as.numeric(substr(s4dengueigmresult, 1, 1)),
    s4dengueiggresult = as.numeric(substr(s4dengueiggresult, 1, 1)),
    s4denguensresult  = as.numeric(substr(s4denguensresult, 1, 1)),
    s4melioresult     = as.numeric(substr(s4melioresult, 1, 1)),
    s4leptoresult     = as.numeric(substr(s4leptoresult, 1, 1)),
    s4wfresult        = as.numeric(substr(s4wfresult, 1, 1)),
    s4widalresult     = as.numeric(substr(s4widalresult, 1, 1)),
    s4med1            = as.numeric(substr(s4med1, 1, 2)),
    s4med2            = as.numeric(substr(s4med2, 1, 2)),
    s4med3            = as.numeric(substr(s4med3, 1, 2)),
    s4med4            = as.numeric(substr(s4med4, 1, 2)),
    s4med5            = as.numeric(substr(s4med5, 1, 2)),
    s4med6            = as.numeric(substr(s4med6, 1, 2)),
    s4med7            = as.numeric(substr(s4med7, 1, 2)),
    s4med8            = as.numeric(substr(s4med8, 1, 2)),
    s4med9            = as.numeric(substr(s4med9, 1, 2)),
    s4med10           = as.numeric(substr(s4med10, 1, 2)),
    
    # Recode 0 to missing
    across(c(
      s4malariathick,
      s4malariathin,
      s4cbloodresult,
      s4curineresult,
      s4ccsfresult,
      s4csputumresult,
      s4cothresult,
      s4med1:s4med10,
      s4cbloodorg1:s4cbloodorg3,
      s4curineorg1:s4curineorg3,
      s4csputumorg1:s4csputumorg3,
      s4ccsforg1:s4ccsforg3,
      s4cothorg1:s4cothorg3
    ),
    function(f) {
      ifelse(f == 0, NA, f)
    }),
    
    # Recode 1, 2 to TRUE, FALSE
    across(c(s4urineex,
             s4cblood,
             s4curine,
             s4csputum,
             s4ccsf,
             s4coth),
           function(f) {
             f[!f %in% c(1, 2)] <- NA
             f == 1
           }),
    
    # Standardize Platelets/WBCCount to one unit
    s4plateletx10 = ifelse(!is.na(s4platelets), s4platelets / 1000, s4plateletx10),
    s4wbccountx10 = ifelse(!is.na(s4wbccount), s4wbccount / 1000, s4wbccountx10),
    
    # Factor categorical variables
    across(c(s4malariathick, s4malariathin),
           function(f) {
             factor(
               f,
               levels = c(1:7),
               labels = c(
                 'P. falciparum',
                 'P. vivax',
                 'P. olave',
                 'P. malariae',
                 'P. knowlesi',
                 'Negative',
                 'Not done'
               )
             )
           }),
    across(c(
      s4cbloodresult,
      s4curineresult,
      s4csputumresult,
      s4ccsfresult,
      s4cothresult
    ),
    function(f) {
      factor(f,
             levels = c(1:2),
             labels = c('Positive', 'Negative'))
    }),
    across(c(
      s4dengueigmresult,
      s4dengueiggresult,
      s4denguensresult,
      s4melioresult,
      s4leptoresult,
      s4wfresult,
      s4widalresult
    ),
    function(f) {
      factor(
        f,
        levels = c(0:3),
        labels = c('Not done', 'Positive', 'Negative', 'Indeterminate')
      )
    })
    
    
  )