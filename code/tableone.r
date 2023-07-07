#######################################################
### Deep Brain Stimulation: STN vs GPi.
### Table One
### 2023 Katrin Petermann
#######################################################


t1_variables <- c("sex", "age_surgery", "disease_duration", "time_after_surgery", "pdq39_si_preop",
                  "ledd_preop", "mu3_total_off_preop", "mu3_improvement_preop",
                  "seadl_worst_preop", "mms_preop", "hamd_total_preop", "sas_total_preop",
                  "vas_maxpain_week_preop", "marconi_total_preop", "insomnia_preop",
                  "stability_on_preop", "stimulator")
t1_factors <- c("sex", "stimulator")

t1 <- CreateTableOne(vars = t1_variables, strata = "stim_target", data = dd_all_wide,
                     factorVars = t1_factors)
  
t1 <- print(t1, printToggle = FALSE, showAllLevels = TRUE)

t1[, 5] <- c("n", "Gender (%)", "", "Age at surgery", "Disease duration (years)", 
             "Time from surgery to assessment (weeks)","PDQ39 SI", "LEDD", 
             "MDS-UPDRS-III (off-medication)", "MDS-UPDRS-III (% improvement)", 
             "Schwab & England", "MMS", "Hamilton", "Starkstein", "Pain", 
             "Marconi", "Insomnia", "Postural Stability", "Stimulator Type (%)", "") 
t1 <- t1[, c(5, 1, 2, 3, 4)]

write.csv(t1, file = "./figures/table1.csv")

## Matched table
tm_variables <- c("sex", "age_surgery", "disease_duration", "time_after_surgery", "pdq39_si_preop",
                  "ledd_preop", "mu3_total_off_preop", "mu3_improvement_preop",
                  "seadl_worst_preop", "mms_preop", "hamd_total_preop", "sas_total_preop",
                  "vas_maxpain_week_preop", "marconi_total_preop", "insomnia_preop",
                  "stability_on_preop", "stimulator")
tm_factors <- c("sex", "stimulator")

tm <- CreateTableOne(vars = tm_variables, strata = "stim_target", data = dd_match,
                     factorVars = tm_factors)

tm <- print(tm, printToggle = FALSE, showAllLevels = TRUE)

tm[, 5] <- c("n", "Gender (%)", "", "Age at surgery", "Disease duration (years)", 
             "Time from surgery to assessment (weeks)","PDQ39 SI", "LEDD", 
             "MDS-UPDRS-III (off-medication)", "MDS-UPDRS-III (% improvement)", 
             "Schwab & England", "MMS", "Hamilton", "Starkstein", "Pain", 
             "Marconi", "Insomnia", "Postural Stability", "Stimulator Type (%)", "") 
tm <- tm[, c(5, 1, 2, 3, 4)]

write.csv(tm, file = "./figures/table_matched.csv")
