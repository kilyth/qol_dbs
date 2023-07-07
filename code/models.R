#######################################################
### Deep Brain Stimulation: STN vs GPi.
### Models
### 2023 Katrin Petermann
#######################################################


#######################################################
### Risk Factor Model
#######################################################

rf_mod <- lm(pdq39_improvement_abs ~ stim_target + sex + age_surgery + disease_duration + 
                     mu3_total_off_preop + mu3_improvement_preop + seadl_worst_preop + 
                     ledd_preop + mms_preop + sas_total_preop + marconi_total_preop + 
                     hamd_total_preop + vas_maxpain_week_preop + insomnia_preop + 
                     stability_on_preop, data = dd_all_wide)

rf_relimp <- calc.relimp(rf_mod, rela = TRUE, type = "pmvd")

rf_tab <- tableRegression(rf_mod, xtable = FALSE, intercept = TRUE)
rf_tab$relimp <- c(NA, round(rf_relimp@pmvd, 3) * 100)

rf_tab$names <- c("Intercept", "Target GPi",  "Gender female", "Age at surgery", "Disease duration (years)", 
                        "MDS-UPDRS-III (off-medication)", "MDS-UPDRS-III (% improvement)", "Schwab & England", 
                        "LEDD", "MMS", "Starkstein", "Marconi", 
                        "Hamilton", "VAS worst pain", "Insomnia", 
                        "Postural Stability") 

rf_tab <- rf_tab[order(rf_tab$relimp, decreasing = TRUE, na.last = FALSE), 
                 c(5, 1, 2, 3, 4)]

tmp <- data.frame(coef = coef(rf_mod))
tmp$lower <- confint(rf_mod)[, 1]
tmp$upper <- confint(rf_mod)[, 2]
tmp$stars <- starsPval(summary(rf_mod)$coefficients[, "Pr(>|t|)"])
tmp$names <- c("intercept", "Target GPi",  "Gender female", "Age at surgery", "Disease duration (years)", 
               "MDS-UPDRS-III (off-medication)", "MDS-UPDRS-III (% improvement)", "Schwab & England", 
               "LEDD", "MMS", "Starkstein", "Marconi", 
               "Hamilton", "VAS worst pain", "Insomnia", 
               "Postural Stability")

tmp <- tmp[tmp$names != "intercept", ]
tmp$relimp <- round(rf_relimp@pmvd, 3) * 100
tmp <- tmp[order(tmp$relimp, decreasing = TRUE), ]
tmp$names <- ordered(tmp$names, levels = tmp$names[order(tmp$relimp, decreasing = TRUE)])

rf_mod_plot <- tmp


#######################################################
### Change Model
#######################################################

change_mod <- lm(pdq39_improvement_abs ~ stim_target + mu3_off_change + seadl_change + 
               ledd_change + mms_change + sas_change + marconi_change + 
               hamd_change + vas_maxpain_change + insomnia_change + 
               stability_on_change, data = dd_all_wide)

change_relimp <- calc.relimp(change_mod, rela = TRUE, type = "pmvd")

change_tab <- tableRegression(change_mod, xtable = FALSE, intercept = TRUE)
change_tab$relimp <- c(NA, round(change_relimp@pmvd, 3) * 100)

change_tab$names <- c("Intercept", "Target GPi", "MDS-UPDRS-III", "Schwab & England", 
                  "LEDD", "MMS", "Starkstein", "Marconi", 
                  "Hamilton", "Pain", "Insomnia", 
                  "Postural Stability") 

change_tab <- change_tab[order(change_tab$relimp, decreasing = TRUE, na.last = FALSE), 
                 c(5, 1, 2, 3, 4)]

tmp <- data.frame(coef = coef(change_mod))
tmp$lower <- confint(change_mod)[, 1]
tmp$upper <- confint(change_mod)[, 2]
tmp$stars <- starsPval(summary(change_mod)$coefficients[, "Pr(>|t|)"])
tmp$names <- c("intercept", "Target GPi", "MDS-UPDRS-III", "Schwab & England", 
               "LEDD", "MMS", "Starkstein", "Marconi", 
               "Hamilton", "Pain", "Insomnia", 
               "Postural Stability")

tmp <- tmp[tmp$names != "intercept", ]
tmp$relimp <- round(change_relimp@pmvd, 3) * 100
tmp <- tmp[order(tmp$relimp, decreasing = TRUE), ]
tmp$names <- ordered(tmp$names, levels = tmp$names[order(tmp$relimp, decreasing = TRUE)])
change_mod_plot <- tmp