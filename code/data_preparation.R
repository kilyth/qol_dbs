#######################################################
### Deep Brain Stimulation: STN vs GPi.
### Data Preparation
### 2022 Katrin Petermann
#######################################################

## patients 23 and 78 have two postop assessments
dd_raw$redcap_repeat_instance <- as.numeric(dd_raw$redcap_repeat_instance)
dd_raw <- dd_raw[!c(dd_raw$redcap_repeat_instance %in% 2), ]
# table(dd_raw$redcap_repeat_instance)

## patients 74 and 100 have wrong implant date
dd_raw$date_dbs[which(dd_raw$record_id == 74)] <- c("2017-01-26", "")
dd_raw$date_dbs[which(dd_raw$record_id == 100)] <- c("2015-04-28", "")

## remove patient 13?
if(include_P13 == "no"){
  dd_raw <- dd_raw[dd_raw$record_id != 13, ]
}

#############################
## factor setup
#############################

## record id
record_id_index <- which(colnames(dd_raw) == "record_id")

## event
dd_raw$assessment <- factor(dd_raw$redcap_event_name, 
                            levels = c("postoperative_asse_arm_1",
                                       "preoperative_asses_arm_1"),
                            labels = c("postop", "preop"))
assessment_index <- which(colnames(dd_raw) == "assessment")

## PDQ-39: primary outcome!
pdq_from <- which(colnames(dd_raw) == "pdq39_1_leis")
pdq_to <- which(colnames(dd_raw) == "pdq39_39_hot")
dd_raw[, pdq_from:pdq_to] <- apply(dd_raw[, pdq_from:pdq_to], 2, as.numeric)
dd_pdq <- as.data.frame(lapply(dd_raw[, pdq_from:pdq_to], ordered, levels = 0:4))
dd_pdq$pdq39_missing <- apply(dd_pdq, 1, function(x){ifelse(sum(is.na(x)) == 39, 1, 0)})
n_missing_pdq <- sum(dd_pdq$pdq39_missing)

## age at surgery
dd_raw$date_dbs <- as.POSIXct(dd_raw$date_dbs, format = "%Y-%m-%d")
dd_raw$year_birth <- ISOdate(dd_raw$year_birth, 1, 1)
dd_raw$age_surgery <- as.numeric((dd_raw$date_dbs - dd_raw$year_birth)/365)

## time from surgery to postop assessment
for(p in unique(dd_raw$record_id)){
  dd_raw$med_date_preop[dd_raw$record_id == p] <- dd_raw$med_date_preop[dd_raw$record_id == p][1]
  dd_raw$med_date_postop[dd_raw$record_id == p] <- dd_raw$med_date_postop[dd_raw$record_id == p][2]
  dd_raw$date_dbs[dd_raw$record_id == p] <- dd_raw$date_dbs[dd_raw$record_id == p][1]
}
dd_raw$med_date_preop <- as.POSIXct(dd_raw$med_date_preop, format = "%Y-%m-%d")
dd_raw$med_date_postop <- as.POSIXct(dd_raw$med_date_postop, format = "%Y-%m-%d")
dd_raw$time_after_surgery <- as.numeric((dd_raw$med_date_postop - dd_raw$date_dbs)/7)

## sex
dd_raw$sex <- factor(dd_raw$sex, levels = c(1, 2), labels = c("male", "female"))

## disease duration
dd_raw$year_diag <- ISOdate(dd_raw$age_diag, 1, 1) # wrongly names age instead of year in redcap
dd_raw$disease_duration <- as.numeric((dd_raw$date_dbs - dd_raw$year_diag)/365)

## LEDD 
dd_raw$ledd <- apply(dd_raw[, c("ledd_preop", "ledd_postop")], 1, max, na.rm = TRUE)
dd_raw$ledd_ago <- apply(dd_raw[, c("led_ago_preop", "led_ago_postop")], 1, max, na.rm = TRUE)

## Schwab & England --> preop worse
seadl_rows <- which(colnames(dd_raw) %in% c("seadl_preop_best", "seadl_preop_best_old", 
                                            "seadl_preop_worse", "seadl_preop_worse_old",
                                            "seadl_postop_best", "seadl_postop_best_old", 
                                            "seadl_postop_worst", "seadl_postop_worse_old"))
dd_seadl <- apply(dd_raw[, seadl_rows], 2, function(x){10*as.numeric(x)})
dd_seadl[dd_seadl == 990] <- NA

seadl_best <- apply(dd_seadl[, c("seadl_preop_best", "seadl_preop_best_old",
                                 "seadl_postop_best", "seadl_postop_best_old")], 1, max, na.rm = TRUE)
seadl_worst <- apply(dd_seadl[, c("seadl_preop_worse", "seadl_preop_worse_old",
                                  "seadl_postop_worst", "seadl_postop_worse_old")], 1, max, na.rm = TRUE)


seadl_best[seadl_best == -Inf] <- NA
seadl_worst[seadl_worst == -Inf] <- NA

dd_raw$seadl_best <- seadl_best
dd_raw$seadl_worst <- seadl_worst

## Mini Mental State MMS
dd_raw$mms <- as.numeric(dd_raw$mms)

## Starkstein Apathy Score
sas_from <- which(colnames(dd_raw) == "sas_learn")
sas_to <- which(colnames(dd_raw) == "sas_apath")
dd_raw[, sas_from:sas_to] <- apply(dd_raw[, sas_from:sas_to], 2, as.numeric)
dd_sas <- as.data.frame(lapply(dd_raw[, sas_from:sas_to], ordered, levels = 0:3))

## Target
dd_raw$stim_target <- factor(dd_raw$stim_target, levels = c(1, 2), labels = c("STN", "GPi"))

## Stimulator Type
dd_raw$stimulator <- factor(dd_raw$stimulator, levels = c(1, 2), labels = c("Medtronic", "Boston"))

## Marconi
marconi_from <- which(colnames(dd_raw) == "marconi_neck")
marconi_to <- which(colnames(dd_raw) == "marconi_lel")
dd_raw[, marconi_from:marconi_to] <- apply(dd_raw[, marconi_from:marconi_to], 2, as.numeric)
dd_marconi <- as.data.frame(lapply(dd_raw[, marconi_from:marconi_to], ordered, levels = 0:4))

## Hamilton
hamd_from <- which(colnames(dd_raw) == "hamd_1")
hamd_to_17 <- which(colnames(dd_raw) == "hamd_17")
hamd_from_19 <- which(colnames(dd_raw) == "hamd_19")
hamd_to <- which(colnames(dd_raw) == "hamd_21") ## take care of adjustments for hamd 18

hamd_18b <- dd_raw$hamd_18
hamd_18a <- rep(NA, length(hamd_18b))
for(i in 1:length(hamd_18b)){
  if(hamd_18b[i] == 0){hamd_18a[i] <- 0}
  else{
    if(hamd_18b[i] == 1){hamd_18a[i] <- dd_raw$hamd_18_mild[i]}
    else{
      if(hamd_18b[i] == 2){hamd_18a[i] <- dd_raw$hamd_18_severe[i]}
      else{hamd_18a[i] <- NA}
    }
  }
}

dd_hamd <- cbind(dd_raw[, hamd_from:hamd_to_17], hamd_18a, hamd_18b, dd_raw[, hamd_from_19:hamd_to])
hamd_5_levels <- which(colnames(dd_hamd) %in% c("hamd_1", "hamd_2", "hamd_3", 
                                                "hamd_7", "hamd_8", "hamd_9", 
                                                "hamd_10", "hamd_11","hamd_15", 
                                                "hamd_19"))
hamd_4_levels <- which(colnames(dd_hamd) %in% c("hamd_16", "hamd_20"))
hamd_3_levels <- which(colnames(dd_hamd) %in% c("hamd_4", "hamd_5", "hamd_6",
                                                "hamd_12", "hamd_13", "hamd_14",
                                                "hamd_17", "hamd_18a", "hamd_18b",
                                                "hamd_21"))

dd_hamd[, hamd_5_levels] <- lapply(dd_hamd[, hamd_5_levels], 
                                   function(x){ordered(as.numeric(x), levels = 0:4)})
dd_hamd[, hamd_4_levels] <- lapply(dd_hamd[, hamd_4_levels], 
                                   function(x){ordered(as.numeric(x), levels = 0:3)})
dd_hamd[, hamd_3_levels] <- lapply(dd_hamd[, hamd_3_levels], 
                                   function(x){ordered(as.numeric(x), levels = 0:2)})
## VAS
dd_raw$vas_maxpain_week <- as.numeric(dd_raw$vas_maxpain_week)/10

## Hoehn and Yahr (old == new?)
hy_rows <- which(colnames(dd_raw) %in% c("hy_preop_mon_old", "hy_preop_mon", 
                                         "hy_preop_moff_old", "hy_preop_moff",
                                         "hy_postop_mon_old", "hy_postop_mon", 
                                         "hy_postop_moff_old", "hy_postop_moff"))
dd_hy <- apply(dd_raw[, hy_rows], 2, as.numeric)
dd_hy[dd_hy == 99] <- NA

hy_off <- apply(dd_hy[, c("hy_preop_moff_old", "hy_preop_moff",
                          "hy_postop_moff_old", "hy_postop_moff")], 1, max, na.rm = TRUE)
hy_on <- apply(dd_hy[, c("hy_preop_mon_old", "hy_preop_mon",
                         "hy_postop_mon_old", "hy_postop_mon")], 1, max, na.rm = TRUE)


hy_off[hy_off == -Inf] <- NA
hy_on[hy_on == -Inf] <- NA

hy_off <- ordered(round(hy_off), levels = 0:5)
hy_on <- ordered(round(hy_on), levels = 0:5)

dd_raw$hy_off <- hy_off
dd_raw$hy_on <- hy_on

#############################################
## create nice tables (with missing values)
#############################################
ind <- which(colnames(dd_raw) %in% c("upd3_speech_moff", "upd3_speech_moff_son",
                                     "updrs3_speech_moff", "updrs3_speech_moff_son"))
dd_raw[, ind] <- apply(dd_raw[, ind], 2, as.numeric)

dd_raw_preop <- dd_raw[dd_raw$assessment == "preop", ]
dd_raw_postop <- dd_raw[dd_raw$assessment == "postop", ]

## MDS-UPDRS-III index
dd_raw_preop$mu3_version <- factor(NA, levels = c("new", "old"))
dd_raw_postop$mu3_version <- factor(NA, levels = c("new", "old"))

for(i in 1:dim(dd_raw_preop)[1]){
  if(!is.na(dd_raw_preop$updrs3_speech_moff[i])){
    dd_raw_preop$mu3_version[i] <- "new"
  }
  else if(!is.na(dd_raw_preop$upd3_speech_moff[i])){
    dd_raw_preop$mu3_version[i] <- "old"
  }
  else{
    dd_raw_preop$mu3_version[i] <- "new"
  }
}

for(i in 1:dim(dd_raw_postop)[1]){
  if(!is.na(dd_raw_postop$updrs3_speech_moff_son[i])){
    dd_raw_postop$mu3_version[i] <- "new"
  }
  else if(!is.na(dd_raw_postop$upd3_speech_moff_son[i])){
    dd_raw_postop$mu3_version[i] <- "old"
  }
  else{
    dd_raw_postop$mu3_version[i] <- "new"
  }
}

dd_bl <- with(dd_raw_preop, cbind(record_id,
                                  sex,
                                  age_surgery,
                                  disease_duration,
                                  time_after_surgery,
                                  stim_target,
                                  stimulator))
dd_bl <- as.data.frame(dd_bl)


dd_missing <- with(dd_raw, cbind(record_id, 
                                 assessment, 
                                 dd_pdq,
                                 ledd,
                                 ledd_ago,
                                 seadl_worst,
                                 mms,
                                 vas_maxpain_week,
                                 dd_sas,
                                 dd_marconi,
                                 dd_hamd))

dd_missing <- merge(dd_missing, dd_bl, by = "record_id", all.x = TRUE)

## Imputation
cat("data imputation for all but motor scales \n")
set.seed(1234)
dd <- missForest(dd_missing)
dd <- dd$ximp

## correct for continuous variables
dd$seadl_worst <- round(dd$seadl_worst)
dd$mms <- round(dd$mms)
dd$vas_maxpain_week <- round(dd$vas_maxpain_week, 1)

###########################
## calculate total scores
###########################

############
## PDQ-39
############
pdq_from <- which(colnames(dd) == "pdq39_1_leis")
pdq_to <- which(colnames(dd) == "pdq39_39_hot")
dd[, pdq_from:pdq_to] <- apply(dd[, pdq_from:pdq_to], 2, function(x){as.numeric(as.character(x))})
dd_pdq <- dd[, pdq_from:pdq_to]
## calculate subscores
dd$pdq39_score_mob <- apply(dd_pdq[, 1:10], 1, function(x){sum(x) * 100 / (4 * 10)})
dd$pdq39_score_actdai <- apply(dd_pdq[, 11:16], 1, function(x){sum(x) * 100 / (4 * 6)})
dd$pdq39_score_emo <- apply(dd_pdq[, 17:22], 1, function(x){sum(x) * 100 / (4 * 6)})
dd$pdq39_score_stig <- apply(dd_pdq[, 23:26], 1, function(x){sum(x) * 100 / (4 * 4)})
dd$pdq39_score_sosup <- apply(dd_pdq[, 27:29], 1, function(x){sum(x) * 100 / (4 * 3)})
dd$pdq39_score_cog <- apply(dd_pdq[, 30:33], 1, function(x){sum(x) * 100 / (4 * 4)})
dd$pdq39_score_commu <- apply(dd_pdq[, 34:36], 1, function(x){sum(x) * 100 / (4 * 3)})
dd$pdq39_score_bodis <- apply(dd_pdq[, 37:39], 1, function(x){sum(x) * 100 / (4 * 3)})

## calculate summary score
from <- which(colnames(dd) == "pdq39_score_mob")
to <- which(colnames(dd) == "pdq39_score_bodis")

dd$pdq39_si <- apply(dd[, from:to], 1, function(x){round(mean(x))})
dd$pdq39_si <- ifelse(dd$pdq39_missing, NA, dd$pdq39_si)
cat(dd$record_id[dd$pdq39_missing], "\n")
cat(dd$record_id[is.na(dd$pdq39_si)], "\n")

## Starkstein
from <- which(colnames(dd) == "sas_learn")
to <- which(colnames(dd) == "sas_apath")
dd$sas_total <- apply(dd[, from:to], 1,
                      function(x){sum(as.numeric(as.character(x)))})

## Marconi
from <- which(colnames(dd) == "marconi_neck")
to <- which(colnames(dd) == "marconi_lel")
dd$marconi_total <- apply(dd[, from:to], 1,
                          function(x){sum(as.numeric(as.character(x)))})

## Hamilton
from <- which(colnames(dd) == "hamd_1")
to <- which(colnames(dd) == "hamd_21")
dd$hamd_total <- apply(dd[, from:to], 1,
                       function(x){sum(as.numeric(as.character(x)))})

## insomnia
from <- which(colnames(dd) == "hamd_4")
to <- which(colnames(dd) == "hamd_6")
dd$insomnia <-apply(dd[, from:to], 1,
                    function(x){sum(as.numeric(as.character(x)))})

#####################
## add UPDRS III
#####################

upd3_colnames <- c("record_id", "assessment", "hy",
                   "upd3_speech", "upd3_facial", "upd3_retremlip",
                   "upd3_retremrue", "upd3_retremlue", "upd3_retremrle",
                   "upd3_retremlle", "upd3_postremr", "upd3_postreml",
                   "upd3_rigidneck", "upd3_rigidrue", "upd3_rigidlue",
                   "upd3_rigidrle", "upd3_rigidlle", "upd3_tapright",
                   "upd3_tapleft", "upd3_movright", "upd3_movleft",
                   "upd3_prosupr", "upd3_prosupl", "upd3_agilr", 
                   "upd3_agill", "upd3_arising", "upd3_postur", 
                   "upd3_gait", "upd3_stabil", "upd3_spontan", 
                   "medication")

mu3_colnames <- c("record_id", "assessment", "hy",
                  "updrs3_speech", "updrs3_facial", "updrs3_rigidneck",
                  "updrs3_rigidrue", "updrs3_rigidlue", "updrs3_rigidrle",
                  "updrs3_rigidlle", "updrs3_tapright", "updrs3_tapleft",
                  "updrs3_movright", "updrs3_movleft", "updrs3_prosupr",
                  "updrs3_prosupl", "updrs3_toetapr", "updrs3_toetapl",
                  "updrs3_agilr", "updrs3_agill", "updrs3_arising", 
                  "updrs3_gait", "updrs3_freez", "updrs3_stabil", 
                  "updrs3_postur", "updrs3_spontan", "updrs3_postremr",
                  "updrs3_postreml", "updrs3_kitremr", "updrs3_kitreml",
                  "updrs3_retremrue", "updrs3_retremlue", "updrs3_retremrle",
                  "updrs3_retremlle", "updrs3_retremlip", "updrs3_constancy",
                  "medication")

make_dd_updrs3 <- function(data, from_colname, to_colname, med, version){
  from <- which(colnames(data) == from_colname)
  to <- which(colnames(data) == to_colname)
  ifelse(med == "off", 
         hy_index <- which(colnames(data) == "hy_off"),
         hy_index <- which(colnames(data) == "hy_on"))
  dd_upd3 <- data[, c(record_id_index,
                      assessment_index,
                      hy_index,
                      from:to)]
  dd_upd3$medication <- factor(med, levels = c("off", "on"))
  if(version == "old"){
    colnames(dd_upd3) <- upd3_colnames
  }
  else if(version == "new"){
    colnames(dd_upd3) <- mu3_colnames
  }
  else{
    print("version incorrect")
    return(0)
  }
  
  return(dd_upd3)
}

## UPDRS 3 preop med-off
u3_medoff_preop <- make_dd_updrs3(dd_raw_preop[dd_raw_preop$mu3_version == "old", ], 
                                  "upd3_speech_moff", 
                                  "upd3_spontan_moff", 
                                  "off", "old")

## UPDRS 3 postop med-off
u3_medoff_postop <- make_dd_updrs3(dd_raw_postop[dd_raw_postop$mu3_version == "old", ], 
                                   "upd3_speech_moff_son", 
                                   "upd3_spontan_moff_son", 
                                   "off", "old")

## UPDRS 3 preop med-on
u3_medon_preop <- make_dd_updrs3(dd_raw_preop[dd_raw_preop$mu3_version == "old", ], 
                                 "upd3_speech_mon", 
                                 "upd3_spontan_mon", 
                                 "on", "old")

## UPDRS 3 postop med-off
u3_medon_postop <- make_dd_updrs3(dd_raw_postop[dd_raw_postop$mu3_version == "old", ], 
                                  "upd3_speech_mon_son", 
                                  "upd3_spontan_mon_son", 
                                  "on", "old")

## merge all 4 tables preop / postop and med-off / med-on
dd_upd3 <- rbind(u3_medoff_preop, u3_medon_preop,
                 u3_medoff_postop, u3_medon_postop)

rm(u3_medoff_preop, u3_medon_preop,
   u3_medoff_postop, u3_medon_postop)

from <- which(colnames(dd_upd3) == "upd3_speech")
to <- which(colnames(dd_upd3) == "upd3_spontan")
dd_upd3[, from:to] <- lapply(dd_upd3[, from:to], 
                             function(x){ordered(as.numeric(x), levels = 0:4)})

dd_upd3_missing_long <- merge(dd_upd3, dd, by = c("record_id", "assessment"))

cat("data imputation for UPDRS 3 \n")
set.seed(1234)
dd_upd3_long <- missForest(dd_upd3_missing_long)
dd_upd3_long <- dd_upd3_long$ximp
dd_upd3_long$upd3_total <- apply(dd_upd3_long[, from:to], 1,
                                 function(x){sum(as.numeric(x))})


## convert updrs3 to mds-updrs-3 according to Geotz et al. 2012
dd_upd3_long$mu3_total <- NA
for(i in 1:dim(dd_upd3_long)[1]){
  if(dd_upd3_long$hy[i] %in% c(0, 1, 2)){
    dd_upd3_long$mu3_total[i] <- 2.3 + 1.2 * dd_upd3_long$upd3_total[i]
  }
  else if(dd_upd3_long$hy[i] == 3){
    dd_upd3_long$mu3_total[i] <- 1.0 + 1.2 * dd_upd3_long$upd3_total[i]
  }
  else if(dd_upd3_long$hy[i] %in% c(4, 5)){
    dd_upd3_long$mu3_total[i] <- 7.5 + 1.1 * dd_upd3_long$upd3_total[i]
  }
  else {print("error")}
}

dd_upd3_long$mu3_total <- round(dd_upd3_long$mu3_total)
dd_upd3_long$stability <- dd_upd3_long$upd3_stabil

#####################
## add MDS-UPDRS III
#####################

## MDS-UPDRS 3 preop med-off
mu3_medoff_preop <- make_dd_updrs3(dd_raw_preop[dd_raw_preop$mu3_version == "new", ], 
                                   "updrs3_speech_moff", 
                                   "updrs3_constancy_moff",
                                   "off", "new")

## MDS-UPDRS 3 postop med-off
mu3_medoff_postop <- make_dd_updrs3(dd_raw_postop[dd_raw_postop$mu3_version == "new", ], 
                                    "updrs3_speech_moff_son", 
                                    "updrs3_constancy_moff_son",
                                    "off", "new")

## MDS-UPDRS 3 preop med-on
mu3_medon_preop <- make_dd_updrs3(dd_raw_preop[dd_raw_preop$mu3_version == "new", ], 
                                  "updrs3_speech_mon", 
                                  "updrs3_constancy_mon",
                                  "on", "new")

## MDS-UPDRS 3 postop med-on
mu3_medon_postop <- make_dd_updrs3(dd_raw_postop[dd_raw_postop$mu3_version == "new", ], 
                                   "updrs3_speech_mon_son", 
                                   "updrs3_constancy_mon_son",
                                   "on", "new")

dd_mu3 <- rbind(mu3_medoff_preop, mu3_medon_preop,
                mu3_medoff_postop, mu3_medon_postop)

rm(mu3_medoff_preop, mu3_medon_preop,
   mu3_medoff_postop, mu3_medon_postop)

from <- which(colnames(dd_mu3) == "updrs3_speech")
to <- which(colnames(dd_mu3) == "updrs3_constancy")
dd_mu3[, from:to] <- lapply(dd_mu3[, from:to], 
                            function(x){ordered(as.numeric(x), levels = 0:4)})

dd_mu3_missing_long <- merge(dd_mu3, dd, by = c("record_id", "assessment"))

cat("data imputation for MDS-UPDRS 3 \n")
set.seed(1234)
dd_mu3_long <- missForest(dd_mu3_missing_long)
dd_mu3_long <- dd_mu3_long$ximp
dd_mu3_long$mu3_total <- apply(dd_mu3_long[, from:to], 1,
                               function(x){sum(as.numeric(x))})
dd_mu3_long$stability <- dd_mu3_long$updrs3_stabil


## create common table with all MDS-UPDRS-III values
dd_all_long <- expand.grid(dd_bl$record_id, c("preop", "postop"), c("off", "on"))
colnames(dd_all_long) <- c("record_id", "assessment", "medication")

cnames <- c("record_id", "assessment", "medication", "sex",
            "age_surgery", "disease_duration", "time_after_surgery",
            "stim_target", "stimulator", "ledd", "ledd_ago", "seadl_worst", 
            "mms", "vas_maxpain_week", "sas_total",
            "marconi_total", "hamd_total", "insomnia",
            "hy", "mu3_total", "stability", "pdq39_score_mob", 
            "pdq39_score_actdai", "pdq39_score_emo", "pdq39_score_stig", 
            "pdq39_score_sosup", "pdq39_score_cog", "pdq39_score_commu", 
            "pdq39_score_bodis", "pdq39_si", "pdq39_missing")

dd_all_long <- merge(dd_all_long, rbind(dd_mu3_long[, cnames],
                                        dd_upd3_long[, cnames]),
                     by = c("record_id", "assessment", "medication"),
                     all.x = TRUE)

dd_all_long <- merge(dd_all_long, 
                     dd_raw[dd_raw$assessment == "preop", c("record_id", "date_dbs")], 
                     by = "record_id", all.x = TRUE)


## remove patients with missing pdq
ids_to_remove <- NULL
if(remove_missing_pdq == "yes"){
  for(i in unique(dd_all_long$record_id)){
    m <- sum(dd_all_long$pdq39_missing[dd_all_long$record_id == i])
    if(m > 0){
      ids_to_remove <- cbind(ids_to_remove, i)
    }
  }
}
print(ids_to_remove)

dd_all_long <- dd_all_long[dd_all_long$record_id %nin% ids_to_remove, ]

## calculate missing scores
dd_all_long$stim_target <- factor(dd_all_long$stim_target, levels = c(1, 2), labels = c("STN", "GPi"))
dd_all_long$stimulator <- factor(dd_all_long$stimulator, levels = c(1, 2), labels = c("Medtronic", "Boston"))
dd_all_long$sex <- factor(dd_all_long$sex, levels = c(1, 2), labels = c("male", "female"))
dd_all_long$stability <- as.numeric(as.character(dd_all_long$stability))

dd_all_preop <- reshape(dd_all_long[dd_all_long$assessment == "preop",],
                        direction = "wide", v.names = c("hy", "mu3_total", "stability"), 
                        timevar = c("medication"), idvar = "record_id", sep = "_")

dd_all_postop <- reshape(dd_all_long[dd_all_long$assessment == "postop",],
                         direction = "wide", v.names = c("hy", "mu3_total", "stability"), 
                         timevar = c("medication"), idvar = "record_id", sep = "_")

dd_all <- rbind(dd_all_preop, dd_all_postop)

## calculate missing scores
dd_all$mu3_improvement <- with(dd_all, round(100 * (mu3_total_off - mu3_total_on)/mu3_total_off))

## create wide data frame
dd_all_wide <- reshape(dd_all,
                       direction = "wide", v.names = c("ledd", "ledd_ago", "seadl_worst", "mms", 
                                                       "vas_maxpain_week", "sas_total", 
                                                       "marconi_total", "hamd_total", "insomnia",
                                                       "hy_off", "hy_on", "mu3_total_off", 
                                                       "mu3_total_on", "stability_off", "stability_on",
                                                       "mu3_improvement", "pdq39_score_mob", 
                                                       "pdq39_score_actdai", "pdq39_score_emo", "pdq39_score_stig", 
                                                       "pdq39_score_sosup", "pdq39_score_cog", "pdq39_score_commu", 
                                                       "pdq39_score_bodis",  "pdq39_si"), 
                       timevar = c("assessment"), idvar = "record_id", sep = "_")

perc_change <- function(pre, post){
  pc <- 100 * (pre - post) / (pre + 1)
  return(pc)
}

## calculate missing scores
dd_all_wide$pdq39_improvement_abs <- with(dd_all_wide, pdq39_si_preop - pdq39_si_postop)
dd_all_wide$pdq39_improvement_perc <- with(dd_all_wide, round(100 * (pdq39_improvement_abs)/pdq39_si_preop))
dd_all_wide$pdq39_change_mob <- with(dd_all_wide, pdq39_score_mob_preop - pdq39_score_mob_postop)
dd_all_wide$pdq39_change_actdai <- with(dd_all_wide, pdq39_score_actdai_preop - pdq39_score_actdai_postop)
dd_all_wide$pdq39_change_emo <- with(dd_all_wide, pdq39_score_emo_preop - pdq39_score_emo_postop)
dd_all_wide$pdq39_change_stig <- with(dd_all_wide, pdq39_score_stig_preop - pdq39_score_stig_postop)
dd_all_wide$pdq39_change_sosup <- with(dd_all_wide, pdq39_score_sosup_preop - pdq39_score_sosup_postop)
dd_all_wide$pdq39_change_cog <- with(dd_all_wide, pdq39_score_cog_preop - pdq39_score_cog_postop)
dd_all_wide$pdq39_change_commu <- with(dd_all_wide, pdq39_score_commu_preop - pdq39_score_commu_postop)
dd_all_wide$pdq39_change_bodis <- with(dd_all_wide, pdq39_score_bodis_preop - pdq39_score_bodis_postop)

dd_all_wide$pdq39_perc_change_mob <- with(dd_all_wide, perc_change(pdq39_score_mob_preop, pdq39_score_mob_postop))
dd_all_wide$pdq39_perc_change_actdai <- with(dd_all_wide, perc_change(pdq39_score_actdai_preop, pdq39_score_actdai_postop))
dd_all_wide$pdq39_perc_change_emo <- with(dd_all_wide, perc_change(pdq39_score_emo_preop, pdq39_score_emo_postop))
dd_all_wide$pdq39_perc_change_stig <- with(dd_all_wide, perc_change(pdq39_score_stig_preop, pdq39_score_stig_postop))
dd_all_wide$pdq39_perc_change_sosup <- with(dd_all_wide, perc_change(pdq39_score_sosup_preop, pdq39_score_sosup_postop))
dd_all_wide$pdq39_perc_change_cog <- with(dd_all_wide, perc_change(pdq39_score_cog_preop, pdq39_score_cog_postop))
dd_all_wide$pdq39_perc_change_commu <- with(dd_all_wide, perc_change(pdq39_score_commu_preop, pdq39_score_commu_postop))
dd_all_wide$pdq39_perc_change_bodis <- with(dd_all_wide, perc_change(pdq39_score_bodis_preop, pdq39_score_bodis_postop))


## define change variables
dd_all_wide$ledd_change <- dd_all_wide$ledd_preop - dd_all_wide$ledd_postop
dd_all_wide$ledd_ago_change <- dd_all_wide$ledd_ago_preop - dd_all_wide$ledd_ago_postop
dd_all_wide$seadl_change <- dd_all_wide$seadl_worst_postop - dd_all_wide$seadl_worst_preop
dd_all_wide$mms_change <- dd_all_wide$mms_postop - dd_all_wide$mms_preop
dd_all_wide$vas_maxpain_change <- dd_all_wide$vas_maxpain_week_preop - dd_all_wide$vas_maxpain_week_postop
dd_all_wide$sas_change <- dd_all_wide$sas_total_preop - dd_all_wide$sas_total_postop
dd_all_wide$marconi_change <- dd_all_wide$marconi_total_preop - dd_all_wide$marconi_total_postop
dd_all_wide$hamd_change <- dd_all_wide$hamd_total_preop - dd_all_wide$hamd_total_postop
dd_all_wide$insomnia_change <- dd_all_wide$insomnia_preop - dd_all_wide$insomnia_postop
dd_all_wide$mu3_off_change <- dd_all_wide$mu3_total_off_preop - dd_all_wide$mu3_total_off_postop
dd_all_wide$mu3_improvement_change <- dd_all_wide$mu3_improvement_preop - dd_all_wide$mu3_improvement_postop
dd_all_wide$stability_on_change <- dd_all_wide$stability_on_preop - dd_all_wide$stability_on_postop

dd_all_wide$ledd_perc_change <- with(dd_all_wide, perc_change(ledd_preop, ledd_postop))
dd_all_wide$ledd_ago_perc_change <- with(dd_all_wide, perc_change(ledd_ago_preop, ledd_ago_postop))
dd_all_wide$seadl_perc_change <- with(dd_all_wide, perc_change(seadl_worst_postop, seadl_worst_preop))
dd_all_wide$mms_perc_change <- with(dd_all_wide, perc_change(mms_postop, mms_preop))
dd_all_wide$vas_maxpain_perc_change <- with(dd_all_wide, perc_change(vas_maxpain_week_preop, vas_maxpain_week_postop))
dd_all_wide$sas_perc_change <- with(dd_all_wide, perc_change(sas_total_preop, sas_total_postop))
dd_all_wide$marconi_perc_change <- with(dd_all_wide, perc_change(marconi_total_preop, marconi_total_postop))
dd_all_wide$hamd_perc_change <- with(dd_all_wide, perc_change(hamd_total_preop, hamd_total_postop))
dd_all_wide$insomnia_perc_change <- with(dd_all_wide, perc_change(insomnia_preop, insomnia_postop))
dd_all_wide$mu3_off_perc_change <- with(dd_all_wide, perc_change(mu3_total_off_preop, mu3_total_off_postop))
dd_all_wide$mu3_improvement_perc_change <- with(dd_all_wide, perc_change(mu3_improvement_preop, mu3_improvement_postop))
dd_all_wide$stability_on_perc_change <- with(dd_all_wide, perc_change(stability_on_preop, stability_on_postop))

############################
## Stimulation parameters ##
############################

from <- which(colnames(dd_raw) == "lead_dir")
to <- which(colnames(dd_raw) == "stimulation_parameters_complete")
dd_stim <- dd_raw[dd_raw$redcap_event_name == "postoperative_asse_arm_1", c(1, from:to)]

################################
## data preparation for figures
################################

pdq39_perc_change <- function(x){
  vec <- with(x,c(mean_perc_change(pdq39_si_preop, pdq39_si_postop),
                  mean_perc_change(pdq39_score_mob_preop, pdq39_score_mob_postop),
                  mean_perc_change(pdq39_score_actdai_preop, pdq39_score_actdai_postop),
                  mean_perc_change(pdq39_score_emo_preop, pdq39_score_emo_postop),
                  mean_perc_change(pdq39_score_stig_preop, pdq39_score_stig_postop),
                  mean_perc_change(pdq39_score_bodis_preop, pdq39_score_bodis_postop),
                  mean_perc_change(pdq39_score_sosup_preop, pdq39_score_sosup_postop),
                  mean_perc_change(pdq39_score_cog_preop, pdq39_score_cog_postop),
                  mean_perc_change(pdq39_score_commu_preop, pdq39_score_commu_postop)))
  return(vec)
}

pdq39_perc_change_ci <- function(x){
  vec <- with(x, rbind(confint_perc_change(pdq39_si_preop, pdq39_si_postop),
                       confint_perc_change(pdq39_score_mob_preop, pdq39_score_mob_postop),
                       confint_perc_change(pdq39_score_actdai_preop, pdq39_score_actdai_postop),
                       confint_perc_change(pdq39_score_emo_preop, pdq39_score_emo_postop),
                       confint_perc_change(pdq39_score_stig_preop, pdq39_score_stig_postop),
                       confint_perc_change(pdq39_score_bodis_preop, pdq39_score_bodis_postop),
                       confint_perc_change(pdq39_score_sosup_preop, pdq39_score_sosup_postop),
                       confint_perc_change(pdq39_score_cog_preop, pdq39_score_cog_postop),
                       confint_perc_change(pdq39_score_commu_preop, pdq39_score_commu_postop)))
  return(vec)
}

pdq39_change_score <- c(pdq39_perc_change(subset(dd_all_wide, stim_target == "STN")),
                        pdq39_perc_change(subset(dd_all_wide, stim_target == "GPi")),
                        pdq39_perc_change(dd_all_wide))

pdq39_change_ci <- rbind(pdq39_perc_change_ci(subset(dd_all_wide, stim_target == "STN")),
                         pdq39_perc_change_ci(subset(dd_all_wide, stim_target == "GPi")),
                         pdq39_perc_change_ci(dd_all_wide))


dd_pdq39_change <- data.frame(target = rep(c("STN", "GPi", "all"), each = 9),
                              subscore = rep(c("Summary Index", "Mobility", "Activities of Daily Living",
                                               "Emotional Well-Being", "Stigma", "Bodily Discomfort",
                                               "Social Support", "Cognition", "Communication"), 3),
                              score = pdq39_change_score,
                              lower = pdq39_change_ci[, 1],
                              upper = pdq39_change_ci[, 2])

dd_pdq39_change$subscore <- ordered(dd_pdq39_change$subscore, 
                                    levels = c("Summary Index", "Mobility", "Activities of Daily Living",
                                               "Emotional Well-Being", "Stigma", "Bodily Discomfort",
                                               "Social Support", "Cognition", "Communication"))
dd_pdq39_change$target <- factor(dd_pdq39_change$target, levels = c("STN", "GPi", "all"), labels = c("STN", "GPi", "all"))

###################################################

allvars_perc_change <- function(x){
  vec <- with(x,c(mean_perc_change(pdq39_si_preop, pdq39_si_postop),
                  mean_perc_change(ledd_preop, ledd_postop),
                  -mean_perc_change(seadl_worst_preop, seadl_worst_postop),
                  -mean_perc_change(mms_preop, mms_postop),
                  mean_perc_change(vas_maxpain_week_preop, vas_maxpain_week_postop),
                  mean_perc_change(sas_total_preop, sas_total_postop),
                  mean_perc_change(marconi_total_preop, marconi_total_postop),
                  mean_perc_change(hamd_total_preop, hamd_total_postop),
                  mean_perc_change(insomnia_preop, insomnia_postop),
                  mean_perc_change(stability_on_preop, stability_on_postop),
                  mean_perc_change(mu3_total_off_preop, mu3_total_off_postop)))
  return(vec)
}

allvars_perc_change_ci <- function(x){
  vec <- with(x, rbind(confint_perc_change(pdq39_si_preop, pdq39_si_postop),
                       confint_perc_change(ledd_preop, ledd_postop),
                       -confint_perc_change(seadl_worst_preop, seadl_worst_postop),
                       -confint_perc_change(mms_preop, mms_postop),
                       confint_perc_change(vas_maxpain_week_preop, vas_maxpain_week_postop),
                       confint_perc_change(sas_total_preop, sas_total_postop),
                       confint_perc_change(marconi_total_preop, marconi_total_postop),
                       confint_perc_change(hamd_total_preop, hamd_total_postop),
                       confint_perc_change(insomnia_preop, insomnia_postop),
                       confint_perc_change(stability_on_preop, stability_on_postop),
                       confint_perc_change(mu3_total_off_preop, mu3_total_off_postop)))
  return(vec)
}

allvars_change_score <- c(allvars_perc_change(subset(dd_all_wide, stim_target == "STN")),
                          allvars_perc_change(subset(dd_all_wide, stim_target == "GPi")),
                          allvars_perc_change(dd_all_wide))

allvars_change_ci <- rbind(allvars_perc_change_ci(subset(dd_all_wide, stim_target == "STN")),
                           allvars_perc_change_ci(subset(dd_all_wide, stim_target == "GPi")),
                           allvars_perc_change_ci(dd_all_wide))



dd_allvars_change <- data.frame(target = rep(c("STN", "GPi", "all"), each = 11),
                                subscore = rep(c("PDQ-39 SI", "LEDD", "Schwab & England",
                                                 "MMS", "Pain", "Starkstein",
                                                 "Marconi", "Hamilton", "Insomnia", 
                                                 "Postural Stability", "MDS-UPDRS-III \n (off-medication)"), 3),
                                score = allvars_change_score,
                                lower = allvars_change_ci[, 1],
                                upper = allvars_change_ci[, 2])

dd_allvars_change$subscore <- ordered(dd_allvars_change$subscore,
                                      levels = c("LEDD", "Marconi",
                                                 "MDS-UPDRS-III \n (off-medication)", "Insomnia", 
                                                 "Postural Stability",
                                                 "Hamilton", "PDQ-39 SI", "Pain",
                                                 "Schwab & England", "MMS", "Starkstein"))
dd_allvars_change$target <- factor(dd_allvars_change$target, levels = c("STN", "GPi", "all"), labels = c("STN", "GPi", "all"))

################################################################


dd_allpat_allvars_change <- data.frame(
  target = rep(dd_all_wide$stim_target, 11),
  scale = rep(c("LEDD", "Marconi",
                "MDS-UPDRS-III \n (off-medication)", "Insomnia", 
                "Postural Stability",
                "Hamilton", "PDQ-39 SI", "Pain",
                "Schwab & England", "MMS", "Starkstein"), each = dim(dd_all_wide)[1]),
  perc_change = c(dd_all_wide$ledd_perc_change,
                  dd_all_wide$marconi_perc_change,
                  dd_all_wide$mu3_off_perc_change,
                  dd_all_wide$insomnia_perc_change,
                  dd_all_wide$stability_on_perc_change,
                  dd_all_wide$hamd_perc_change,
                  dd_all_wide$pdq39_improvement_perc,
                  dd_all_wide$vas_maxpain_perc_change,
                  dd_all_wide$seadl_perc_change,
                  dd_all_wide$mms_perc_change,
                  dd_all_wide$sas_perc_change)
)

dd_allpat_allvars_change$scale <- ordered(
  dd_allpat_allvars_change$scale,
  levels = c("LEDD", "Marconi",
             "MDS-UPDRS-III \n (off-medication)", "Insomnia", 
             "Postural Stability",
             "Hamilton", "PDQ-39 SI", "Pain",
             "Schwab & England", "MMS", "Starkstein"))

####################################
## matched data
####################################

dd_match <- dd_all_wide
dd_match$sex_bin <- ifelse(dd_match$sex == "male", 0, 1)
dd_match$stim_target_bin <- ifelse(dd_match$stim_target == "GPi", 0, 1)
matches <- Match(Y = dd_match$pdq39_improvement_abs,
                 Tr = dd_match$stim_target_bin, 
                 X = dd_match[, c("sex_bin", "age_surgery", "disease_duration", "pdq39_si_preop",
                                  "mu3_improvement_preop", "mms_preop", "marconi_total_preop",
                                  "stability_on_preop")],
                 estimand = "ATC",
                 replace = FALSE)

dd_match <- dd_match[c(matches$index.treated, matches$index.control), ]
dd_match$match_index <- rep(1:matches$wnobs, 2)
dd_match$stim_target <- factor(dd_match$stim_target, labels = c("STN subgroup", "GPi"))

pdq_treated <- dd_all_wide$pdq39_improvement_abs[matches$index.treated]
pdq_control <- dd_all_wide$pdq39_improvement_abs[matches$index.control]

n <- 10^4
diff_pdq <- rep(NA, n)
for(i in 1:n){
  
  l <- length(pdq_control)
  shuffle <- sample(c(TRUE, FALSE), l, replace = TRUE)
  pt <- rep(NA, l)
  pc <- rep(NA, l)
  for(s in 1:l){
    pt[s] <- ifelse(shuffle[s], pdq_treated[s], pdq_control[s])
    pc[s] <- ifelse(shuffle[s], pdq_control[s], pdq_treated[s])
  }
  diff_pdq[i] <- mean(pt - pc)
}

p_matches <- (sum(diff_pdq > matches$est[1]) + 1)/n

################################################################

rm(list=ls()[! ls() %in% c("dd_all_preop", "dd_all_postop", 
                           "dd_all", "dd_all_wide", 
                           "dd_all_long", "dd_stim",
                           "dd_pdq39_change", "dd_allvars_change",
                           "dd_allpat_allvars_change", "dd_match",
                           "matches", "p_matches", "starsPval",
                           "include_P13", "remove_missing_pdq")])

save(dd_all, dd_all_long, dd_all_postop, 
     dd_all_preop, dd_all_wide, dd_stim,
     dd_pdq39_change, dd_allvars_change,
     dd_allpat_allvars_change, dd_match,
     matches, p_matches,
     include_P13, remove_missing_pdq, file = "./data/dd_clean_imputed.RData")
