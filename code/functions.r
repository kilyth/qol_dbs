#######################################################
### Deep Brain Stimulation: STN vs GPi.
### Functions
### 2022 Katrin Petermann
#######################################################

create_variables <- function(data){
  ## date variables
  data$date_dbs <- as.POSIXct(data$date_dbs, format = "%Y-%m-%d")
  data$year_birth <- ISOdate(dd$year_birth, 1, 1)
  data$age_surgery <- as.numeric((data$date_dbs - data$year_birth)/365)
  data$age_diag <- as.POSIXct(as.character(data$age_diag), format = "%Y")
  data$disease_duration <- as.numeric((data$date_dbs - data$age_diag)/365)
  data$age_diag <- as.numeric((data$age_diag - data$year_birth)/365)
  
  ## updrs --> mds-updrs
  data$mu3_mon <- updrs3_to_mdsupdrs3(data$upd3_score_mon_old, data$updrs3_score_mon)
  data$mu3_moff <- updrs3_to_mdsupdrs3(data$upd3_score_moff_old, data$updrs3_score_moff)
  
  ## updrs improvement
  data$mu3_preop_improvement <- 100 * (data$mu3_moff - data$mu3_mon)/data$mu3_moff
  
  ## Hamilton score 18 adjustment
  data$hamd_18_b <- apply(data[, c("hamd_18_mild", "hamd_18_severe")], 1, function(x){sum(as.numeric(x), na.rm = TRUE)})
  data$hamd_18_b[is.na(dd$hamd_18)] <- NA
  
  ## Sleep items from Hamilton
  data$insomnia <- data$hamd_4 + data$hamd_5 + data$hamd_6
  
  ## PDQ Summary Index and change in pdq39
  data$pdq39_si <- get_pdq39_si(data)
  
  
  return(data)
}

score_unfavorable <- function(x, n){
  points <- seq(100, 0, len = n)
  return(points[as.numeric(x)])
}

score_favorable <- function(x, n){
  points <- seq(0, 100, len = n)
  return(points[as.numeric(x)])
}

sf36_subscores <- function(x){
  res <- data.frame(physical_functioning <- mean(x[3:12]))
  res$role_functioning_physical <- mean(x[13:16])
  res$role_functioning_emotional <- mean(x[17:19])
  res$energy_fatigue <- mean(x[c(13, 17, 19, 31)])
  res$emotional_wellbeing <- mean(x[c(24:26, 28, 30)])
  res$social_functioning <- mean(x[c(20, 32)])
  res$pain <- mean(x[c(21, 22)])
  res$general_health <- mean(x[c(1, 33:36)])
  return(res)
}

score_sf36 <- function(dd, from, to){
  results <- data.frame(matrix(nrow = dim(dd)[1], ncol = 8))
  for(i in 1:dim(dd)[1]){
    score <- rep(NA, 26)
    dd_sf <- dd[i, from:to]
    ####################################################
    ## 6 answers from good = 1 to bad = 6
    ## items 21, 23, 26, 27, 30 RAND
    ## items 7, 9a, 9d, 9e, 9h in SF36
    #################################################### 
    
    score[c(21, 23, 26, 27, 30)] <- score_unfavorable(dd_sf[c(21, 23, 26, 27, 30)], n = 6)
    
    ####################################################
    ## 6 answers from bad = 1 to good = 6
    ## items 24, 25, 28, 29, 31 RAND
    ## items 9b, 9c, 9f, 9g, 9i in SF36
    #################################################### 
    
    score[c(24, 25, 28, 29, 31)] <- score_favorable(dd_sf[c(24, 25, 28, 29, 31)], n = 6)
    
    ####################################################
    ## 5 answers from good = 1 to bad = 5
    ## items 1, 2, 20, 22, 34, 36 RAND
    ## items 1, 2, 6, 8, 11b, 11d in SF36
    ####################################################
    
    score[c(1, 2, 20, 22, 34, 36)] <- score_unfavorable(dd_sf[c(1, 2, 20, 22, 34, 36)], n = 5)
    
    ####################################################
    ## 5 answers from bad = 1 to good = 5
    ## items 32, 33, 35 RAND
    ## items 10, 11a, 11c in SF36
    ####################################################
    
    score[c(32, 33, 35)] <- score_favorable(dd_sf[c(32, 33, 35)], n = 5)
    
    ####################################################
    ## 3 answers from bad = 1 to good = 3
    ## items 3 to 12 RAND
    ## items 3a to 3j in SF36
    ####################################################
    
    score[c(3:12)] <- score_favorable(dd_sf[c(3:12)], n = 3)
    
    ####################################################
    ## 2 answers from bad = 1 to good = 2
    ## items 13 to 19 RAND
    ## items 4a to 5c in SF36
    ####################################################
    
    score[c(13:19)] <- score_favorable(dd_sf[c(13:19)], n = 2)
    
    results[i, ] <- sf36_subscores(score)
  }
  
  return(results)
}

repeat_entries <- function(df){
  
  for(id in unique(df$record_id)){
    dd_id <- df[which(df$record_id == id), ]
    for(entry in c("sex", "stim_target")){
      dd_id[, entry] <- dd_id[1, entry]
    }
    df[which(df$record_id == id), ] <- dd_id
  }
  
  return(df)
}

get_change_scores <- function(df){
  variables <- c("record_id", "redcap_event_name", "pdq39_si")
  df_wide <- reshape(df[, variables], v.names = variables[-c(1, 2)], timevar = "redcap_event_name", idvar = "record_id", direction = "wide")
  colnames(df_wide) <- c("record_id", "pdq39_si_preop", "pdq39_si_postop")
  df_wide$pdq39_change_score <- df_wide$pdq39_si_preop - df_wide$pdq39_si_postop
  return(df_wide)
}

updrs3_to_mdsupdrs3 <- function(old, new){
  if(sum(!is.na(old + new))){
    cat("double entry for updrs and mds-updrs 3")
    return(NA)
  }
  old <- old + 3
  old[is.na(old)] <- 0
  new[is.na(new)] <- 0
  return(old + new)
}

get_pdq39_si <- function(dd){
  dd_pdq39 <- cbind(dd$pdq39_score_mob * 100 / (4 * 10),
                    dd$pdq39_score_actdai * 100 / (4 * 6),
                    dd$pdq39_score_emo * 100 / (4 * 6),
                    dd$pdq39_score_stig * 100 / (4 * 4),
                    dd$pdq39_score_sosup * 100 / (4 * 3),
                    dd$pdq39_score_cog * 100 / (4 * 4),
                    dd$pdq39_score_commu * 100 / (4 * 3),
                    dd$pdq39_score_bodis * 100 / (4 * 3))
  pdq39_si <- apply(dd_pdq39, 1, mean)
  return(pdq39_si)
}

impute_scale <- function(dd, score, first_var, last_var){
  dd <- dd[!is.na(dd[, score]), ]
  from <- which(colnames(dd) == first_var)
  to <- which(colnames(dd) == last_var)
  cat("missing variables: ", sum(is.na(dd[, from:to])), "/", prod(dim(dd[, from:to])))
  mf <- missForest(dd[, from:to])
  dd_imp <- mf$ximp
  dd_imp[, score] <- apply(dd_imp, 1, function(x){sum(as.numeric(as.character(x)))})
  dd_imp$record_id <- dd$record_id
  dd_imp$redcap_event_name <- dd$redcap_event_name
  return(dd_imp)
}

tableMixedModel <- function (model, stats = NULL, col.nam = NULL, row.nam = NULL, 
          intercept = NULL, text = "english", text.ci = text, 
          eps.pvalue = 1e-04, digits = NULL, big.mark = "'", 
          xtable = TRUE, align = NULL, caption = NULL, label = NULL, 
          vars = NULL, ...) 
{
  raw.col.nam.german <- c("Koeffizient", "Exp(Koeffizient)", 
                          "Standardfehler", "$t$-Wert", "95\\%-Konfidenzintervall", 
                          "$p$-Wert")
  raw.col.nam.english <- c("Coefficient", "Exp(Coefficient)", 
                           "Standarderror", "$t$-value", "95\\%-confidence interval", 
                           "$p$-value")
  raw.stats <- c("estimate", "exp.estimate", "standarderror", 
                 "t.value", "ci.95", "p.value")
  clm <- class(model)[1]
  if (clm %in% c("glm", "geeglm")) {
    cl <- model$family$family
  }
  else {
    cl <- clm
  }
  if (clm == "lmerModLmerTest") {
    if (is.null(intercept)) 
      intercept <- TRUE
    k <- c(1, 5, 6)
    if (is.null(stats)) 
      stats <- raw.stats[k]
    ind <- sapply(stats, function(x) which(x == raw.stats))
    if (is.null(col.nam)) {
      if (text == "german") 
        col.nam <- raw.col.nam.german[ind]
      if (text == "english") 
        col.nam <- raw.col.nam.english[ind]
    }
    if (is.null(row.nam)) {
      row.nam <- rownames(summary(mod_mixed)$coef)[-1]
      if (intercept) {
        if (text == "german") 
          intercept.nam <- "Achsenabschnitt"
        if (text == "english") 
          intercept.nam <- "Intercept"
        row.nam <- c(intercept.nam, row.nam)
      }
    }
    if (is.null(digits)) 
      digits <- rep(2, length(stats))
  }
  else {
  }
  if (intercept & clm %in% c("list", "coxph")) {
    warning("Weibull and Cox models do not include an intercept. Set intercept = FALSE")
  }
  if ("ci.95" %in% stats) {
    digits.ci <- digits[stats %in% "ci.95"]
  }
  else {
    digits.ci <- 2
  }
  if (clm == "lmerModLmerTest") {
    estimate <- summary(model)$coef[, 1]
    exp.estimate <- exp(estimate)
    standarderror <- summary(model)$coef[, 2]
    t.value <- summary(model)$coef[, 4]
    p.value <- summary(model)$coef[, 5]
    ci.95 <- formatCI(confint(model)[-c(1:2), ], digits = digits.ci, 
                      text = text.ci)
  }
  
  output <- data.frame(estimate, exp.estimate, standarderror, 
                       t.value, ci.95, p.value, stringsAsFactors = FALSE)
  
  if (nrow(output) > 1) {
    output.return <- output[, ind]
    colnames(output.return) <- col.nam
    rownames(output.return) <- row.nam
  }
  else {
    output.return <- data.frame(output[, ind])
    names(output.return) <- col.nam
    rownames(output.return) <- row.nam
  }
  for (i in 1:ncol(output.return)) {
    if (stats[i] == "p.value") {
      output.return[, i] <- biostatUZH::formatPval(as.numeric(as.character(output.return[, 
                                                                                         i])), break.eps = eps.pvalue)
    }
    else {
      if (stats[i] != "ci.95") {
        output.return[, i] <- sapply(output.return[, 
                                                   i], function(x) format(as.numeric(as.character(x)), 
                                                                          big.mark = big.mark, digits = digits[i], nsmall = digits[i], 
                                                                          scientific = FALSE))
        if (stats[i] == "exp.estimate" & nrow(output.return) > 
            1) {
          output.return[-1, i] <- sapply(output.return[-1, 
                                                       i], function(x) format(as.numeric(as.character(x)), 
                                                                              digits = digits[i], big.mark = big.mark, 
                                                                              nsmall = digits[i], scientific = FALSE))
        }
      }
    }
  }
  if (xtable && requireNamespace("xtable")) {
    if (is.null(align)) 
      align <- paste(rep("r", length(stats) + 1), 
                     collapse = "")
    xtab <- xtable::xtable(output.return, caption = caption, 
                           label = label, align = align)
    oopt <- options(xtable.include.rownames = TRUE, xtable.floating = TRUE, 
                    xtable.type = "latex", xtable.size = "footnotesize", 
                    xtable.table.placement = "!h", xtable.sanitize.colnames.function = identity)
    on.exit(options(oopt))
    print(xtab, ...)
  }
  else {
    output.return
  }
}

## bootstrap confidence intervals
confint_perc_change <- function(pre, post){
  set.seed(1234)
  n_boot <- 10000
  perc_change_means <- rep(NA, n_boot)
  for(b in 1:n_boot){
    b_pre <- sample(pre, length(pre), replace = TRUE)
    b_post <- sample(post, length(post), replace = TRUE)
    perc_change_means[b] <- mean_perc_change(b_pre, b_post)
  }
  return(quantile(perc_change_means, c(0.025, 0.975)))
}

mean_perc_change <- function(pre, post){
  change <- 100 * (mean(pre) - mean(post)) / mean(pre)
  return(change)
}


starsPval <- function(x){
  s <- unclass(symnum(x, corr = FALSE, na = FALSE, 
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", "+", " ")))
  return(s)
}