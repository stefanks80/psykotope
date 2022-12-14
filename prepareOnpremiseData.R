perpare_onpremise_itemscores <- function(qop_data) {
  stud_response <- qop_data[["svar"]]
  stud_item_scores <- aggregate(kar ~ kandnr + spmid, stud_response, mean)
  names(stud_item_scores) <- c("kandnr", "spmid", "item_score")
  return(stud_item_scores)
 }

perpare_onpremise_scorelevels <- function(item_scores_qop, n_cat = 4) {

  stud_sumscore <- aggregate(item_score ~ kandnr, item_scores_qop, mean)
  names(stud_sumscore) <- c("kandnr", "sum_score")
  stud_sumscore$sum_score <- round(stud_sumscore$sum_score/6*100, 2)
  stud_sumscore$sum_score_rnd <- round(stud_sumscore$sum_score)

  stud_sumscore$score_level <- cut(
    stud_sumscore$sum_score,
    breaks = quantile(stud_sumscore$sum_score, seq(0, 1, 1 / n_cat)), 
    include.lowest = TRUE)

  stud_sumscore$score_level_label <- stud_sumscore$score_level 
  stud_sumscore$score_level <- as.factor(
    as.numeric(stud_sumscore$score_level))

  levels(stud_sumscore$score_level_label) <- gsub(
      "^(\\D{1})(\\d+\\.*\\d*)(,)(\\d+\\.*\\d*)(\\D+)", "\\2\\%-\\4\\%",
      levels(stud_sumscore$score_level_label))

  for_group_n <- data.frame(ftable(stud_sumscore$score_level_label))

  stud_sumscore$score_level_nlabel <- stud_sumscore$score_level_label

  levels(stud_sumscore$score_level_nlabel) <- paste0(
    levels(stud_sumscore$score_level_nlabel), "\n(N=", for_group_n$Freq, ")")

  return(stud_sumscore)
}

prepare_onpremise_mcmr <- function(qop_data) {
  mc_mr <- qop_data[["svar"]]
  sel_vars <- c("spmid", "altnr", "kandnr", "type")
  mc_mr <- mc_mr[mc_mr$type %in% c("MC", "MR"), sel_vars]
  mc_mr[, "alt_letters"] <- "#"
  sel_alt <- mc_mr[mc_mr$altnr >= 0, "altnr"] 
  mc_mr[mc_mr$altnr >= 0, "alt_letters"] <- letters[sel_alt + 1]
  return(mc_mr)
}

prepare_onpremise_mcmr_key <- function(qop_data) {
  for_key <- qop_data[["spm"]]
  for_key <- for_key[for_key$type %in% c("MC", "MR"), ]
  key_vars <- c("spmid", "altnr", "korrekt")
  for_key <- unique(for_key[, key_vars])
  for_key[, "alt_letters"] <- "#"
  sel_alt <- for_key[for_key$altnr >= 0, "altnr"] 
  for_key[for_key$altnr >= 0, "alt_letters"] <- letters[sel_alt + 1]
  for_key$korrekt <- as.logical(for_key$korrekt)
  out_vars <- c("spmid", "alt_letters", "korrekt")
  out <- for_key[, out_vars]
  return(out)
}

prepare_onpremise_ess <- function(qop_data) {
    ess_data <- qop_data[["sensur"]]
    ess_data$sens_diff <- ess_data$kar - ess_data$kar2
    ess_data$score1 <- ess_data$kar
    ess_data$score2 <- ess_data$kar2
    ess_data$kar <- NULL
    ess_data$kar2 <- NULL
    
    nstud <- unique(ess_data$studid)

    kand_data <- qop_data[["svar"]]
    kand_data <- kand_data[, c("kandnr", "studid")]
    kand_data <- unique(kand_data)
    nkand <- unique(kand_data$studid)

    if (sum(!nstud %in% nkand) == 0) {
        ess_data_out <- merge(ess_data, kand_data)
        ess_data_out$studid <- NULL
        if (nrow(ess_data_out) == nrow(ess_data))
          return(ess_data_out)
      } else {
        print("ERROR: Check candidate ids in 'svar' and 'sensur' data")
        return(ess_data)
      }
}