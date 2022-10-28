check_item_mismatch <- function(qop_data) {

  data_spm <- qop_data[["spm"]]
  data_spm <- data_spm[, c("spmid", "altnr", "korrekt", "opsjnr")]

  data_svar <- qop_data[["svar"]]
  spm_missing <- setdiff(data_svar$spmid, data_svar$spmid)

  if (length(spm_missing) > 0) { # CAVE: FIXES original object!
    data_svar <- data_svar[!data_svar$spmid %in% spm_missing, ]
    delete_from_qop_svar <- !qop_data[["svar"]]$spmid %in% spm_missing
    qop_data[["svar"]] <- qop_data[["svar"]][delete_from_qop_svar, ]

    data_spm <- qop_data[!qop_data$spmid %in% spm_missing, ]
    delete_from_qop_spm <- !qop_data[["spm"]]$spmid %in% spm_missing
    qop_data[["spm"]] <- qop_data[["spm"]][delete_from_qop_spm, ]

    tkmessageBox(message = "There seems to be a problem and an
      item was deleted from analysis. Check results and warning.txt")
      sink("_warning.txt")
      cat("Problem with ITEM: ", spm_missing, "\n\r")
      cat("-Item was deleted from dataset")
      sink()
      print(getwd())
  } else {
    cat("\n\r --- No issue on missing items detected ---\n\r")
  }

  return(qop_data)
}
