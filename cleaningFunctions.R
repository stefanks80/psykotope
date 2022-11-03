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

clean_itemtext <- function(qop_data){
  text_to_clean <- qop_data[["spmtekst"]]
  text_to_clean[, "tekst"] <- replace_html(text_to_clean[, "tekst"])
  text_to_clean[, "tekst"] <- replace_symbol_norsk(text_to_clean[, "tekst"])
  text_to_clean[, "tekst"] <- replace_white(text_to_clean[, "tekst"])
  text_to_clean[, "tekst"] <- replace_curly_quote(text_to_clean[, "tekst"])

  qop_data[["spmtekst"]] <- text_to_clean

  text_to_clean <- qop_data[["spm"]]
  clean_text <- text_to_clean$opsjtekst 
  clean_text <- replace_html(clean_text)
  clean_text <- replace_symbol_norsk(clean_text)
  clean_text <- replace_white(clean_text)
  clean_text <- replace_curly_quote(clean_text)
  text_to_clean$opsjtekst <- clean_text

  clean_text <- text_to_clean$alttekst 
  clean_text <- replace_html(clean_text)
  clean_text <- replace_symbol_norsk(clean_text)
  clean_text <- replace_white(clean_text)
  clean_text <- replace_curly_quote(clean_text)
  text_to_clean$alttekst <- clean_text
  qop_data[["spm"]] <- text_to_clean

  return(qop_data)
}

replace_symbol_norsk <- function(x, dollar = TRUE, percent = TRUE, 
    pound = TRUE, at = TRUE, and = TRUE, with = TRUE, ...) {

    y <- c(dollar, percent, pound, at, and, with, with)

    symbs <-  c("%", "$", "#", "&", "@", "w/o", "w/")
    replaces <- paste0(" ", c("prosent", "dollar", "antall", "og", "at", 
        "uten", "med"), " ")

    gsub("\\+", " ", mgsub(
        x,
        pattern = symbs[y], 
        replacement = replaces[y], 
        fixed = TRUE,
    ))
}
