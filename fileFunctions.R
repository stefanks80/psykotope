# INFO ---------------------------------------
# All directories conform to an identical structure
# Datasets within each directory:
# *_spm -> spørsmål (= txt of answer options)
# *_spmtekst -> spørsmål (=Questions w/ txt)
# *_svar -> student responses
# ____________________________________________


choose_file_dir_dialogue <- function(set_path = FALSE) {
  tkmessageBox(message = "Choose raw data directory")
  work_dir <- tk_choose.dir()
  for_control <- length(dir(work_dir, "^statistik.*.txt$")) == 3

  if (for_control == TRUE) 
    cat("\n All relevant files found \n")

  if (for_control == FALSE)
    cat("\n Check folder, no statistikk files found! \n")

  if (set_path == TRUE) setwd(work_dir)
  return(work_dir)
}


read_statistikk <- function(exam_path) {
  # Loads and formats exam data from the examination system
  # Returns a list of objects for further use in the analyses

  spmtekst <- read.table( # Reads questions-texts
    paste(exam_path, "/statistikk_spmtekst.txt", sep = ""),
    sep = "\t",
    fileEncoding = "UTF-8",
    encoding = "ISO-8859-1", as.is = TRUE, quote = "")

  names(spmtekst) <- c("spmid", "tekst")

  spm <- read.table( # Reads question infos etc.
    paste(exam_path, "/statistikk_spm.txt", sep = ""),
    sep = "\t",
    fileEncoding = "UTF-8", encoding = "ISO-8859-1",
    as.is = TRUE, quote = "")

  names(spm) <- c(
    "oppgave",
    "blokk",
    "spmnr",
    "spmid",
    "type",
    "fag",
    "altnr",
    "alttekst",
    "opsjnr",
    "opsjtekst",
    "korrekt")

  spm$kortform <- gsub("([^_]+)_([^_]+)_([^_]+)_([^_]+)", "\\2", spm$oppgave)

  spm$opsjtekst <- ifelse(
    nchar(spm$opsjtekst) > 50,
    paste0(substr(spm$opsjtekst, 0, 50), "..."),
    spm$opsjtekst)

  svar <- read.table( # Reads question-responses
    paste(exam_path, "/statistikk_svar.txt", sep = ""),
    sep = "\t",
    fileEncoding = "UTF-8",
    encoding = "ISO-8859-1",
    as.is = TRUE,
    quote = "")

  names(svar) <- c(
    "kandnr",
    "studid",
    "oppgave",
    "spmid",
    "type",
    "sensor",
    "kar",
    "altnr",
    "opsjnr")

  spm[spm$korrekt == "true", "korsymbol"] <- "*"
  spm[spm$korrekt == "false", "korsymbol"] <- " "

  # For ESSAY items
  j <- unique(svar[svar$type == "ESSAY", c("studid", "kar", "spmid", "sensor")])

  max_rating_sensor <- names(sort(table(j$sensor), decreasing=TRUE))[1]

  sensor_ratings <- j
  sensor_ratings$ext_sensor <- ifelse(j$sensor %in% max_rating_sensor, 1, 2)

  sensor_ratings <- sensor_ratings[order(
    sensor_ratings$studid, sensor_ratings$spmid, sensor_ratings$sensor), ]

  slong <- j[order(j$studid, j$spmid, j$sensor), ]

  l <- dim(slong)[1]
  ll <- dim(unique(slong[, c("studid", "spmid")]))[1]

  if (ll == l) {
    tosensorer <- FALSE
  } else {
    tosensorer <- TRUE
    s  <- slong[seq(from = 1, to = l, by = 2), ]
    ss <- slong[seq(from = 2, to = l, by = 2), ]

    sensur <- merge(s, ss, by = c("studid", "spmid"), suffixes = c("", "2"))

    sensur2 <- reshape(
      sensor_ratings,
      direct = "wide",
      idvar = c("studid", "spmid"),
      timevar = "ext_sensor",
      sep = "")

    names(sensur2) <- gsub("1", "", names(sensur2))
    sensur <- na.omit(sensur2)
    sensur <- sensur[, c("studid", "spmid", "kar", "sensor", "kar2", "sensor2")]
  }

  if (tosensorer == TRUE) {
    out_list <- list(spmtekst, spm, svar, sensur)
    names(out_list) <- c("spmtekst", "spm", "svar", "sensur")
    return(out_list)
  } else {
    out_list <- list(spmtekst, spm, svar)
    names(out_list) <- c("spmtekst", "spm", "svar")
    return(out_list)
  }
}
