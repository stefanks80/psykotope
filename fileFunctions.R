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
  if (set_path == TRUE) setwd(work_dir)
  return(work_dir)
}

# ------------------------------------------------------------------------------
#
#
# Functions for reading the statistics from QP OnPremise output
#
#
# ------------------------------------------------------------------------------

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

  srtspm <- spm[
    order(spm$oppgave, spm$blokk, spm$spmnr, spm$altnr, spm$opsjnr),
    ]

  spmseq <- unique(srtspm[, c("oppgave", "blokk", "spmnr", "spmid", "type")])

  spmseq$kortform <- gsub(
    "([^_]+)_([^_]+)_([^_]+)_([^_]+)", "\\2", 
    spmseq$oppgave)

  spmseq[, "idfactor"] <- factor(spmseq$spmid)

  fagseq <- unique(srtspm[, c("fag", "spmid")])

  fagseq[, "idfactor"] <- factor(fagseq$spmid)

  oo <- unique(spm[, c("oppgave", "spmid")])
  oppg <- sort(unique(oo$oppgave))

  ff <- unique(spm[, c("fag", "spmid")])
  fag <- sort(unique(ff$fag))

  kand <- unique(svar[,c("studid", "kandnr")])

  j <- unique(svar[,c("studid", "spmid", "sensor", "kar")])
  jj <- aggregate(x=j[,c('studid','spmid','kar')], by=list(j$studid, j$spmid), mean)
  snittkar <- merge(merge(jj, oo, by=c('spmid','spmid')), ff, by=c('spmid','spmid'))

  j <- unique(svar[svar$type == 'ESSAY', c('studid','kar','spmid','sensor')])
  
  sensorMaxRatings <- names(sort(table(j$sensor), decreasing=TRUE))[1]
  
  forNewSens <- j
  forNewSens$sensorExt <- ifelse(j$sensor %in% sensorMaxRatings, 1, 2)
  forNewSens <- forNewSens[order(forNewSens$studid, forNewSens$spmid, forNewSens$sensor),]
  
  slong <- j[order(j$studid, j$spmid, j$sensor),]

  l <- dim(slong)[1]
  ll <- dim(unique(slong[,c('studid','spmid')]))[1]
  

  if (ll == l) {
    tosensorer <- F
    #  sensur <- slong
    #  sensur[,"sensor2"] <- NA
    #  sensur[,"kar2"] <- NA
  } else {
    tosensorer <- T
    s  <- slong[seq(from=1,to=l,by=2),]
    ss <- slong[seq(from=2,to=l,by=2),]
    
    sensur <- merge(s,ss,by=c('studid','spmid'),suffixes=c('','2'))
    sensorer <- unique(sensur[,c('sensor','sensor2')])

    sensur2 <- reshape(forNewSens, direct="wide", idvar=c("studid", "spmid"), timevar="sensorExt", sep="")
    
    names(sensur2) <- gsub("1", "", names(sensur2))

    sensur0 <- sensur
    sensur <- na.omit(sensur2)
    sensur <- sensur[, c('studid','spmid', 'kar', 'sensor', 'kar2', 'sensor2')]

  }
  
  if (tosensorer == T) {
    L <- list(spmtekst, spm, svar, sensur)
    names(L) <- c("spmtekst", "spm", "svar", "sensur")
    return(L)
  } else {
    L <- list(spmtekst, spm, svar)
    names(L) <- c("spmtekst", "spm", "svar")
    return(L)
  }	
}
