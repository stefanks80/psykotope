# INFO ---------------------------------------
# All directories conform to an identical structure
# Datasets within each directory:
# *_spm -> spørsmål (= txt of answer options)
# *_spmtekst -> spørsmål (=Questions w/ txt)
# *_svar -> student responses
# ____________________________________________


## Required Packages

library(tcltk)


choose_file_dir_dialogue <- function(set_path = FALSE) {
  tkmessageBox(message = "Choose raw data directory")
  work_dir <- tk_choose.dir()
  if (set_path == TRUE) setwd(work_dir)
  return(work_dir)
}


# ------------------------------------------------------------------------------
#
#
# Functions for checking for correct file format, issues with HTML in text osv. 
#
#
# ------------------------------------------------------------------------------


# ...



# ------------------------------------------------------------------------------
#
#
# Functions for reading the statistics from QP OnPremise output
#
#
# ------------------------------------------------------------------------------

LoadExamData <- function(pathToExam){
  # Loads and formats exam data from the examination system
  
  spmtekst <- read.table(paste(pathToExam, "/statistikk_spmtekst.txt", sep=""), sep="\t", fileEncoding="UTF-8", encoding="ISO-8859-1", as.is=TRUE, quote = "")
  names(spmtekst) <- c('spmid','tekst')

  spm <- read.table(paste(pathToExam, "/statistikk_spm.txt", sep=""), sep="\t", fileEncoding="UTF-8", encoding="ISO-8859-1", as.is=TRUE, quote = "")
  names(spm) <- c('oppgave','blokk','spmnr','spmid','type','fag','altnr','alttekst','opsjnr','opsjtekst','korrekt') 
  spm$kortform <- gsub('([^_]+)_([^_]+)_([^_]+)_([^_]+)','\\2',spm$oppgave)
  spm$opsjtekst <- ifelse(nchar(spm$opsjtekst) > 50, paste0(substr(spm$opsjtekst, 0, 50), "..."), spm$opsjtekst)
  
  svar <- read.table(paste(pathToExam, "/statistikk_svar.txt", sep=""), sep="\t", fileEncoding="UTF-8", encoding="ISO-8859-1", as.is=TRUE, quote = "")
  names(svar) <- c('kandnr', 'studid', 'oppgave','spmid','type','sensor','kar','altnr','opsjnr')
  
  spm[spm$korrekt == 'true','korsymbol'] <- '*'
  spm[spm$korrekt == 'false','korsymbol'] <- ' '

  srtspm <- spm[order(spm$oppgave,spm$blokk,spm$spmnr,spm$altnr,spm$opsjnr),]
  spmseq <- unique(srtspm[,c('oppgave','blokk','spmnr','spmid','type')])
  spmseq$kortform <- gsub('([^_]+)_([^_]+)_([^_]+)_([^_]+)','\\2',spmseq$oppgave)
  spmseq[,"idfactor"] <- factor(spmseq$spmid)
  fagseq <- unique(srtspm[,c('fag','spmid')])
  fagseq[,"idfactor"] <- factor(fagseq$spmid)

  oo <- unique(spm[,c('oppgave','spmid')])
  oppg <- sort(unique(oo$oppgave))

  ff <- unique(spm[,c('fag','spmid')])
  fag <- sort(unique(ff$fag))

  kand <- unique(svar[,c('studid', 'kandnr')])

  j <- unique(svar[,c('studid','spmid','sensor','kar')])
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


# ------------------------------------------------------------------------------
#
#
# Function for calculating student scores and other info
#
# CAVE: CHECK IF karMean is used or can be deleted!
# ------------------------------------------------------------------------------

CombineAndFill <- function(dataL, nCat=4){
  # Person Means ('sum scores') and Option-Code
  persMean <- dataL[["svar"]]
  
  karMean <- aggregate(persMean$kar, by=list(persMean$kandnr, persMean$spmid), mean)
  names(karMean) <- c("kandnr", "spmid", "karMean")
  persMean$karMean <- NULL
  
  persMean <- merge(persMean, karMean, all.x=TRUE)
 
  persMean[is.na(persMean$karMean), "karMean"] <- persMean[is.na(persMean$karMean), "kar"]
  persMean$oldKar <- persMean$kar
  persMean$kar <- persMean$karMean

  dataL[["svar"]] <- persMean
  dataL[["svar"]]$oldKar <- NULL
  
  persMean <-  unique(persMean[ , c("kandnr", "spmid", "studid", "kar")])
  persMean <- data.frame(tapply(persMean$kar, persMean$kandnr, mean)/6)
  
  names(persMean) <- "sum_score"
  persMean$sum_score <-  round(persMean$sum_score, 4)*100
  persMean$kandnr <- row.names(persMean)
 
  persMean$sumScoreFact <- cut(persMean$sum_score, 
      breaks=quantile(persMean$sum_score, seq(0, 1, 1/nCat)), 
	  include.lowest=TRUE)
 
  levels(persMean$sumScoreFact) <- gsub(
      "^(\\D{1})(\\d+\\.*\\d*)(,)(\\d+\\.*\\d*)(\\D+)", "\\2\\%-\\4\\%", 
      levels(persMean$sumScoreFact)
	)
  
  fTab <- data.frame(ftable(persMean$sumScoreFact))

  persMean$scoreFactor1 <- persMean$sumScoreFact
  
  levels(persMean$sumScoreFact) <- paste0(levels(persMean$sumScoreFact), "\n(N=", fTab$Freq, ")")
    
  # Add answer options
  dataSpm <- dataL[["spm"]]
  dataSpm <- dataSpm[, c("spmid", "altnr", "korrekt", "opsjnr")]
  dataSvar <- dataL[["svar"]]
  spmMissing <- setdiff(dataSpm$spmid, dataSvar$spmid)

  if(length(spmMissing) > 0){
    dataSvar <- dataSvar[!dataSvar$spmid %in% spmMissing, ]
    dataL[["svar"]] <- dataL[["svar"]][!dataL[["svar"]]$spmid %in% spmMissing, ]
    dataSpm <- dataSpm[!dataSpm$spmid %in% spmMissing, ]
    dataL[["spm"]] <- dataL[["spm"]][!dataL[["spm"]]$spmid %in% spmMissing, ]	
    
  tkmessageBox(message="There seems to be a problem and an item was deleted from analysis. Check results and warning.txt")
	sink("_warning.txt")
	  cat("Problem with ITEM: ", spmMissing, "\n\r")
	  cat("-Item was deleted from dataset")
	sink()	
	print(getwd())
  }

  
  dataSvarL <- split(dataSvar, dataSvar$studid)  
  dataTF <- mapply(function(X, Y){merge(Y, X, all=TRUE)}, dataSvarL, MoreArgs=list(dataSpm), SIMPLIFY=FALSE)
  dataTF <- mapply(function(X, Y){X$studid <- Y; return(X)}, dataTF, names(dataTF), SIMPLIFY=FALSE)
   
  dataTF <- do.call('rbind', dataTF)
	  
  dataTF$response <- ifelse(is.na(dataTF$kar), 0, 1)
  dataTF$korrekt <- as.numeric(as.logical(dataTF$korrekt))
  
  dataTF <- split(dataTF, dataTF$studid)
	  
  substituteKar <- function(X){
    
	Xsplit <- split(X, X$spmid)
    
	subInner <- function(Xl){
      Xl$kar <- ifelse(FALSE %in% is.na(Xl$kar), unique(na.exclude(Xl$kar)), 0)
      Xl$type <- ifelse(FALSE %in% is.na(Xl$type), unique(na.exclude(Xl$type)), NA)
      Xl$sensor <- ifelse(FALSE %in% is.na(Xl$sensor), unique(na.exclude(Xl$sensor)), NA)
      Xl$kandnr <- ifelse(FALSE %in% is.na(Xl$kandnr), unique(na.exclude(Xl$kandnr)), NA)
      Xl$oppgave <- ifelse(FALSE %in% is.na(Xl$oppgave), unique(na.exclude(Xl$oppgave)), NA)
      return(Xl)
    }
	
    Xsplit <- mapply(subInner, Xsplit, SIMPLIFY=FALSE)
    
    Xsplit <- do.call(rbind, Xsplit)
    return(Xsplit)
  }
  
  dataTF <- mapply(substituteKar, dataTF, SIMPLIFY=FALSE)
  dataTF <- do.call(rbind, dataTF)

  spmMissings <- unique(dataTF[apply(dataTF, 1, function(x) NA %in% x), "spmid"])

  row.names(dataTF) <- NULL
   
  dataTF <- merge(dataTF, persMean, all=TRUE)

  dataTF[ , "alt_letters"] <- "#"
  dataTF[dataTF$altnr >= 0, "alt_letters"] <-  letters[dataTF[dataTF$altnr >= 0, "altnr"]+1]
   
  dataTF[(dataTF$korrekt %in% 1) & (dataTF$response %in% 0), "alt_letters"] <- paste0("#",
  dataTF[(dataTF$korrekt %in% 1) & (dataTF$response %in% 0), "alt_letters"])
  
  dataTF$selMR <- ifelse((dataTF$response %in% 0 &  dataTF$korrekt %in% 1) | (dataTF$response %in% 1), 1, 0)

  dataTF <- dataTF[((dataTF$selMR %in% 1) & (dataTF$type %in% "MR")) | ((dataTF$response %in% 1) & (!dataTF$type %in% c("MR"))), ]  
  
  dataTF[apply(dataTF, 1, function(x){sum(is.na(x))==length(x)}), ]
  
  dataL[["svar"]] <- unique(dataTF)
  
  return(dataL)
}

# ------------------------------------------------------------------------------
#
#
# Function for cleaning question-text
#
#
#
# ------------------------------------------------------------------------------