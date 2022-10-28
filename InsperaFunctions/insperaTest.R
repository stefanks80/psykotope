##################################
## Setting the Environment      ##
##################################

rm(list=ls())

.libPaths()
.libPaths("C:/prog/R/lib")

##################################
## Loading packages             ##
##################################

library(rjson)
library(pdftools)

##################################
## Reading and preparing data   ##
##################################

setwd("M:/01 Eksamen/04 Inspera Test")

result <- fromJSON(file = "6800/results.json")

candidates <- result[["ext_inspera_candidates"]]

nCandidates <- length(candidates)
nItems <- length(candidates[[1]]$result$ext_inspera_questions)

namesL <- NULL
namesL <- list()

for(i in 1:nItems){
  checkScore <- names(candidates[[1]]$result$ext_inspera_questions[[i]])
  namesL[[i]] <- c(manualScore="ext_inspera_manualScores" %in% checkScore)
  }
  
for(i in 1:nItems){
 responseTest <- candidates[[2]]$result$ext_inspera_questions[[i]]$ext_inspera_autoScore
 print(responseTest)
}

candidates[[2]]$result$ext_inspera_questions[[140]]

##################################
## Inspera PDF Test             ##
##################################

pdfFile <- "6800/MED6800Inspera.pdf"

pdf_info(pdfFile)

pdfText <- pdf_text(pdfFile)
pdfData <- pdf_data(pdfFile)


pdfTextTest <- paste0(pdfText, collapse="+++++")
pdfTextTest <- strsplit(pdfTextTest, "MED6800")

as.data.frame(pdfData[[2]])