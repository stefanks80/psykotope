# GENERAL OPTIONS
rm(list = ls())
options(max.print = 200, scipen = 20)

# ------------------------------------------------------------------------------
# Load functions first!
# ------------------------------------------------------------------------------

wdPath <- chooseFileDirDialogue(setPath=TRUE)

dirContent <- dir(wdPath, recursive=TRUE)

examDatL <- LoadExamData(wdPath)

examDatPrep <- CombineAndFill(examDatL) 

# ------------------------------------------------------------------------------
# Prepare data for plotting
# ------------------------------------------------------------------------------

# Plot single-best-answer

# Plot MR

# Plot Essay

