# GENERAL OPTIONS
rm(list = ls())
options(max.print = 200, scipen = 20)

getwd()
source("loadPackages.R")
source("fileFunctions.R")
source("dataprepFunctions.R")
source("cleaningFunctions.R")

# ------------------------------------------------------------------------------
# Load functions first!
# ------------------------------------------------------------------------------

exam_path <- choose_file_dir_dialogue(set_path = TRUE)

exam_data <- read_statistikk(exam_path)

qop_data <- exam_data

# ------------------------------------------------------------------------------
# Prepare data: Check data
# CAVE: Check text for LaTeX use
# ------------------------------------------------------------------------------

exam_data <- check_item_mismatch(exam_data)

# ------------------------------------------------------------------------------
# Prepare data for plotting
# ------------------------------------------------------------------------------

exam_item_score <- perpare_onpremise_itemscores(exam_data)
exam_score_levels <- perpare_onpremise_scorelevels(exam_item_score)
exam_response_data <- prepare_response_data(exam_data)

 

# Plot single-best-answer

# Plot MR

# Plot Essay
