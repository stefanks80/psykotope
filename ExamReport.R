########################################################
########################################################
# Generating Exam Report 
# Date: 22-10-31
#
#
#
########################################################
########################################################


# GENERAL OPTIONS
rm(list = ls())
options(max.print = 200, scipen = 20)

getwd()

# ------------------------------------------------------------------------------
# Source Packages
# ------------------------------------------------------------------------------

source("loadPackages.R")

# ------------------------------------------------------------------------------
# Source Data Read and Prep Functions
# ------------------------------------------------------------------------------

source("fileFunctions.R")
source("prepareOnpremiseData.R")
source("prepareOnpremiseResponses.R")
source("cleaningFunctions.R")

# ------------------------------------------------------------------------------
# Source Plotting Functions
# ------------------------------------------------------------------------------

source("plotFunctionMC.R")
source("plotFunctionMC.R")

# Plot theme
source("plotThemes.R")

# ------------------------------------------------------------------------------
# Load functions first!
# ------------------------------------------------------------------------------

exam_path <- choose_file_dir_dialogue(set_path = FALSE)
exam_data <- read_statistikk(exam_path)

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
exam_item_correct <- prepare_onpremise_key(exam_data)

# For plotting MC and MR items
exam_response_data <- prepare_onpremise_mcmr(exam_data) # OnPremise Specific

for_item_plots <- merge(exam_response_data, exam_score_levels, all.x = TRUE)
for_item_plots <- merge(for_item_plots, exam_item_score, all.x = TRUE)

test <- for_item_plots[for_item_plots$spmid == 43179, ]
mc_data <- test

lapply(mr_items, plot_mc_mr, out_dir = exam_path)

# Plot SEL

# Plot Essay
