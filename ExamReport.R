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
# Source Functions
# ------------------------------------------------------------------------------

source("loadPackages.R")
source("fileFunctions.R")
source("dataprepFunctions.R")
source("cleaningFunctions.R")
source("plotFunctionMC.R")

# Plotting functions
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
exam_response_data <- prepare_response_data(exam_data) # Excludes Essays

# For plotting SR items

for_item_plots <- merge(exam_response_data, exam_score_levels, all.x = TRUE)
for_item_plots <- merge(for_item_plots, exam_item_score, all.x = TRUE)

# Plot single-best-answer

mc_items <- for_item_plots[for_item_plots$type %in% "MC", ]
mc_items <- split(mc_items, mc_items$spmid)

lapply(mc_items, plot_mc_mr, out_dir = exam_path)

# Plot MR
mr_items <- for_item_plots[for_item_plots$type %in% "MR", ]
mr_items <- split(mr_items, mr_items$spmid)

lapply(mr_items, plot_mc_mr, out_dir = exam_path)

# Plot SEL

# Plot Essay
