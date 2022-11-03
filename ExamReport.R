########################################################
########################################################
# Generating Exam Report 
# Date: 22-10-31
# CAVE: Check latexify if useful?
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
source("cleaningFunctions.R")

source("prepareOnpremiseData.R")
source("prepareOnpremiseResponses.R")
source("prepareOnpremiseSEL.R")

# ------------------------------------------------------------------------------
# Source Plotting Functions
# ------------------------------------------------------------------------------
source("plotFunctionMC.R")
source("plotFunctionSEL.R")

# Plot theme
source("plotThemes.R")

################################################################################
#
# Read and prepare data for an exam
#
################################################################################

# ------------------------------------------------------------------------------
# Read OnPremise/Statistikk Files
# ------------------------------------------------------------------------------

exam_path <- choose_file_dir_dialogue(set_path = FALSE)
exam_data <- read_statistikk(exam_path)

# ------------------------------------------------------------------------------
# Prepare data: Check data
# CAVE: Check text for LaTeX use
# ------------------------------------------------------------------------------

exam_data <- check_item_mismatch(exam_data)
exam_data <- clean_itemtext(exam_data)

# ------------------------------------------------------------------------------
# General dataprep for plotting
# ------------------------------------------------------------------------------

exam_item_score <- perpare_onpremise_itemscores(exam_data)
exam_score_levels <- perpare_onpremise_scorelevels(exam_item_score)

################################################################################
#
# Make item-plots for the different item-types
#
################################################################################

# ------------------------------------------------------------------------------
# For plotting MC and MR items
# ------------------------------------------------------------------------------

# Prepare response data and answer-key
exam_response_data <- prepare_onpremise_mcmr(exam_data) # OnPremise Specific
for_item_plots <- merge(exam_response_data, exam_score_levels, all.x = TRUE)
for_item_plots <- merge(for_item_plots, exam_item_score, all.x = TRUE)

exam_item_keys <- prepare_onpremise_mcmr_key(exam_data)

items_to_plot <- split(for_item_plots, for_item_plots$spmid)
items_keys <- split(exam_item_keys, exam_item_keys$spmid)

# Make MC and MR plots
mapply(plot_mc_mr, # Generic function
    mc_data = items_to_plot,
    answer_key_df = items_keys,
    MoreArgs = list(out_dir = exam_path))

# ------------------------------------------------------------------------------
# For plotting SEL items / This is Onpremise-specific
# ------------------------------------------------------------------------------

# Prepare response data for Pull-Down items
for_sel_plots <- prepare_onpremise_sel(exam_data)
for_sel_plots <- merge(for_sel_plots, exam_score_levels, all.x = TRUE)

sel_data_split <- split(for_sel_plots, for_sel_plots$spmid)

# Make SEL plots
lapply(sel_data_split, plot_sel, out_dir = exam_path) 

# ------------------------------------------------------------------------------
# For plotting ESSAY items
# ------------------------------------------------------------------------------

