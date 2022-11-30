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
source("plotFunctionESS.R")

# Plot theme
source("plotThemes.R")

# ------------------------------------------------------------------------------
# Source Item Preparation Functions
# ------------------------------------------------------------------------------

source("itemprepFunctions.R") # 

# ------------------------------------------------------------------------------
# Source Functions for CTT Item-Descriptives
# ------------------------------------------------------------------------------

source("itemStats.R") # Calculates basic CTT for CR and SR
source("itemtabFunctions.R") # Calculates basic CTT for CR and SR

# ------------------------------------------------------------------------------
# Source IndexCard Functions and TeX additions
# ------------------------------------------------------------------------------

source("texParts.R") # Contains Header for TeX files
source("makeIndexcardMCMR.R")
source("makeIndexcardSEL.R")

################################################################################
#
# Define working directory for TeX and plots
#
################################################################################

output_dir <- "C:/temp"

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
# Arrange item info itemtext and answering options (OnPremise-specific)
# ------------------------------------------------------------------------------

mainitemtext <- exam_data[["spmtekst"]]
mainitemtext <- split(mainitemtext, mainitemtext$spmid)
mainitemtext <- lapply(mainitemtext, function(df) df[, "tekst"])

itemalt_text <- prep_alttext(exam_data)
iteminfo_label <- prep_iteminfo(exam_data)
item_short_lab <- prep_itemabrev(exam_data)

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
    MoreArgs = list(out_dir = output_dir))

# ------------------------------------------------------------------------------
# For plotting SEL items / This is Onpremise-specific
# ------------------------------------------------------------------------------

# Prepare response data for Pull-Down items
for_sel_plots <- prepare_onpremise_sel(exam_data)
for_sel_plots <- merge(for_sel_plots, exam_score_levels, all.x = TRUE)

sel_data_split <- split(for_sel_plots, for_sel_plots$spmid)

# Make SEL plots
lapply(sel_data_split, plot_sel, item_id = "spmid", out_dir = output_dir)

# ------------------------------------------------------------------------------
# For plotting ESSAY items
# ------------------------------------------------------------------------------

ess_data <- prepare_onpremise_ess(exam_data)

ess_data <- merge(ess_data, exam_item_score, by = c("spmid", "kandnr"))
ess_data <- merge(ess_data, exam_score_levels)

ess_data_split <- split(ess_data, ess_data$spmid)

lapply(ess_data_split, plot_ess, out_dir = output_dir)

################################################################################
#
# Make tables & CTT stats for items
#
################################################################################

item_tabs <- mapply(tabfunc_mc_mr, # Generic function
    mc_tab = items_to_plot,
    answer_key_df = items_keys,
    MoreArgs = list(candidate_level_label = "score_level_label"))

# ------------------------------------------------------------------------------
# Make item-statistics (DISCR & DIFF)
# ------------------------------------------------------------------------------

foritem_stats <- merge(exam_score_levels, exam_item_score, all = TRUE)

item_list <- split(foritem_stats, foritem_stats$spmid)
item_list <- lapply(item_list, calc_ctt_item, item_score_max = 6)

# ------------------------------------------------------------------------------
# Make item-statistics (Kappa & Weighted Kappa)
# ------------------------------------------------------------------------------

kappa_list <- lapply(ess_data_split, kappa_calc)

################################################################################
#
# Generate filecards for MC/MR items
#
################################################################################

lapply(names(item_tabs),
    generate_indexcard_mc,
    fig_path = output_dir, # Path to figures
    tex_path = output_dir, # Where to write
    itemtab = item_tabs, # Item-tabs with response freq
    item_options_text = itemalt_text, # LIST named by itemid containing text
    itemstem_text = mainitemtext, # LIST named by item id containing option text
    item_label = iteminfo_label, # LIST 
    item_abrev = item_short_lab, # LIST
    cttstats = item_list,
    owndoc = TRUE
 )
