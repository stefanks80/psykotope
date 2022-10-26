# GENERAL OPTIONS
rm(list = ls())
options(max.print = 400, scipen = 20)

# ------------------------------------------------------------------------------
# Load functions first!
# ------------------------------------------------------------------------------

wd_path <- choose_file_dir_dialogue(setPath = TRUE)

dir_content <- dir(wd_path, recursive = TRUE)

exam_data <- LoadExamData(wd_path)

exam_data_statistikk <- CombineAndFill(exam_data) 

# ------------------------------------------------------------------------------
# Prepare data for plotting
# ------------------------------------------------------------------------------

# Plot single-best-answer

# Plot MR

# Plot Essay
