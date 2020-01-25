rm(list = ls())
source("utils.R")

source("data_cleaning.R")
rm_all_except_functions()

source("create_date_level_data.R")
rm_all_except_functions()

# Check EDA from eda_sahil_2.R

source("split.R")
rm_all_except_functions()

source("convert_to_msts.R")
rm_all_except_functions()

source("baseline_models_date_features_based.R")