library(utils)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages_always_required <- c("readr", "readxl", "zoo", "plotly", "plyr", "reshape2", "magrittr",  "tidyverse", "ggthemes", "RColorBrewer", "lubridate", "janitor", "assertr", "grid", "ggforce", "knitr")

packages_hierarchy <- c("treemap", "data.tree", "networkD3")

packaging_models_basic <- c("caret", "rpart", "rpart.plot", "Metrics", "h2o")

time_series_packages <- c("manipulate", "xts", "timetk", "tidyquant", "dygraphs", "forecast", "stR")


packages <- c(packages_always_required, packages_hierarchy, packaging_models_basic, time_series_packages)

ipak(packages)

options("scipen"=100, "digits"=4) #### Use this to not display no. in exponent format in R
