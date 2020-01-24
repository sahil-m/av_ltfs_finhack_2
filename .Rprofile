library(utils)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readr", "readxl", "zoo", "manipulate", "xts", "timetk", "tidyquant", "dygraphs", "plotly", "plyr", "reshape2", "magrittr", "treemap", "data.tree", "networkD3", "tidyverse", "ggthemes", "RColorBrewer", "lubridate", "janitor", "caret", "assertr", "grid", "ggforce", "rpart", "rpart.plot", "knitr", "Metrics")

ipak(packages)

options("scipen"=100, "digits"=4) #### Use this to not display no. in exponent format in R

source("utils.R")
