library(utils)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readr", "readxl", "zoo", "manipulate", "xts", "dygraphs", "plotly", "plyr", "reshape2", "magrittr", "tidyverse", "ggthemes", "RColorBrewer", "lubridate", "janitor", "caret", "assertr", "grid", "ggforce", "rpart", "rpart.plot", "knitr")

ipak(packages)

options("scipen"=100, "digits"=4) #### Use this to not display no. in exponent format in R
