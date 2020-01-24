count_nas <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

rm_all_except_functions <- function() {
  rm(list=setdiff(ls(all.names=TRUE, envir=globalenv()), lsf.str(all.names=TRUE, envir=globalenv())), envir=globalenv())
}

# this assumes name of the date column
add_date_based_features <- function(df) {
  return(
    df %>% 
      mutate(day_of_week = wday(application_date, label = TRUE, abbr = TRUE),
             day_of_month = mday(application_date),
             day_of_year = yday(application_date),
             month = lubridate::month(application_date, label = TRUE, abbr = TRUE),
             quarter = lubridate::quarter(application_date),
             year = lubridate::year(application_date),
             year_month = paste(year, month, sep = "_"),
             year_quarter = paste(year, quarter, sep = "_"),
             is_end_of_month = (application_date == (ceiling_date(application_date, unit = "months") - days(1))),
             part_of_month = cut(day_of_month, c(0,2,29,31), c('start', 'mid', 'end')),
             is_weekend = day_of_week %in% c('Sat', 'Sun'))
  )
}

check_if_date_is_continuous <- function(df) {
  unique_dates_in_data = unique(df$application_date)
  span_of_data = as.numeric(max(unique_dates_in_data) - min(unique_dates_in_data)) + 1
  return(span_of_data == n_distinct(df$application_date))
}
