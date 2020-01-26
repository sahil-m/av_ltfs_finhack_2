count_nas <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

rm_all_except_functions <- function() {
  rm(list=setdiff(ls(all.names=TRUE, envir=globalenv()), lsf.str(all.names=TRUE, envir=globalenv())), envir=globalenv())
}

first_day_of_month_wday <- function(date) {
  lubridate::day(date) <- 1
  lubridate::wday(date)
}

week_of_month <- function(date) {
  ceiling((lubridate::day(date) + first_day_of_month_wday(date) - 1) / 7)
}
week_of_month <- function(date) {
  ceiling((lubridate::day(date) + first_day_of_month_wday(date) - 1) / 7)
}

# this assumes name of the date column
add_date_based_features <- function(df) {
  return(
    df %>% 
      mutate(day_of_week = wday(application_date, label = TRUE, abbr = TRUE),
             day_of_month = mday(application_date),
             day_of_year = yday(application_date),
             week_of_month = week_of_month(application_date),
             month = lubridate::month(application_date, label = TRUE, abbr = TRUE),
             quarter = lubridate::quarter(application_date),
             year = lubridate::year(application_date),
             year_month = paste(year, month, sep = "_"),
             year_quarter = paste(year, quarter, sep = "_"),
             is_end_of_month = (application_date == (ceiling_date(application_date, unit = "months") - days(1))),
             part_of_month = cut(day_of_month, c(0,2,29,31), c('start', 'mid', 'end')),
             is_weekend = day_of_week %in% c('Sat', 'Sun'),
             label = paste(application_date, 'week', week_of_month, day_of_week, sep = '_')
             )
  )
}

check_if_date_is_continuous <- function(df) {
  unique_dates_in_data = unique(df$application_date)
  span_of_data = as.numeric(max(unique_dates_in_data) - min(unique_dates_in_data)) + 1
  return(span_of_data == n_distinct(df$application_date))
}

## Taken in the actual and predicted columns only ##
getAPE <- function(actual, predicted) {
  df <- data.frame('actual' = actual, 'predicted' = predicted)
  
  df <- df %>%
    mutate(
      'abs_error' = abs(actual - predicted),
      'ape' = if_else(
        actual != 0,
        abs(actual - predicted) / abs(actual) * 100,
        abs(actual - predicted) / 1 * 100
      )
    )
  
  return(df)
}

## Takes in the APE distribution for comparable models ##
visualise_model_ape_comparison <- function(current_ape_dist, base_ape_dist) {
  df <- data.frame('ape_dist' = current_ape_dist, 'model' = 'current_model')
  df1 <- data.frame('ape_dist' = base_ape_dist, 'model' = 'base_model')
  
  df <- rbind(df, df1)
  
  ggplotly(
    ggplot(df, aes(x=ape_dist, color = model, fill = model)) +
      geom_density(alpha=0.5)
  )
}

combine_segment_1_and_segment_2_predictions <- function(segment_1_preds, segment_2_preds) {
  segment_1_preds_filtered <- segment_1_preds %>% 
    select(id, application_date, segment, case_count)
  
  segment_2_preds_filtered <- segment_2_preds %>% 
    select(id, application_date, segment, case_count)
  
  final_predictions <- segment_1_preds_filtered %>% 
    bind_rows(segment_2_preds_filtered)
  
  return(final_predictions)
}