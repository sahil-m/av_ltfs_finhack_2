get_prrophet_model <- function(df, holidays_df) {
  formatted_data <- df %>% 
    rename('ds' = application_date,  'y'=case_count)
  
  national_holidays <- data_frame(
    holiday = 'national_holidays',
    ds = holidays_df$date
  )
  
  m <- prophet(yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE, holidays = national_holidays) %>%
    # add_country_holidays(country_name = 'IN') %>% 
    add_seasonality(name = "weekly", period = 7, fourier.order = 10) %>% 
    add_seasonality(name = "monthly", period = 30.5, fourier.order = 15) %>% 
    add_regressor("is_end_of_month") %>% 
    add_regressor("is_weekend") %>% 
    fit.prophet(formatted_data)
  
  return(m)
}

get_prophet_predictions_as_feature <- function(m, future_data, future_period) {
  train_predictions <- predict(m) %>% 
    select(ds, yhat)
  
  future_data_formatted <- future_data %>% 
    rename('ds' = 'application_date') %>% 
    mutate(ds = as.POSIXct(ds))
  
  future_df = make_future_dataframe(m, periods = future_period, freq='day', include_history = FALSE) %>% left_join(future_data_formatted, by="ds")
  
  validation_predictions <- predict(m, future_df) %>% 
    select(ds, yhat)
  
  prophet_predictions_df <- bind_rows(train_predictions, validation_predictions) %>% 
    mutate(ds = ymd(ds)) %>% 
    rename(application_date = ds, prophet_predictions = yhat) %>% 
    mutate(prophet_predictions = if_else(prophet_predictions < 0, 0, prophet_predictions))
  
  return(prophet_predictions_df)
  
}

create_prophet_prediction_feature <- function(df, holidays_df, future_data, future_period) {
  m <- get_prrophet_model(df, holidays_df)
  
  prophet_predictions_df <- get_prophet_predictions_as_feature(m, future_data, future_period)
  
  return(prophet_predictions_df)
}