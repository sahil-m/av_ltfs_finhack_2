source('utils.R')

counts_by_date <- read_csv('data/segment1_date_level.csv')
holidays <- read_csv('data/holidays_cleaned.csv')

train_data <- counts_by_date %>% 
  filter(application_date < date("2019-04-05"))

validation_data <- counts_by_date %>% 
  filter(application_date >= date("2019-04-05"))

national_holidays <- data_frame(
  holiday = 'national_holidays',
  ds = holidays$date
)

# Model building
formatted_data <- train_data %>% rename('ds' = application_date,  'y'=case_count)
m <- prophet(holidays = national_holidays, weekly.seasonality=TRUE)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- add_regressor(m, 'is_end_of_month')
m <- add_regressor(m, 'is_weekend')
m <- fit.prophet(m, formatted_data)

train_pred_obj <- predict(m)

train_predictions <- train_pred_obj %>% 
  select(ds, yhat)

future = make_future_dataframe(m, periods = 92, freq='day', include_history = FALSE) 
future <- future %>% 
  left_join(validation_data %>% rename('ds' = 'application_date') %>% mutate('ds' = as.POSIXct(ds)), by=('ds'))

forecast = predict(m, future)
# plot(m, forecast)
# prophet_plot_components(m, forecast)

validation_predictions <- forecast %>% 
  select(ds, yhat) %>% 
  filter(ds >= date('2019-04-05'))

#### Check APE for validation ####
# ape_dist <- getAPE(validation_data$case_count, validation_preds$yhat)
# summary(ape_dist$ape)


#### create a feature out of it #### 
prophet_predictions_df <- bind_rows(train_predictions, validation_predictions) %>% 
  mutate(ds = ymd(ds)) %>% 
  rename(application_date = ds, prophet_predictions = yhat) %>% 
  mutate(prophet_predictions = if_else(prophet_predictions < 0, 0, prophet_predictions))

write_csv(prophet_predictions_df, "data/prophet_predictions_df.csv")
