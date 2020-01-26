source('utils.R')
library(prophet)

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

future = make_future_dataframe(m, periods = 92, freq='day') %>%  filter(ds >= date('2019-04-05'))
future <- future %>% 
  left_join(validation_data %>% rename('ds' = 'application_date') %>% mutate('ds' = as.POSIXct(ds)), by=('ds'))

forecast = predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

#### Check APE for validation ####
validation_preds <- forecast %>% 
  select(ds, yhat) %>% 
  filter(ds >= date('2019-04-05'))

ape_dist <- getAPE(validation_data$case_count, validation_preds$yhat)
summary(ape_dist$ape)

validation_data$prophet_prediction <- validation_preds$yhat
write_csv(validation_data, 'data/predictions/prophet_segment_1.csv')

