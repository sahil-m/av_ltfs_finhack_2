library(forecast)
library(MLmetrics)

#### Segment 1 ####
train_data_s1 <- read_csv('data/train_segment_1_filled.csv')

ride_counts_by_data_s1 <- train_data_s1 %>% 
  group_by(application_date) %>% 
  summarise('count' = sum(case_count))

train_data <- ride_counts_by_data_s1 %>% 
  filter(application_date < date("2019-04-05"))

validation_data <- ride_counts_by_data_s1 %>% 
  filter(application_date >= date("2019-04-05"))

y <- msts(train_data$count, seasonal.periods = c(7,30))
autoplot(y)
fit <- tbats(y)
autoplot(forecast(fit), 92)
forecasts <- forecast(fit, 92)$mean

validation_data$predicted_count = forecasts

validation_data <- validation_data %>% 
  mutate('ape' = (abs(count - predicted_count) / count) * 100)
validation_data_wo_outliers <- validation_data %>% filter(ape < 100)

summary(validation_data_wo_outliers$ape)
mape <- mean(validation_data_wo_outliers$ape)

test_data <- read_csv('data/test.csv')

#### Segment 2 ####
train_data_s2 <- read_csv('data/train_segment_2_filled.csv')

ride_counts_by_data_s2 <- train_data_s2 %>% 
  group_by(application_date) %>% 
  summarise('count' = sum(case_count))

train_data_s2 <- ride_counts_by_data_s2 %>% 
  filter(application_date < date("2019-04-23"))

validation_data_s2 <- ride_counts_by_data_s2 %>% 
  filter(application_date >= date("2019-04-23"))

y_s2 <- msts(train_data_s2$count, seasonal.periods = c(30))
autoplot(y_s2)
fit_s2 <- tbats(y_s2)
autoplot(forecast(fit_s2), 92)
forecasts_s2 <- forecast(fit_s2, 92)$mean

validation_data_s2$predicted_count = forecasts_s2

validation_data_s2 <- validation_data_s2 %>% 
  mutate('ape' = (abs(count - predicted_count) / count) * 100)
validation_data_wo_outliers <- validation_data %>% filter(ape < 100)

summary(validation_data_wo_outliers$ape)
mape <- mean(validation_data_wo_outliers$ape)



#### Predict for segment 1 ####
test_data <- read_csv('data/test.csv')

train_data_s1 <- read_csv('data/train_segment_1_filled.csv')

ride_counts_by_data_s1 <- train_data_s1 %>% 
  group_by(application_date) %>% 
  summarise('count' = sum(case_count))


test_data_s1 <- test_data %>% filter(segment == 1)

train_ts <- msts(ride_counts_by_data_s1$count, seasonal.periods = c(7,30))
autoplot(train_ts)
fit_s1 <- tbats(train_ts)
autoplot(forecast(fit_s1), 92)
forecasts <- forecast(fit_s1, 87)$mean

test_data_s1$case_count = forecasts

#### Predict for segment 2 ####

train_data_s2 <- read_csv('data/train_segment_2_filled.csv')

ride_counts_by_data_s2 <- train_data_s2 %>% 
  group_by(application_date) %>% 
  summarise('count' = sum(case_count))

test_data_s2 <- test_data %>% filter(segment == 2)

train_ts_s2 <- msts(ride_counts_by_data_s2$count, seasonal.periods = c(30))
autoplot(train_ts_s2)
fit_s2 <- tbats(train_ts_s2)
autoplot(forecast(fit_s2), 93)
forecasts_s2 <- forecast(fit_s2, 93)$mean

test_data_s2$case_count = forecasts_s2

#### create preds file ####
test_data_s1$application_date <- as.character(test_data_s1$application_date)
test_data_s2$application_date <- as.character(test_data_s2$application_date)

test_data_s1$case_count <- as.numeric(test_data_s1$case_count)
test_data_s2$case_count <- as.numeric(test_data_s2$case_count)

preds <- rbind(test_data_s1, test_data_s2)

write_csv(preds, "data/predictions/tbats_7_30_s1_30_s2_model_no_eda_for_segment_2.csv")
