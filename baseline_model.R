#### Read data for segment 1
segment1 <- read_csv('data/train_segment_1_filled.csv')

train_data <- segment1 %>% 
  filter(application_date < date("2019-04-05"))

validation_data <- segment1 %>% 
  filter(application_date >= date("2019-04-05"))

#### Universal model ####
ride_counts_by_date <- train_data %>% 
  group_by(application_date) %>% 
  summarise(count = sum(case_count))

## Calculate MAPE for validation set for mean model ##
mean_count_by_date <- mean(ride_counts_by_date$count)

validation_ride_counts_by_date <- validation_data %>% 
  group_by(application_date) %>% 
  summarise(actual_count = sum(case_count))

validation_ride_counts_by_date$predicted_count_mean = mean_count_by_date

validation_ride_counts_by_date <- validation_ride_counts_by_date %>% 
  mutate('ape_mean' = (abs(actual_count - predicted_count_mean) / actual_count) * 100)

mape_mean_model <- mean(validation_ride_counts_by_date$ape_mean)  

## Calculate MAPE for validation set for median model ##
median_count_by_date <- median(ride_counts_by_date$count)

validation_ride_counts_by_date$predicted_count_median = median_count_by_date

validation_ride_counts_by_date <- validation_ride_counts_by_date %>% 
  mutate('ape_median' = (abs(actual_count - predicted_count_median) / actual_count) * 100)

mape_median_model <- mean(validation_ride_counts_by_date$ape_median)  



#### Zone level model ####
zones = list('CENTRAL', 'EAST', 'WEST', 'NORTH', 'WEST')

ride_counts_by_date_zone <- train_data %>% 
  group_by(application_date, zone) %>% 
  summarise(count = sum(case_count))

## Mean model ##
median_ride_counts_by_zone <- ride_counts_by_date_zone %>% 
  group_by(zone) %>% 
  summarise('median_count' = median(count))

median_count_by_zone <- sum(median_ride_counts_by_zone$median_count)

validation_ride_counts_by_date$predicted_count_median_zone = median_count_by_zone
validation_ride_counts_by_date <- validation_ride_counts_by_date %>% 
  mutate('ape_median_zone' = (abs(actual_count - predicted_count_median_zone) / actual_count) * 100)

mape_median_zone = mean(validation_ride_counts_by_date$ape_median_zone)
