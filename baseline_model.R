source('./utils.R')

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
summary(ride_counts_by_date$count)

ride_counts_by_date_outliers_filtered <- ride_counts_by_date %>% 
  select(count) %>% 
  filter(count < quantile(ride_counts_by_date$count, probs=0.95) & count > quantile(ride_counts_by_date$count, probs=0.05))

mean_count_by_date <- mean(ride_counts_by_date_outliers_filtered$count)

validation_ride_counts_by_date <- validation_data %>% 
  group_by(application_date) %>% 
  summarise(actual_count = sum(case_count))

validation_ride_counts_by_date$predicted_count_mean = mean_count_by_date

validation_ride_counts_by_date <- validation_ride_counts_by_date %>% 
  mutate('ape_mean' = (abs(actual_count - predicted_count_mean) / actual_count) * 100)

validation_data_wo_outliers <- validation_ride_counts_by_date %>% filter(ape_mean < 200)

summary(validation_data_wo_outliers$ape_mean)
mape_mean_model <- mean(validation_data_wo_outliers$ape_mean)  

## Calculate MAPE for validation set for median model ##
median_count_by_date <- median(ride_counts_by_date$count)

validation_ride_counts_by_date$predicted_count_median = median_count_by_date

validation_ride_counts_by_date <- validation_ride_counts_by_date %>% 
  mutate('ape_median' = (abs(actual_count - predicted_count_median) / actual_count) * 100)

mape_median_model <- mean(validation_ride_counts_by_date$ape_median)  

visualise_model_ape_comparison(validation_ride_counts_by_date$ape_median, validation_ride_counts_by_date$ape_mean)

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

## Predict for submission on mean model ##
test_data <- read_csv('data/test.csv')

# For segment 1
segment1 <- read_csv('data/train_segment_1_filled.csv')
test_data_segment1 <- test_data %>% filter(segment == '1')
summary(segment1$case_count)

segment1_counts_by_date <- segment1 %>% group_by(application_date) %>% summarise('count' = sum(case_count))
summary(segment1_counts_by_date$count)

higher_outlier_threshold <- quantile(segment1_counts_by_date$count, probs=0.5)
lower_outlier_threshold <- quantile(segment1_counts_by_date$count, probs=0.05)

segment1_counts_outliers_filtered <- segment1_counts_by_date %>% 
  select(count) %>% 
  filter(count < higher_outlier_threshold & count > lower_outlier_threshold)

mean_segment_1 <- mean(segment1_counts_outliers_filtered$count)

test_data_segment1$case_count = mean_segment_1


# For segment 2
test_data_segment2 <- test_data %>% filter(segment == '2')
summary(segment1$case_count)

train_data_segment2 <- read_csv('data/train_segment_2_filled.csv')

segment2_counts_by_date <- train_data_segment2 %>% group_by(application_date) %>% summarise('count' = sum(case_count))
summary(segment2_counts_by_date$count)

higher_outlier_threshold <- quantile(segment2_counts_by_date$count, probs=0.5)
lower_outlier_threshold <- quantile(segment2_counts_by_date$count, probs=0.05)

segment2_counts_outliers_filtered <- segment2_counts_by_date %>% 
  select(count) %>% 
  filter(count < higher_outlier_threshold & count > lower_outlier_threshold)

mean_segment_2 <- mean(segment2_counts_outliers_filtered$count)

test_data_segment2$case_count = mean_segment_2

test_data_predicted <- rbind(test_data_segment1, test_data_segment2)

write_csv(test_data_predicted, 'data/predictions/baseline_model_mean.csv')
