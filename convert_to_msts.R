#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')
segment2_date_level <- read_csv('data/segment2_date_level.csv')

load('data/segment_wise_date_level_splitted_data.Rdata')

#### convert to msts object
# s1_train_data[which.min(s1_train_data$application_date),]$day_of_year
# s1_validation_data[which.min(s1_validation_data$application_date),]$day_of_year
# s2_train_data[which.min(s2_train_data$application_date),]$day_of_year
# s2_validation_data[which.min(s2_validation_data$application_date),]$day_of_year

segment1_date_level_msts <- msts(segment1_date_level$case_count, seasonal.periods = c(7, 30), start = c(2017, 91))

segment2_date_level_msts <- msts(segment2_date_level$case_count, seasonal.periods = c(7, 30), start = c(2017, 91))

s1_train_msts <- msts(s1_train_data$case_count, seasonal.periods = c(7, 30), start = c(2017, 91))

s1_validation_msts <- msts(s1_validation_data$case_count, seasonal.periods = c(7, 30), start = c(2019, 95))

s2_train_msts <- msts(s2_train_data$case_count, seasonal.periods = c(7, 30), start = c(2017, 91))

s2_validation_msts <- msts(s2_validation_data$case_count, seasonal.periods = c(7, 30), start = c(2019, 113))

#### decomposition visualizatuion
segment1_date_level_msts %>% 
  mstl() %>%
  autoplot() + 
  xlab("Days")

segment2_date_level_msts %>% 
  mstl() %>%
  autoplot() + 
  xlab("Days")

#### save
save(segment1_date_level_msts, segment2_date_level_msts, s1_train_msts, s1_validation_msts, s2_train_msts, s2_validation_msts, file = 'data/segment_wise_date_level_msts_objects.Rdata')
