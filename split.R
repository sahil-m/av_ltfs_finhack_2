#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')

segment2_date_level <- read_csv('data/segment2_date_level.csv')

load('data/segment_wise_date_level_msts_objects.Rdata')

#### split
s1_validation_start_date <- ymd("2019-04-05")

s1_train_data <- segment1_date_level %>% 
  filter(application_date < s1_validation_start_date)

s1_validation_data <- segment1_date_level %>% 
  filter(application_date >= s1_validation_start_date)

ls_s1_splittted <- splitTrainTest(segment1_date_level_msts, nrow(s1_train_data))

s1_train_msts <- msts(ls_s1_splittted$train, seasonal.periods = c(7, 30))

s1_validation_msts <- msts(ls_s1_splittted$test, seasonal.periods = c(7, 30))


s2_validation_start_date <- ymd("2019-04-23")

s2_train_data <- segment2_date_level %>% 
  filter(application_date < s2_validation_start_date)

s2_validation_data <- segment2_date_level %>% 
  filter(application_date >= s2_validation_start_date)

ls_s2_splittted <- splitTrainTest(segment2_date_level_msts, 752)

s2_train_msts <- msts(ls_s2_splittted$train, seasonal.periods = c(7, 30))

s2_validation_msts <- msts(ls_s2_splittted$test, seasonal.periods = c(7, 30))


#### save
save(s1_train_data, s1_validation_data, s1_train_msts, s1_validation_msts, s2_train_data, s2_validation_data, s2_train_msts, s2_validation_msts, file = 'data/segment_wise_date_level_splitted_objects.Rdata')

