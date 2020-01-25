#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')

segment2_date_level <- read_csv('data/segment2_date_level.csv')

#### split
s1_validation_start_date <- ymd("2019-04-05")
s1_train_data <- segment1_date_level %>% 
  filter(application_date < s1_validation_start_date)
s1_validation_data <- segment1_date_level %>% 
  filter(application_date >= s1_validation_start_date)

s2_validation_start_date <- ymd("2019-04-23")
s2_train_data <- segment2_date_level %>% 
  filter(application_date < s2_validation_start_date)

s2_validation_data <- segment2_date_level %>% 
  filter(application_date >= s2_validation_start_date)

#### save
save(s1_train_data, s1_validation_data, s2_train_data, s2_validation_data, file = 'data/segment_wise_date_level_splitted_data.Rdata')
