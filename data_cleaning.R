source('utils.R')
train_data <- read_csv('data/train.csv')

###### Divide train data into segment 1 and segment 2 ####
segment_1_train_data <- train_data %>% filter(segment == 1)
segment_2_train_data <- train_data %>% filter(segment == 2) %>% select(-c('branch_id', 'zone'))

#### Fill missing dates for segment 1 ####
segment1_filled <- segment_1_train_data %>%
  select(-c(segment)) %>% 
  complete(application_date = seq.Date(min(segment_1_train_data$application_date), max(segment_1_train_data$application_date), by="day"), nesting(zone, state, branch_id), fill = list(case_count = 0)) 

check_if_date_is_continuous(segment1_filled)

write_csv(segment1_filled, 'data/train_segment_1_filled.csv')

#### Fill missing dates for segment 1 ####
segment2_filled <- segment_2_train_data %>%
  select(-c(segment)) %>% 
  complete(application_date = seq.Date(min(segment_2_train_data$application_date), max(segment_2_train_data$application_date), by="day"), nesting(state), fill = list(case_count = 0)) 

check_if_date_is_continuous(segment2_filled)

write_csv(segment2_filled, 'data/train_segment_2_filled.csv')

#### Clean holidays data ####
holidays <- read_csv('data/holidays.csv') %>% rename('date' = 'Date')

holidays <- holidays %>% 
  mutate('date_formatted' = as.Date(date, format='%b %d, %Y')) %>% 
  select(-c(date)) %>% 
  rename('date' = 'date_formatted')

write_csv(holidays, 'data/holidays_cleaned.csv')  

