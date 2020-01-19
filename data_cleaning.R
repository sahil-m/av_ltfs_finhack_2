train_data <- read_csv('data/train.csv')

###### Divide train data into segment 1 and segment 2 ####
segment_1_train_data <- train_data %>% filter(segment == 1)
segment_2_train_data <- train_data %>% filter(segment == 2) %>% select(-c('branch_id', 'zone'))

#### Fill missing dates for segment 1 ####
segment1_filled <- segment_1_train_data %>%
  select(-c(segment)) %>% 
  complete(application_date = seq.Date(min(segment_1_train_data$application_date), max(segment_1_train_data$application_date), by="day"), nesting(zone, state, branch_id)) %>%
  group_by(zone, state, branch_id) %>%
  fill(`case_count`)

write_csv(segment1_filled, 'data/train_segment_1_filled.csv')


#### Fill missing dates for segment 1 ####
segment2_filled <- segment_2_train_data %>%
  select(-c(segment)) %>% 
  complete(application_date = seq.Date(min(segment_1_train_data$application_date), max(segment_1_train_data$application_date), by="day"), state) %>%
  group_by(state) %>%
  fill(`case_count`)

write_csv(segment2_filled, 'data/train_segment_2_filled.csv')


