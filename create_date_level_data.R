#### Read continuous data
segment1 <- read_csv('data/train_segment_1_filled.csv')

segment2 <- read_csv('data/train_segment_2_filled.csv')

#### aggregating by date
segment1_date_level <- segment1 %>% 
  group_by(application_date) %>% 
  summarise(case_count = sum(case_count)) 
check_if_date_is_continuous(segment1_date_level)

segment2_date_level <- segment2 %>% 
  group_by(application_date) %>% 
  summarise(case_count = sum(case_count))
check_if_date_is_continuous(segment2_date_level)

#### create date related featurtes
segment1_date_level <- add_date_based_features(segment1_date_level)
check_if_date_is_continuous(segment1_date_level)

segment2_date_level <- add_date_based_features(segment2_date_level)
check_if_date_is_continuous(segment2_date_level)

##### write date level data
write_csv(segment1_date_level, 'data/segment1_date_level.csv')

write_csv(segment2_date_level, 'data/segment2_date_level.csv')
