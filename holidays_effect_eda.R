holidays <- read_csv('data/holidays.csv')

#### Segment 2 ####
train_csv <- read_csv('data/train_segment_2_filled.csv')

ride_counts_by_day_state <- train_csv %>% 
  group_by(application_date, state) %>% 
  summarise('case_count' = sum(case_count))

ggplotly(ggplot(ride_counts_by_day_state %>% filter(state=='WEST BENGAL'), aes(x=application_date, y=case_count)) +
  geom_line())
