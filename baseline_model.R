#### Baseline model for segment 1 ####
segment1 <- read_csv('data/train_segment_1.csv')


train_data <- segment1 %>% 
  filter(application_date < date("2019-04-05"))

validation_data <- segment1 %>% 
  filter(application_date >= date("2019-04-05"))

#### Fill missing dates ####

