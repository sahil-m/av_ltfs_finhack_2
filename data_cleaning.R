train_data <- read_csv('data/train.csv')

summary(train_data)

table(train_data$segment)

###### Divide train data into segment 1 and segment 2 ####
segment_1_train_data <- train_data %>% filter(segment == 1)
segment_2_train_data <- train_data %>% filter(segment == 2) %>% select(-c('branch_id'))

write_csv(segment_1_train_data, 'data/train_segment_1.csv')
write_csv(segment_2_train_data, 'data/train_segment_2.csv')
