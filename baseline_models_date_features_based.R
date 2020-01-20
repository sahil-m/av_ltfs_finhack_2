#### Read data for segment 1
segment1 <- read_csv('data/train_segment_1_filled.csv')

#### data prep. for submission
segment1_date_level <- segment1 %>% 
  group_by(application_date) %>% 
  summarise(case_count = sum(case_count))

#### create date related featurtes
segment1_date_level <- segment1_date_level %>% 
  mutate(day_of_month = mday(application_date),
         day_of_week = wday(application_date, label = TRUE, abbr = TRUE),
         is_end_of_month = (application_date == (ceiling_date(application_date, unit = "months") - days(1))),
         part_of_month = cut(day_of_month, c(0,2,29,31), c('start', 'mid', 'end')),
         month = month(application_date, label = TRUE, abbr = TRUE),
         year = year(application_date),
         year_month = paste(year, month, sep = "_"))

#### split
train_data <- segment1_date_level %>% 
  filter(application_date < date("2019-04-05"))

validation_data <- segment1_date_level %>% 
  filter(application_date >= date("2019-04-05"))


#### base models
train_centralized_count_by_day_of_month <- train_data %>% 
  group_by(day_of_month) %>% 
  summarise(case_count_mean = mean(case_count),
            case_count_median = median(case_count))
  
train_centralized_count_by_day_of_week <- train_data %>% 
  group_by(day_of_week) %>% 
  summarise(case_count_mean = mean(case_count),
            case_count_median = median(case_count))

train_centralized_count_by_day_of_month_and_week<- train_data %>% 
  group_by(part_of_month, day_of_week) %>% 
  summarise(case_count_mean = mean(case_count),
            case_count_median = median(case_count))

validation_data <- validation_data %>% 
  left_join(train_centralized_count_by_day_of_month, by = "day_of_month", suffix = c("", "_dom")) %>% 
  left_join(train_centralized_count_by_day_of_week, by = "day_of_week", suffix = c("", "_dow")) %>% 
  left_join(train_centralized_count_by_day_of_month_and_week, by = c("part_of_month", "day_of_week"), suffix = c("", "_pom_dow"))

sapply(validation_data, function(x) sum(is.na(x)))

#### evaluation
Metrics::mape(validation_data$case_count, validation_data$case_count_mean)
Metrics::mape(validation_data$case_count, validation_data$case_count_median)

Metrics::mape(validation_data$case_count, validation_data$case_count_mean_dow)
Metrics::mape(validation_data$case_count, validation_data$case_count_median_dow)

Metrics::mape(validation_data$case_count, validation_data$case_count_mean_pom_dow)
Metrics::mape(validation_data$case_count, validation_data$case_count_median_pom_dow)
