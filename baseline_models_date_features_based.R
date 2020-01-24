#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')

segment2_date_level <- read_csv('data/segment2_date_level.csv')

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

train_mean_count <- mean(train_data$case_count)

validation_error_df <- validation_data %>% 
  select(application_date, actual = case_count, pred_dom_median = case_count_median) %>% 
  mutate(pred_overall_mean = train_mean_count) %>% 
  mutate(error_dom_median = actual - pred_dom_median,
         error_dom_median_percent = round(error_dom_median/actual*100, 2),
         error_dom_median_percent_abs = abs(error_dom_median_percent),
         error_overall_mean = actual - pred_overall_mean,
         error_overall_mean_percent = round(error_overall_mean/actual*100, 2),
         error_overall_mean_percent_abs = abs(error_overall_mean_percent))

ggplotly(
  validation_error_df %>% 
    select(application_date, contains("_percent_abs")) %>% 
    pivot_longer(cols = contains("_percent_abs"), names_to = "model", values_to = "ape") %>% 
    ggplot(aes(x = ape)) +
    geom_density(aes(color = model, fill = model), 
                 alpha = 0.3)
)

quantile(validation_error_df$error_dom_median_percent_abs, probs = seq(0, 1, .1))
quantile(validation_error_df$error_overall_mean_percent_abs, probs = seq(0, 1, .1))
