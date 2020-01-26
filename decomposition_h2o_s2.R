source('./utils.R')
library('stR')
library('h2o')

set.seed(123L)

#### Read date level data ####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

#### just 2019 - STR decomposition ####
# s2_train_msts %>%
#   AutoSTR() %>%
#   plot()

s2_train_str_object <- AutoSTR(s2_train_msts, robust = FALSE)

# s1_train_msts_2019 <- msts(splitTrainTest(s1_train_msts, which(s1_train_data$application_date == ymd("2019-01-01")) - 1)$test, seasonal.periods = c(7, 30))
#
# s1_train_msts_2019 %>%
#   AutoSTR(robust = FALSE) %>%
#   plot()
#
# s1_train_2019_str_object <- AutoSTR(s1_train_msts_2019, robust = FALSE)
#
# s1_train_data_2019 <- s1_train_data %>%
#   dplyr::filter(application_date >= ymd('2019-01-01'))

# y_limit <- 10000
# ggplotly(
#   s1_train_data_2019 %>%
#     ggplot(aes(x = application_date, y = case_count)) +
#     geom_line() +
#     # geom_vline(xintercept= as.numeric(holidays$date), linetype=4, color='red') +
#     geom_point(aes(size = I(.5))) +
#     geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
#     # stat_smooth(method = "loess", aes(color = branch_id, fill = branch_id)) +
#     geom_line(data = data.frame(application_date = s1_train_data_2019$application_date, predicted = s1_train_str_object$output$forecast$data), aes(x = application_date, y = predicted, color = 'red')) +
#     scale_y_continuous(limits = c(0, y_limit)) +
#     scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month") +
#     theme(axis.text.x = element_text(angle=45, hjust = 1))
# )

# error_df <- getAPE(s1_train_data$case_count, s2_train_str_object$output$forecast$data)
# summary(error_df$ape)


####  create features using STR decomposition
s2_str_features <- s2_train_data %>%
  dplyr::select(
    application_date,
    case_count,
    day_of_week,
    is_end_of_month,
    is_weekend,
    week_of_month,
    day_of_month,
    year
  ) %>%
  mutate(
    trend = s2_train_str_object$output$predictors[[1]]$data,
    season_7 = s2_train_str_object$output$predictors[[2]]$data,
    season_30 = s2_train_str_object$output$predictors[[3]]$data
  ) %>%
  bind_rows(
    dplyr::select(
      s2_validation_data,
      application_date,
      case_count,
      day_of_week,
      is_end_of_month,
      is_weekend,
      week_of_month,
      day_of_month,
      year
    )
  ) %>%
  group_by(year) %>%
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, day_of_week) %>%
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, day_of_month) %>%
  mutate(season_30_mean_by_wday = mean(season_30, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-trend,-season_7,-season_30,-year, -season_7_mean_by_wday) %>%
  mutate(
    'week_of_month_2' = as.factor(ceiling(day_of_month / 7)),
    'day_of_month' = as.factor(day_of_month),
    'is_sunday' = day_of_week == 'Sun'
    
  ) %>%
  select(-c(day_of_week, week_of_month))

# s2_str_features <- s2_train_data %>%
#   bind_rows(s2_validation_data) %>%
#   mutate('is_sunday' = (day_of_week == 'Sun'),
#          'week_of_month_2' = as.factor(ceiling(day_of_month / 7)),
#          'day_of_month' = as.factor(day_of_month),
#          'quarter' = as.factor(quarter)
#          ) %>%
#   select(application_date, case_count, day_of_month, is_sunday, week_of_month_2, quarter)

########## model using h2o ##########
#### split
validaton_s2_start_date <- ymd("2019-04-23")

train_s2_for_model <- s2_str_features %>%
  filter(application_date < validaton_s2_start_date) %>%
  select_if( ~ !is.Date(.))

validation_s2_for_model <- s2_str_features %>%
  filter(application_date >= validaton_s2_start_date) %>%
  select_if( ~ !is.Date(.))

#### h20
h2o.init()

train_h2o <- as.h2o(train_s2_for_model)
valid_h2o <- as.h2o(validation_s2_for_model)

y <- "case_count"
x <- setdiff(names(train_h2o), c(y, 'index.num', 'label', ''))

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  # leaderboard_frame = test_h2o,
  exclude_algos = c('StackedEnsemble', 'DeepLearning'),
  stopping_metric = "RMSE",
  seed = 123
)
lb <- h2o.get_leaderboard(automl_models_h2o, extra_columns = "ALL")
print(lb, n = nrow(lb))
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)

predictions <- pred_h2o %>% as_tibble() %>% pull(predict)
errors_df <- getAPE(validation_s2_for_model$case_count, predictions)

validation_s2_for_model$predictions <- predictions
validation_s2_for_model$ape <- errors_df$ape

summary(errors_df$ape)

#### Generate test predictions ####
source('./utils.R')
library('stR')
library('h2o')

set.seed(123L)

#### Read date level data ####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

s2_test <- read_csv('data/test.csv') %>% 
  filter(segment == 2)

segment_2_date_level <- read_csv('data/segment2_date_level.csv')
test_data_s2 <- read_csv('data/test.csv') %>% filter(segment == 2)
test_data_s2 <- add_date_based_features(test_data_s2)

test_data_s2 <- test_data_s2 %>% 
  select(application_date, is_end_of_month, is_weekend, day_of_month, day_of_week, year) %>% 
  mutate('day_of_week' = as.character(day_of_week),
         'week_of_month_2' = as.factor(ceiling(day_of_month / 7)),
         'is_sunday' = day_of_week == 'Sun')

##### feature prep.
s2_str_object <- AutoSTR(segment2_date_level_msts, robust = FALSE)

s2_str_features <- segment_2_date_level %>% 
  dplyr::select(application_date,
                case_count,
                is_end_of_month,
                is_weekend,
                day_of_month,
                day_of_week,
                year) %>% 
  mutate('week_of_month_2' = as.factor(ceiling(day_of_month / 7)),
         'is_sunday' = day_of_week == 'Sun') %>% 
  mutate(trend = s2_str_object$output$predictors[[1]]$data,
         season_7 = s2_str_object$output$predictors[[2]]$data,
         season_30 = s2_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(test_data_s2, application_date, day_of_week, is_end_of_month, is_weekend, week_of_month_2, is_sunday, day_of_month, year)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_mday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-trend, -season_7, -season_7_mean_by_wday, -season_30, -day_of_week, -year)

validaton_s2_start_date <- ymd("2019-04-23")
test_s2_start_date <- min(test_data_s2$application_date)

train_plus_valid_s2_for_model <- s2_str_features %>% 
  filter(application_date < test_s2_start_date) %>% 
  left_join(select(segment_2_date_level, application_date, case_count)) %>% 
  select_if(~ !is.Date(.)) 

test_s2_for_model <- s2_str_features %>% 
  filter(application_date >= test_s2_start_date) %>% 
  select_if(~ !is.Date(.)) %>% 
  select(-case_count)

validation_s2_for_model <- s2_str_features %>% 
  filter((application_date >= validaton_s2_start_date) & (application_date < test_s2_start_date)) %>% 
  left_join(select(s2_validation_data, application_date, case_count)) %>% 
  select_if(~ !is.Date(.)) 

#### h20
h2o.init()

train_plus_valid_h2o <- as.h2o(train_plus_valid_s2_for_model)
valid_h2o <- as.h2o(validation_s2_for_model)
test_h2o  <- as.h2o(test_s2_for_model)

y <- "case_count"
x <- setdiff(names(train_plus_valid_h2o), c(y, 'index.num', 'label'))

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_plus_valid_h2o, 
  max_runtime_secs = 0,
  exclude_algos = c("DeepLearning", "StackedEnsemble"),
  seed = 123)

automl_leader <- automl_models_h2o@leader

h2o.varimp(automl_leader)

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)
errors_df <- getAPE(validation_s2_for_model$case_count, pred_h2o %>% as_tibble() %>% pull(predict))
summary(errors_df$ape)

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

test_submission <- s2_test %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

pred_on_train_s2_object %>%
  autoplot() +
  geom_line(data = s2_validation_msts,
            aes(
              x = as.numeric(time(s2_validation_msts)),
              y = as.numeric(s2_validation_msts)
            ),
            col = "red"
  )

write_csv(test_submission, 'data/predictions/segment_2_h2o.csv')


