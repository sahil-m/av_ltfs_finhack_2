# segment 1 ---------------------------------------------------------------
rm_all_except_functions()

##### config #####
version = "v4"
segment = "s1"

##### read data #####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

holidays_augmented <- read_csv("data/holidays_augmented.csv")

holidays_cleaned <- read_csv("data/holidays_cleaned.csv")

segment1_date_level <- read_csv('data/segment1_date_level.csv')

otest <- read_csv('data/test.csv')
s1_test <- otest %>% 
  dplyr::filter(segment == 1)
s1_test_data <- s1_test %>% 
  add_date_based_features()

##### feature prep. #####
prophet_predictions_df <- create_prophet_prediction_feature(segment1_date_level, holidays_cleaned, s1_test_data, nrow(s1_test_data))

s1_str_object <- AutoSTR(segment1_date_level_msts, robust = TRUE)

s1_str_features <- segment1_date_level %>% 
  dplyr::select(application_date, case_count, day_of_week, is_end_of_month, is_weekend, week_of_month_plain, day_of_month, year, part_of_month) %>% 
  mutate(trend = s1_str_object$output$predictors[[1]]$data,
         season_7 = s1_str_object$output$predictors[[2]]$data,
         season_30 = s1_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(s1_test_data, application_date, day_of_week, is_end_of_month, is_weekend, week_of_month_plain, day_of_month, year, part_of_month)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_mday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(holidays_augmented) %>% 
  left_join(prophet_predictions_df) %>% 
  dplyr::select(-day_of_month, -trend, -season_7, -season_30, -is_other_holidays, -is_diwali_month, -is_weekend, -day_of_week, -part_of_month, -is_oct_2, -is_first_day_of_year)

#### split #####
validaton_s1_start_date <- date("2019-04-05")
test_s1_start_date <- min(s1_test_data$application_date)

train_plus_valid_s1_for_model <- s1_str_features %>% 
  filter(application_date < test_s1_start_date) %>% 
  left_join(select(segment1_date_level, application_date, case_count)) %>% 
  select_if(~ !is.Date(.)) 

test_s1_for_model <- s1_str_features %>% 
  filter(application_date >= test_s1_start_date) %>% 
  select_if(~ !is.Date(.))

validation_s1_for_model <- s1_str_features %>% 
  filter((application_date >= validaton_s1_start_date) & (application_date < test_s1_start_date)) %>% 
  left_join(select(s1_validation_data, application_date, case_count)) %>% 
  select_if(~ !is.Date(.)) 

#### train #####
# h2o.init()

train_plus_valid_h2o <- as.h2o(train_plus_valid_s1_for_model)
valid_h2o <- as.h2o(validation_s1_for_model)
test_h2o  <- as.h2o(test_s1_for_model)

y <- "case_count"
x <- setdiff(names(train_plus_valid_h2o), c(y, 'index.num', 'label'))
# x <- c("is_end_of_month", "season_30_mean_by_mday", "season_7_mean_by_wday", "week_of_month", "is_diwali_week", "trend_mean", "is_diwali_month", "is_weekend")

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_plus_valid_h2o, 
  max_runtime_secs = 0,
  exclude_algos = c("DeepLearning", "StackedEnsemble"),
  seed = 123)

automl_leader <- automl_models_h2o@leader

h2o.varimp(automl_leader)

#### predict #####
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

test_submission <- s1_test %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

##### write #####
write_csv(test_submission, paste("data/pred_test", segment, "withProphet", version, "csv", sep = '.'))


# segment 2 ---------------------------------------------------------------
rm_all_except_functions()

##### config #####
version = "v4"
segment = "s2"

##### read data #####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

holidays_cleaned <- read_csv("data/holidays_cleaned.csv")

segment_2_date_level <- read_csv('data/segment2_date_level.csv')
s2_test <- read_csv('data/test.csv') %>% filter(segment == 2)
test_data_s2 <- add_date_based_features(s2_test)

##### feature prep. #####
prophet_predictions_df <- create_prophet_prediction_feature(segment_2_date_level, holidays_cleaned, test_data_s2, nrow(test_data_s2))

s2_str_object <- AutoSTR(segment2_date_level_msts, robust = TRUE)

s2_str_features <- segment_2_date_level %>% 
  dplyr::select(application_date,
                case_count,
                is_end_of_month,
                is_weekend,
                day_of_month,
                day_of_week,
                year,
                week_of_month_plain,
                is_sunday) %>% 
  mutate(trend = s2_str_object$output$predictors[[1]]$data,
         season_7 = s2_str_object$output$predictors[[2]]$data,
         season_30 = s2_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(test_data_s2,
                          application_date,
                          is_end_of_month,
                          is_weekend,
                          day_of_month,
                          day_of_week,
                          year,
                          week_of_month_plain,
                          is_sunday)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_mday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(prophet_predictions_df) %>% 
  select(-trend, -season_7, -season_7_mean_by_wday, -season_30, -day_of_week, -year, -week_of_month_plain, -is_weekend, -is_end_of_month)

##### split #####
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

#### train #####
# h2o.init()

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

##### predict #####
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

test_submission <- s2_test %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

##### write #####
write_csv(test_submission, paste("data/pred_test", segment, "withProphet", version, "csv", sep = '.'))


# create final submission -------------------------------------------------
rm_all_except_functions()

version = "v4"

s1_file_path <- paste("data/pred_test", "s1", "withProphet", version, "csv", sep = '.')
s2_file_path <- paste("data/pred_test", "s2", "withProphet", version, "csv", sep = '.')

segment_1_preds <- read_csv(s1_file_path)
segment_2_preds <- read_csv(s2_file_path)

final_submission <- combine_segment_1_and_segment_2_predictions(segment_1_preds, segment_2_preds)


write_csv(final_submission, paste("data/submission_withProphet", version, "csv", sep = '.'))

# analysis ----------------------------------------------------------------
test_submission = segment_1_preds
actuals <- read_csv('data/segment1_date_level.csv')

test_pred_augmented <- test_submission %>% 
  add_date_based_features() %>% 
  mutate(split = "test") %>% 
  select(-id, -segment)

train_plus_valid_actual_augmented <- actuals %>% 
  mutate(split = "train_plus_valid")

all_with_test_pred <- train_plus_valid_actual_augmented %>% 
  bind_rows(test_pred_augmented)

y_limit = 10000
ggplotly(
  all_with_test_pred %>%
    ggplot(aes(x = day_of_year, y = case_count, label = label)) +
    geom_line() +
    geom_point(aes(size = I(.5), color = is_end_of_month)) +
    geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
    geom_vline(xintercept = min(test_pred_augmented$day_of_year), size = 0.5, color = "red") +
    facet_grid(year ~ .) +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_x_continuous(breaks = c(1, 365, 1)) +
    theme_bw(),
  tooltip = c('label', 'y')
)

