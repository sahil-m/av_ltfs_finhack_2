#### Read date level data ####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

holidays_cleaned <- read_csv("data/holidays_cleaned.csv")

#### just 2019 - STR decomposition ####
s2_train_str_object <- AutoSTR(s2_train_msts, robust = FALSE)


####  create prophet features
prophet_predictions_df <- create_prophet_prediction_feature(s2_train_data, holidays_cleaned, s2_validation_data, nrow(s2_validation_data))

####  create features using STR decomposition
s2_str_features <- s2_train_data %>%
  dplyr::select(
    application_date,
    case_count,
    is_end_of_month,
    is_weekend,
    day_of_month,
    day_of_week,
    year,
    week_of_month_plain,
    is_sunday
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
      is_end_of_month,
      is_weekend,
      day_of_month,
      day_of_week,
      year,
      week_of_month_plain,
      is_sunday
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
  dplyr::select(-trend,-season_7,-season_30,-year, -season_7_mean_by_wday, -day_of_week) %>%
  left_join(prophet_predictions_df)

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
# h2o.init()

train_h2o <- as.h2o(train_s2_for_model)
valid_h2o <- as.h2o(validation_s2_for_model)

y <- "case_count"
x <- setdiff(names(train_h2o), c(y, 'index.num', 'label'))

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  max_runtime_secs = 0,
  exclude_algos = c('StackedEnsemble', 'DeepLearning'),
  stopping_metric = "RMSE",
  seed = 123
)

# lb <- h2o.get_leaderboard(automl_models_h2o, extra_columns = "ALL")
# print(lb, n = nrow(lb))
automl_leader <- automl_models_h2o@leader

h2o.varimp(automl_leader)

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)

predictions <- pred_h2o %>% as_tibble() %>% pull(predict)

# # taking mean with prophet predictions
# predictions_h2o <- pred_h2o %>% as_tibble() %>% pull(predict)
# 
# predictions_prophet <- dplyr::filter(prophet_predictions_df, application_date >= validaton_s2_start_date)$prophet_predictions
# 
# predictions <- (predictions_h2o + predictions_prophet)/2

# evaluating
errors_df <- getAPE(validation_s2_for_model$case_count, predictions)

summary(errors_df$ape)
summary(dplyr::filter(errors_df, ape < 1000)$ape)

#### Generate test predictions ####
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

holidays_cleaned <- read_csv("data/holidays_cleaned.csv")

segment_2_date_level <- read_csv('data/segment2_date_level.csv')
s2_test <- read_csv('data/test.csv') %>% filter(segment == 2)
test_data_s2 <- add_date_based_features(s2_test)

##### feature prep.
prophet_predictions_df <- create_prophet_prediction_feature(segment_2_date_level, holidays_cleaned, test_data_s2, nrow(test_data_s2))

s2_str_object <- AutoSTR(segment2_date_level_msts, robust = FALSE)

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

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)
errors_df <- getAPE(validation_s2_for_model$case_count, pred_h2o %>% as_tibble() %>% pull(predict))
summary(errors_df$ape)
valid_pred_df <- s2_validation_data %>% 
  select(application_date) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

test_submission <- s2_test %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

#### analysis
test_pred_augmented <- test_submission %>% 
  add_date_based_features() %>% 
  mutate(split = "test") %>% 
  select(-id, -segment)

valid_pred_augmented <- valid_pred_df %>% 
  add_date_based_features() %>% 
  mutate(split = "valid")

train_plus_valid_actual_augmented <- segment_2_date_level %>% 
  mutate(split = "train_plus_valid")

all_with_test_pred <- train_plus_valid_actual_augmented %>% 
  bind_rows(test_pred_augmented)

y_limit = 50000
ggplotly(
  all_with_test_pred %>%
    ggplot(aes(x = day_of_year, y = case_count, label = label)) +
    geom_line() +
    geom_line(data = valid_pred_augmented, aes(x = day_of_year, y = case_count, color = "red")) +
    geom_point(aes(size = I(.5), color = is_end_of_month)) +
    geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
    # geom_vline(data=holidays, aes(xintercept = day_of_year, color=holidays$Occasion)) +
    geom_vline(xintercept = min(test_pred_augmented$day_of_year), size = 0.5, color = "red") +
    facet_grid(year ~ .) +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_x_continuous(breaks = c(1, 365, 1)) +
    theme_bw(),
  tooltip = c('label', 'y')
)

write_csv(test_submission, 'data/pred_test_s2_withProphet_v2.csv')


