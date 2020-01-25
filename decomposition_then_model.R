#### Read date level data
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

#### just 2019 - STR decomposition
# s1_train_msts %>% 
#   AutoSTR() %>%
#   plot()

s1_train_str_object <- AutoSTR(s1_train_msts, robust = FALSE)

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

error_df <- getAPE(s1_train_data$case_count, s1_train_str_object$output$forecast$data)
summary(error_df$ape)


####  create features using STR decomposition
s1_str_features <- s1_train_data %>% 
  dplyr::select(application_date, case_count, day_of_week, is_end_of_month, is_weekend, week_of_month, day_of_month, year) %>% 
  mutate(trend = s1_train_str_object$output$predictors[[1]]$data,
         season_7 = s1_train_str_object$output$predictors[[2]]$data,
         season_30 = s1_train_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(s1_validation_data, application_date, case_count, day_of_week, is_end_of_month, is_weekend, week_of_month, day_of_month, year)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_wday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-day_of_month, -trend, -season_7, -season_30, -year)

########## model using h2o ########## 
#### split
validaton_s1_start_date <- date("2019-04-05")

train_s1_for_model <- s1_str_features %>% 
  filter(application_date < validaton_s1_start_date) %>% 
  select_if(~ !is.Date(.)) 

validation_s1_for_model <- s1_str_features %>% 
  filter(application_date >= validaton_s1_start_date) %>% 
  select_if(~ !is.Date(.)) 

#### h20
h2o.init()

train_h2o <- as.h2o(train_s1_for_model)
valid_h2o <- as.h2o(validation_s1_for_model)

y <- "case_count"
x <- setdiff(names(train_h2o), c(y, 'index.num', 'label', ''))

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  # leaderboard_frame = test_h2o, 
  max_runtime_secs = 5*60, 
  stopping_metric = "RMSE")

automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)

errors_df <- getAPE(validation_s1_for_model$case_count, pred_h2o %>% as_tibble() %>% pull(predict))

summary(errors_df$ape)
