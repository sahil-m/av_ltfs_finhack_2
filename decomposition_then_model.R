#####  Read date level data #####  
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

# load('data/incorrect_fill.Rdata')

# holidays_augmented <- read_csv('data/holidays_cleaned.csv') %>%
#   mutate(application_date = date) %>%
#   select(-date, -Weekday) %>%
#   group_by(application_date) %>%
#   dplyr::filter(row_number()==1) %>%
#   right_join(data.frame(application_date = seq.Date(ymd("2017-01-01"), ymd("2019-12-31"), by="day"))) %>%
#   mutate(Occasion = if_else(is.na(Occasion), "Nothing", Occasion)) %>%
#   add_date_based_features() %>%
#   select(-contains("is_")) %>%
#   mutate(is_oct_2 =  (Occasion == "Mathatma Gandhi Jayanti"),
#          is_labor_day =  (Occasion == "Labor Day"),
#          is_other_holidays = !(Occasion %in% c("Mathatma Gandhi Jayanti", "Labor Day")),
#          is_first_day_of_year = (day_of_year == 1)
#          ) %>%
#   group_by(year) %>%
#   mutate(is_diwali_week = (week_of_year == week_of_year[Occasion == "Diwali / Deepavali"][1]),
#          is_diwali_month = (month == month[Occasion == "Diwali / Deepavali"][1])
#          ) %>%
#   ungroup() %>%
#   select(application_date, contains("is_"))
# 
# write_csv(holidays_augmented, "data/holidays_augmented.csv")

holidays_augmented <- read_csv("data/holidays_augmented.csv")


# View(select(holidays_augmented, application_date, Occasion, is_diwali_week))

#### STR decomposition #####  
# s1_train_msts %>%
#   AutoSTR() %>%
#   plot()

s1_train_str_object <- AutoSTR(s1_train_msts, robust = TRUE)

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

# error_df <- getAPE(s1_train_data$case_count, s1_train_str_object$output$forecast$data)
# summary(error_df$ape)


##### create features using STR decomposition #####  
s1_str_features <- s1_train_data %>% 
  dplyr::select(application_date, case_count, day_of_week, is_end_of_month, is_weekend, week_of_month_plain, day_of_month, year) %>% 
  mutate(trend = s1_train_str_object$output$predictors[[1]]$data,
         season_7 = s1_train_str_object$output$predictors[[2]]$data,
         season_30 = s1_train_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(s1_validation_data, application_date, case_count, day_of_week, is_end_of_month, is_weekend, week_of_month_plain, day_of_month, year)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_mday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-day_of_month, -trend, -season_7, -season_30) %>% 
  left_join(holidays_augmented)

# nrow(s1_str_features) - (nrow(s1_train_data) + nrow(s1_validation_data))

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
# h2o.init()

train_h2o <- as.h2o(train_s1_for_model)
valid_h2o <- as.h2o(validation_s1_for_model)

y <- "case_count"
x <- setdiff(names(train_h2o), c(y, 'index.num', 'label'))

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  # leaderboard_frame = valid_h2o,
  max_runtime_secs = 0,
  exclude_algos = c("DeepLearning", "StackedEnsemble"),
  seed = 123)

automl_leader <- automl_models_h2o@leader

h2o.varimp(automl_leader)

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)

errors_df <- getAPE(validation_s1_for_model$case_count, pred_h2o %>% as_tibble() %>% pull(predict))

# quantile(errors_df$ape, probs = seq(.1,1,.05))
summary(errors_df$ape)
summary(dplyr::filter(errors_df, ape < 1000)$ape)


########## predict on test ##########
##### read data
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')
holidays_augmented <- read_csv("data/holidays_augmented.csv")

segment1_date_level <- read_csv('data/segment1_date_level.csv')

otest <- read_csv('data/test.csv')
s1_test <- otest %>% 
  dplyr::filter(segment == 1)
s1_test_data <- s1_test %>% 
  add_date_based_features()

##### feature prep.
s1_str_object <- AutoSTR(segment1_date_level_msts, robust = FALSE)

s1_str_features <- segment1_date_level %>% 
  dplyr::select(application_date, day_of_week, is_end_of_month, is_weekend, week_of_month, day_of_month, year) %>% 
  mutate(trend = s1_str_object$output$predictors[[1]]$data,
         season_7 = s1_str_object$output$predictors[[2]]$data,
         season_30 = s1_str_object$output$predictors[[3]]$data) %>% 
  bind_rows(dplyr::select(s1_test_data, application_date, day_of_week, is_end_of_month, is_weekend, week_of_month, day_of_month, year)) %>% 
  group_by(year) %>% 
  mutate(trend_mean = mean(trend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_week) %>% 
  mutate(season_7_mean_by_wday = mean(season_7, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, day_of_month) %>% 
  mutate(season_30_mean_by_mday = mean(season_30, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-day_of_month, -trend, -season_7, -season_30) %>% 
  left_join(holidays_augmented)

#### split
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

# lapply(list(train_plus_valid_s1_for_model, validation_s1_for_model, test_s1_for_model), count_nas)

#### train
# h2o.init()

train_plus_valid_h2o <- as.h2o(train_plus_valid_s1_for_model)
valid_h2o <- as.h2o(validation_s1_for_model)
test_h2o  <- as.h2o(test_s1_for_model)

y <- "case_count"
# x <- setdiff(names(train_plus_valid_h2o), c(y, 'index.num', 'label'))
x <- c("is_end_of_month", "season_30_mean_by_mday", "season_7_mean_by_wday", "week_of_month", "is_diwali_week", "trend_mean", "is_diwali_month", "is_weekend")

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_plus_valid_h2o, 
  max_runtime_secs = 0,
  exclude_algos = c("DeepLearning", "StackedEnsemble"),
  seed = 123)

automl_leader <- automl_models_h2o@leader

h2o.varimp(automl_leader)

#### predict
pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)
errors_df <- getAPE(validation_s1_for_model$case_count, pred_h2o %>% as_tibble() %>% pull(predict))
summary(errors_df$ape)
quantile(errors_df$ape, probs = seq(.1,1,.05))
valid_pred_df <- s1_validation_data %>% 
  select(application_date) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

test_submission <- s1_test %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>% 
  rename(case_count = pred)

##### analyze
test_pred_augmented <- test_submission %>% 
  add_date_based_features() %>% 
  mutate(split = "test") %>% 
  select(-id, -segment)

valid_pred_augmented <- valid_pred_df %>% 
  add_date_based_features() %>% 
  mutate(split = "valid")

train_plus_valid_actual_augmented <- segment1_date_level %>% 
  mutate(split = "train_plus_valid")

all_with_test_pred <- train_plus_valid_actual_augmented %>% 
  bind_rows(test_pred_augmented)

y_limit = 10000
ggplotly(
  all_with_test_pred %>%
    ggplot(aes(x = day_of_year, y = case_count, label = label)) +
    geom_line() +
    geom_line(data = valid_pred_augmented, aes(x = day_of_year, y = case_count, color = "red")) +
    geom_point(aes(size = I(.5), color = is_end_of_month)) +
    geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
    # geom_vline(data=holidays, aes(xintercept = day_of_year, color=holidays$Occasion)) +
    geom_vline(xintercept = 187, size = 2, color = "red") +
    facet_grid(year ~ .) +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_x_continuous(breaks = c(1, 365, 1)) +
    theme_bw(),
  tooltip = c('label', 'y')
)

View(test_submission)

##### write
write_csv(test_submission, 'data/pred_test_s1_STR_autoML_holidays_v1.csv')



