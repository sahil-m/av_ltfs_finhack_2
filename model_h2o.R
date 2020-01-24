#### Read data for segment 1
train_segment1 <- read_csv('data/train_segment_1_filled.csv')
train_segment2 <- read_csv('data/train_segment_2_filled.csv')

otest <- read_csv('data/test.csv')
test_segment_1 <- otest %>% 
  dplyr::filter(segment == 1) %>% 
  select(-segment)

test_s1_start_date <- min(test_segment_1$application_date)

test_segment_2 <- otest %>% 
  dplyr::filter(segment == 2) %>% 
  select(-segment)

test_s2_start_date <- min(test_segment_2$application_date)

#### aggregate at date-all_region level
train_segment1_date_level <- train_segment1 %>% 
  group_by(application_date) %>% 
  summarise(case_count = sum(case_count))

train_segment2_date_level <- train_segment2 %>% 
  group_by(application_date) %>% 
  summarise(case_count = sum(case_count))

#### visualize
# train_segment1_date_level %>%
#   ggplot(aes(application_date, case_count)) +
#   # Validation Region
#   geom_rect(xmin = as.numeric(ymd("2019-04-05")), 
#             xmax = as.numeric(ymd("2019-07-05")),
#             ymin = 0, ymax = Inf, alpha = 0.02,
#             fill = palette_light()[[3]]) +
#   # Data
#   geom_line(col = palette_light()[1]) +
#   geom_point(col = palette_light()[1]) +
#   geom_ma(ma_fun = SMA, n = 12, size = 1, color = "red") +
#   # Aesthetics
#   theme_tq() +
#   scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle=45, hjust = 1))


#### merge
train_plus_test_s1 <- bind_rows(train_segment1_date_level, select(test_segment_1, -id))
train_plus_test_s1$case_count[is.na(train_plus_test_s1$case_count)] <- 0

#### add time series features
train_plus_test_s1_augmented <- train_plus_test_s1 %>% 
  tk_augment_timeseries_signature()

train_plus_test_s1_augmented_sel <- train_plus_test_s1_augmented %>% 
  select(-contains('.iso'), -contains('.xts'), -contains('.lbl'), -diff) %>% 
  select_if(~ n_distinct(.) > 1) %>% 
  # select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

#### split
validaton_s1_start_date <- date("2019-04-05")

train_s1_for_model <- train_plus_test_s1_augmented_sel %>% 
  filter(application_date < validaton_s1_start_date) %>% 
  select_if(~ !is.Date(.)) 

validation_s1_for_model <- train_plus_test_s1_augmented_sel %>% 
  filter((application_date >= validaton_s1_start_date) & (application_date < test_s1_start_date)) %>% 
  select_if(~ !is.Date(.)) 

test_s1_for_model <- train_plus_test_s1_augmented_sel %>% 
  filter(application_date >= test_s1_start_date) %>% 
  select_if(~ !is.Date(.)) 

#### h20
h2o.init()

train_h2o <- as.h2o(train_s1_for_model)
valid_h2o <- as.h2o(validation_s1_for_model)
test_h2o  <- as.h2o(test_s1_for_model)

y <- "case_count"
x <- setdiff(names(train_h2o), c(y, 'index.num'))

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 4*60*60, 
  stopping_metric = "RMSE")

automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = valid_h2o)

h2o.performance(automl_leader, newdata = valid_h2o)

validation_errors <- validation_s1_for_model %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = case_count) %>%
  mutate(
    error     = actual - pred,
    error_pct = round(error / actual * 100, 2)
  ) 

mean(abs(validation_errors$error_pct))
