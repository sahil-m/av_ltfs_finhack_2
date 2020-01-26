source('utils.R')

s2_data <- read_csv('data/segment2_date_level.csv')

#### Visualisations ####
ggplot(s2_data, aes(x=as.factor(day_of_month), y=case_count)) + geom_boxplot()
ggplot(s2_data, aes(x=as.factor(day_of_week), y=case_count)) + geom_boxplot()
ggplot(s2_data, aes(x=as.factor(week_of_month), y=case_count)) + geom_boxplot()
ggplot(s2_data, aes(x=as.factor(month), y=case_count)) + geom_boxplot()
ggplot(s2_data, aes(x=part_of_month, y=case_count)) + geom_boxplot()

s2_data_filtered <- s2_data %>% 
  select(application_date, case_count, day_of_month, part_of_month, day_of_week, week_of_month, month, quarter) %>% 
  mutate(
    'wom2' = as.factor(ceiling(day_of_month / 7)),
    'day_of_month' = as.factor(day_of_month),
    'day_of_week' = as.factor(day_of_week),
    'week_of_month' = as.factor(week_of_month),
    'part_of_month' = as.factor(part_of_month),
    'month' = as.factor(month),
    'is_sunday' = (day_of_week == 'Sun'),
    'quarter' = as.factor(quarter)
  )


#### First model ( name to be inserted later ) ####
fit <- lm(case_count ~ day_of_week + week_of_month + month, data = s2_data_filtered)
summary(fit)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(fit$resid~s2_data$day_of_month,
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(fit$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(fit$resid)
qqline(fit$resid)


#### Cross validation ####
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(case_count ~ day_of_week + week_of_month + month, data = s2_data_filtered, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#### Check MAPE on validation data ####
s2_validation_start_date <- ymd("2019-04-23")

s2_train_data <- s2_data_filtered %>% 
  filter(application_date < s2_validation_start_date)

s2_validation_data <- s2_data_filtered %>% 
  filter(application_date >= s2_validation_start_date)

fit <- lm(case_count ~ day_of_month + is_sunday + quarter, data=s2_train_data)

s2_validation_data$predicted <- predict(fit, s2_validation_data)


median_counts_by_day <- s2_train_data %>%
  filter(application_date > date('2019-01-01')) %>% 
  group_by(day_of_week) %>%
  summarise('median' = median(case_count))
  
s2_validation_data <- s2_validation_data %>% 
  mutate('predicted' = if_else(predicted < 0,
                               0,
                               predicted
                               ))

ape_dist <- getAPE(s2_validation_data$case_count, s2_validation_data$predicted)

s2_validation_data$ape <- ape_dist$ape
summary(ape_dist$ape)

####### DO NOT RUN #####
summary(base_ape)

visualise_model_ape_comparison(s2_validation_data$ape, base_ape)

#### Create test submission file ####
s1_predictions <- read_csv('data/predictions/tbats_7_30_s1_30_s2_lm_model_day_of_month.csv') %>% 
  filter(segment == 1)

s2_preds <- read_csv('data/test.csv') %>% 
  filter(segment == 2)

final_fit <- lm(case_count ~ day_of_month + is_sunday + quarter, data=s2_data_filtered)

s2_preds <- add_date_based_features(s2_preds)
s2_preds <- s2_preds %>% 
  select(id, application_date, day_of_month, segment, day_of_week, quarter) %>% 
  mutate(
    'day_of_month' = as.factor(day_of_month),
    'is_sunday' = (day_of_week == 'Sun'),
    'quarter' = as.factor(quarter)
  ) %>% 
  select(-c(day_of_week))

s2_preds$case_count <- predict(final_fit, s2_preds) 
s2_preds <- s2_preds %>% 
  select(id, application_date, segment, case_count)

final_predictions <- rbind(s1_predictions, s2_preds)

write_csv(final_predictions, 'data/predictions/tbats_7_30_s1_30_s2_lm_model_dom_iss_dow.csv')
