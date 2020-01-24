library(MLmetrics)

#### For segment 2 ####
segment_2_data <- read_csv('data/train_segment_2_filled.csv')

segment_2_demand <- segment_2_data %>% 
  group_by(application_date) %>% 
  summarise('total_request' = sum(case_count))

#### Add temporal features for regression ####
segment_2_demand$day_of_month = day(segment_2_demand$application_date)
segment_2_demand$day_of_week = wday(segment_2_demand$application_date)

segment_2_demand$day_of_month = as.factor(segment_2_demand$day_of_month)
segment_2_demand$day_of_week = as.factor(segment_2_demand$day_of_week)

ggplot(segment_2_demand, aes(x=day_of_month, y=total_request)) + geom_boxplot()
ggplot(segment_2_demand, aes(x=day_of_week, y=total_request)) + geom_boxplot()

#### Split into train and validation split ####
train_data_s2 <- segment_2_demand %>% 
  filter(application_date < date("2019-04-23"))

validation_data_s2 <- segment_2_demand %>% 
  filter(application_date >= date("2019-04-23"))


#### Create linear model on train data ####
fit <- lm(total_request ~ day_of_month, data = train_data_s2)

summary(fit)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(fit$resid~segment_2_demand$day_of_month,
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(fit$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(fit$resid)
qqline(fit$resid)

#### Check mape for validation data ####
validation_data_s2$predictions <- predict(fit, validation_data_s2)

validation_data_s2$ape <- abs(validation_data_s2$predictions - validation_data_s2$total_request) / validation_data_s2$total_request * 100

summary(validation_data_s2$ape)
mape_s2_lm <- MAPE(validation_data_s2$predictions, validation_data_s2$total_request) * 100


#### Predict for segment 2 using lm model with temporal features ####
test_data <- read_csv('data/test.csv')

test_data_s2 <- test_data %>% filter(segment == 2)
test_data_s2$day_of_month = as.factor(day(test_data_s2$application_date))

final_fit <- lm(total_request ~ day_of_month, data = segment_2_demand)
test_data_s2$predictions <- predict(final_fit,test_data_s2) 

test_data_preds <- test_data_s2 %>%
  select(-c(day_of_month)) %>% 
  rename('case_count' = 'predictions')


#### Create output file by adding segment 1 data ####
s1_preds <- read_csv('data/predictions/tbats_7_30_s1_30_s2_model_no_eda_for_segment_2.csv') %>% 
  filter(segment == 1) 

predictions <- rbind(s1_preds, test_data_preds)

write_csv(predictions, 'data/predictions/tbats_7_30_s1_30_s2_lm_model_day_of_month.csv')      
      
      
      
      
      
      
      


#### For Segment 1 ####
segment_1_data <- read_csv('data/train_segment_1_filled.csv')

segment_1_demand <- segment_1_data %>% 
  group_by(application_date) %>% 
  summarise('total_request' = sum(case_count))

segment_1_demand$day_of_month = as.factor(day(segment_1_demand$application_date))
segment_1_demand$day_of_week = as.factor(wday(segment_1_demand$application_date))

ggplot(segment_1_demand, aes(x=day_of_month, y=total_request)) + geom_boxplot()
ggplot(segment_1_demand, aes(x=day_of_week, y=total_request)) + geom_boxplot()

train_data_s1 <- segment_1_demand %>% 
  filter(application_date < date("2019-04-05") & application_date > date("2019-01-01"))

validation_data_s1 <- segment_1_demand %>% 
  filter(application_date >= date("2019-04-05"))

fit_s1 <- lm(total_request ~ day_of_month + day_of_week, data = train_data_s1)
summary(fit_s1)

layout(matrix(c(1,1,2,3),2,2,byrow=T))

#Histogram of Residuals
hist(fit_s1$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(fit_s1$resid)
qqline(fit_s1$resid)

validation_data_s1$predictions <- predict(fit_s1, validation_data_s1)

validation_data_s1$ape <- abs(validation_data_s1$predictions - validation_data_s1$total_request) / validation_data_s1$total_request * 100

summary(validation_data_s1$ape)

