#### Read date level data
load('data/segment_wise_date_level_msts_objects.Rdata')
load('data/segment_wise_date_level_splitted_objects.Rdata')

#### decomposition visualizatuion
s1_train_msts %>% 
  mstl() %>%
  autoplot() + 
  xlab("Days")

s2_train_msts %>% 
  mstl() %>%
  autoplot() + 
  xlab("Days")

#### decomposition + model
get_mape_on_stlf <- function(s.window) {
  pred_on_train_s1_object <- s1_train_msts %>%  
    stlf(h = nrow(s1_validation_data), method = "ets", robust = TRUE, s.window = 13)
  
  return(mape(s1_validation_data$case_count, as.numeric(pred_on_train_s1_object$mean)))
}

sapply(seq(7, 90, 2), get_mape_on_stlf)

pred_on_train_s1_object <- s1_train_msts %>%  
  stlf(h = nrow(s1_validation_data), method = "ets", robust = TRUE, lambda = "auto")

mape(s1_validation_data$case_count, as.numeric(pred_on_train_s1_object$mean))


#### evaluation

# summary(pred_on_train_s1_object)
# 
pred_on_train_s1_object %>%
  autoplot() +
  geom_line(data = s1_validation_msts,
    aes(
      x = as.numeric(time(s1_validation_msts)),
      y = as.numeric(s1_validation_msts)
    ),
    col = "red"
  )

#### compare
error_df <- getAPE(s1_validation_data$case_count, as.numeric(pred_on_train_s1_object$mean))

last_best_model_error_df <- getAPE(s1_validation_data$case_count, mean(s1_train_data$case_count, na.rm = TRUE))

visualise_model_ape_comparison(error_df$ape, last_best_model_error_df$ape)

#### just 2019
s1_train_msts_2019 <- msts(splitTrainTest(s1_train_msts, which(s1_train_data$application_date == ymd("2019-01-01")) - 1)$test, seasonal.periods = c(7, 30))

s1_train_msts_2019 %>% 
  mstl(robust = TRUE) %>%
  autoplot() + 
  xlab("Days")

pred_on_train_s1_object <- s1_train_msts_2019 %>%  
  stlf(h = nrow(s1_validation_data), method = "ets", robust = TRUE)

mape(s1_validation_data$case_count, as.numeric(pred_on_train_s1_object$mean))

#### just 2019 - x11 decomposition
s1_train_ts <- ts(s1_train_data$case_count, frequency = 30)
s1_train_2019_ts <- ts(dplyr::filter(s1_train_data, application_date >= ymd("2019-01-01"))$case_count, frequency = 30)

s1_train_ts %>% 
  AutoSTR(robust = TRUE) %>%
  plot()

s1_train_msts %>% 
  AutoSTR() %>%
  plot()

s1_train_msts_2019 %>% 
  AutoSTR(robust = FALSE) %>%
  plot()

s1_train_str_object <- AutoSTR(s1_train_msts, robust = FALSE)

error_df <- getAPE(s1_train_data$case_count, s1_train_str_object$output$forecast$data)



#### just 2019, date based features
s1_2019_date_features <- s1_train_data %>% 
  group_by()



#### decomposition, then model
stl_decomp_object_s1 <- s1_train_msts %>%
  mstl(robust = TRUE)

seas_adj_s1 <- seasadj(stl_decomp_object_s1)




