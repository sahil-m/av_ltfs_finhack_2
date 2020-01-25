#### Read date level data
load('data/segment_wise_date_level_msts_objects.Rdata')

#### visualizing the default on entire
s1_train_msts %>%  
  stlf(h = 90) %>%
  autoplot() + 
  xlab("Days")

#### model
pred_s1_object <- s1_train_msts %>%  
  stlf(h = 90)

summary(pred_s1_object)

air_multi_forecast %>%
  autoplot() +
  geom_line(
    aes(
      x = as.numeric(time(air_test)),
      y = as.numeric(air_test)
    ),
    col = "red"
  )