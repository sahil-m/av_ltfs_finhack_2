holidays_augmented <- read_csv('data/holidays_cleaned.csv') %>%
  mutate(application_date = date) %>%
  select(-date, -Weekday) %>%
  group_by(application_date) %>%
  dplyr::filter(row_number()==1) %>%
  right_join(data.frame(application_date = seq.Date(ymd("2017-01-01"), ymd("2019-12-31"), by="day"))) %>%
  mutate(Occasion = if_else(is.na(Occasion), "Nothing", Occasion)) %>%
  add_date_based_features() %>%
  select(-contains("is_")) %>%
  mutate(is_oct_2 =  (Occasion == "Mathatma Gandhi Jayanti"),
         is_labor_day =  (Occasion == "Labor Day"),
         is_other_holidays = !(Occasion %in% c("Mathatma Gandhi Jayanti", "Labor Day")),
         is_first_day_of_year = (day_of_year == 1)
         ) %>%
  group_by(year) %>%
  mutate(is_diwali_week = (week_of_year == week_of_year[Occasion == "Diwali / Deepavali"][1]),
         is_diwali_month = (month == month[Occasion == "Diwali / Deepavali"][1])
         ) %>%
  ungroup() %>%
  select(application_date, contains("is_"))

write_csv(holidays_augmented, "data/holidays_augmented.csv")