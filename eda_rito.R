## EDA for segment 1
segment_1_data <- read_csv('data/train_segment_1.csv')

#### Visualize daywise demand ####
segment_1_demand_by_date <- segment_1_data %>% 
  group_by(application_date) %>% 
  summarise('total_request' = sum(case_count))

ggplotly(ggplot(segment_1_demand_by_date, aes(x=application_date, y=total_request)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab(""))

#### Visualize day zone wise demand ####
segment_1_demand_by_date_zone <- segment_1_data %>% 
  group_by(application_date, zone) %>% 
  summarise('total_count' = sum(case_count))

ggplot(segment_1_demand_by_date_zone, aes(x=application_date, y=total_count)) +
  geom_line(aes(group = zone, color=as.factor(zone))) +
  xlab("")

#### Visualise state-wise demand by zone ####
segment_1_data_east_zone <- segment_1_data %>% 
  filter(zone == 'EAST')

segment_1_east_zone_demand_by_date_state <- segment_1_data_east_zone %>% 
  group_by(application_date, state) %>% 
  summarise('total_count' = sum(case_count))

ggplotly(ggplot(segment_1_east_zone_demand_by_date_state, aes(x=application_date, y=total_count)) +
  geom_line(aes(group = state, color=as.factor(state))) +
  xlab(""))

#### Visualise branch wise data in WB ####
segment_1_data_wb <- segment_1_data %>% 
  filter(state == 'WEST BENGAL')

segment_1_wb_demand_by_date_branch <- segment_1_data_wb %>% 
  group_by(application_date, branch_id) %>% 
  summarise('total_count' = sum(case_count))

ggplotly(ggplot(segment_1_wb_demand_by_date_branch, aes(x=application_date, y=total_count)) +
           geom_line(aes(group = branch_id, color=as.factor(branch_id))) +
           xlab(""))

