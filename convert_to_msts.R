#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')
segment2_date_level <- read_csv('data/segment2_date_level.csv')

#### convert to msts object
segment1_date_level_msts <- msts(segment1_date_level$case_count, seasonal.periods = c(7, 30))

segment2_date_level_msts <- msts(segment2_date_level$case_count, seasonal.periods = c(7, 30))

#### save
save(segment1_date_level_msts, segment2_date_level_msts, file = 'data/segment_wise_date_level_msts_objects.Rdata')





