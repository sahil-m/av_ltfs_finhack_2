#### Read date level data
segment1_date_level <- read_csv('data/segment1_date_level.csv')

segment2_date_level <- read_csv('data/segment2_date_level.csv')


##### visualizing
segment1_date_level_xts <- xts(dplyr::select(segment1_date_level, case_count), order.by = segment1_date_level$application_date)

segment2_date_level_xts <- xts(dplyr::select(segment2_date_level, case_count), order.by = segment2_date_level$application_date)

# using dygraphs
dygraph(segment1_date_level_xts) %>%
  dyAxis("y", valueRange = c(0, 12000)) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyRoller(rollPeriod = 30)   # use this to smoothen out

# using ggplot2
y_limit <- 12000
ggplotly(
  segment1_date_level %>%
    ggplot(aes(x = application_date, y = case_count)) +
    geom_line() +
    geom_point(aes(size = I(.5))) +
    geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
    # stat_smooth(method = "loess", aes(color = branch_id, fill = branch_id)) +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle=45, hjust = 1))
)

# faceting by year
ggplotly(
  segment1_date_level %>%
    ggplot(aes(x = day_of_year, y = case_count)) +
    geom_line() +
    geom_point(aes(size = I(.5))) +
    geom_area(aes(y=is_weekend*y_limit), fill="yellow", alpha = .3) +
    facet_grid(year ~ .) +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_x_continuous(breaks = c(1, 365, 1)) +
    theme_bw()
)

ggplotly(
  segment2_date_level %>%
    ggplot(aes(x = day_of_year, y = case_count)) +
    geom_line() +
    geom_point(aes(size = I(.5))) +
    geom_area(aes(y=is_weekend*max(case_count)), fill="yellow", alpha = .3) +
    facet_grid(year ~ .) +
    scale_x_continuous(breaks = c(1, 365, 1)) +
    theme_bw()
)