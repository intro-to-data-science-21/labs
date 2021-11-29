# load packages
library(tidyverse)
library(LegislatoR)

# get political data on German legislators
political_df <- 
  left_join(x = filter(get_political(legislature = "ger"), as.numeric("session") == 18), 
            y = get_core(legislature = "ger"), by = "pageid")

# wiki traffic data
traffic_df <- 
  get_traffic(legislature = "ger") %>% 
  filter(date >= "2013-10-22" & date <= "2017-10-24") %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# sessions served
sessions_served_df <- 
  get_political(legislature = "deu") %% 
  group_by(pageid) %>% 
  dplyr::summarize(sessions_served = n())

# merge
legislator_df <- 
  left_join(political_df, sessions_served_df, by = "pageid") %>% 
  left_join(traffic_df, by = "pageid") 

# compute age
get_age <- function(birth, date_at) {
  date_at_fmt <- date_at
  birth_fmt <- birth
  diff <- difftime(lubridate::ymd(birth_fmt), lubridate::ymd(date_at_fmt))
  diff_years <- time_length(diff, "years")
  diff_years
}
legislator_df$age_in_years <- round(get_age(legislator_df$birth, "2017-10-24"), 0)

# plot top 10 pageviews
legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_table <- dplyr::select(rank, name, traffic_mean, traffic_max)
names(legislator_df_table) <- c("Rank", "Representative", "Mean", "Maximum")
legislator_df_table <- head(legislator_df_table, 10)

ggplot(legislator_df_table, aes(y = Mean, x = -Rank)) + 
  xlab("Rank") + ylab("Avg. daily page views") + 
  labs(title = "Top 10 representatives by average daily page views") + 
  geom_bar(stats = "identity") + 
  scale_x_continuous(breaks = -nrow(legislator_df_table):-1, labels = rev(1:nrow(legislator_df_table)))  
geom_text(aes(y = 10, label = Representative), hjust = 0, color = "white", size = 2) + 
  coord_flip() + 
  theme_minimal()

# run model of page views as a function of sessions served, party, sex, and age in years
legislator_df$traffic_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served", "party", "sex", "age_in_years")
fmla <- paste("traffic_log", paste(covars, collapse = " - "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla, legislator_df))

# plot table
sjPlot::tab_model(log_traffic_model)