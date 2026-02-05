library(tidyverse)
library(ggplot2)


# 1. Load original data
data <- read_csv("all_data.csv")
translink_ridership <- read_csv("data/translink.csv")
combined_data <- bind_rows(data, translink_ridership)

# we see that Vancouver's true monthly upt tracks nicely with Portland and Seattle, but we just don't have the data...
# Seattle's upt does seem to increase more abruptly post pandemic - hard to say.

# plot 1: upt over time
ggplot(combined_data, aes(x = date, y = total_upt, color = city, group = city)) +
  geom_line(linewidth = 1) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Total UPT (Unlinked Passenger Trips) Over Time",
    subtitle = "Comparing Seattle, Portland, Vancouver, and Vancouver (Translink)",
    x = "Date",
    y = "Total UPT",
    color = "City Source"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# plot 2: pre-pandemic as baseline
indexed <- combined_data %>%
  group_by(city) %>%
  mutate(
    upt_index = total_upt / mean(total_upt[date < as.Date("2020-03-01")], na.rm = TRUE) * 100
  )

ggplot(indexed, aes(date, upt_index, color = city)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dotted") +
  labs(
    title = "Indexed Transit Ridership (Pre-Pandemic = 100)",
    y = "Index"
  ) +
  theme_minimal()

# plot 3: year-over-year change in upt
yoy <- combined_data %>%
  group_by(city) %>%
  arrange(date) %>%
  mutate(
    upt_yoy = (total_upt / lag(total_upt, 12) - 1) * 100
  )

ggplot(yoy, aes(date, upt_yoy, color = city)) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_line() +
  labs(
    title = "Year-over-Year % Change in UPT",
    y = "% change"
  ) +
  theme_minimal()


# plot 4: post-covid slope
post_covid <- combined_data %>%
  filter(date >= as.Date("2020-04-01") & date <= as.Date("2022-12-01"))

ggplot(post_covid, aes(date, total_upt, color = city)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Post-Pandemic Recovery Trends")







