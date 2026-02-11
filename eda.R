library(tidyverse)
library(ggplot2)


# 1. Load original data
data <- read_csv("all_data.csv")

combined_data <- data

# translink_ridership <- read_csv("data/translink.csv")
# combined_data <- bind_rows(data, translink_ridership)

# we see that Vancouver's true monthly upt tracks nicely with Portland and Seattle, but we just don't have the data...
# Seattle's upt does seem to increase more abruptly post pandemic - hard to say.

# plot 1: upt over time
ggplot(combined_data, aes(x = date, y = total_upt, color = city, group = city)) +
  geom_line(linewidth = 1) +
  geom_point(alpha = 0.5) +
  labs(
  title = "Total UPT (Unlinked Passenger Trips) Over Time",
  #   subtitle = "Comparing Seattle, Portland, Vancouver, and Vancouver (Translink)",
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


# Filter for Seattle and Portland and select modal share columns
modal_trends <- data %>%
  filter(city %in% c("Seattle", "Portland")) %>%
  select(date, city, pct_drove_alone, pct_public_transit, 
         pct_work_from_home, pct_walked, pct_bicycle, pct_taxi_or_ride_share) %>%
  # Pivot to long format for ggplot
  pivot_longer(cols = starts_with("pct_"), 
               names_to = "mode", 
               values_to = "percentage") %>%
  # Clean up names for a prettier legend
  mutate(mode = str_replace_all(mode, "pct_", ""),
         mode = str_replace_all(mode, "_", " "),
         mode = str_to_title(mode)) %>%
  # Remove NAs to keep the plot clean
  filter(!is.na(percentage))

ggplot(modal_trends, aes(x = date, y = percentage, color = mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~city) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Annual Commute Mode Share: Seattle vs. Portland",
    subtitle = "Data source: American Community Survey (ACS) 1-Year Estimates",
    x = "Year",
    y = "Percentage of Commuters",
    color = "Mode of Travel"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )



