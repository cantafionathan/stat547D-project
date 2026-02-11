library(tidyverse)
library(lubridate)
library(splines)

# Configuration
TREATMENT_DATE <- as.Date("2021-01-01")
DONOR_CITIES <- c("Portland") # Add more cities to all_data.csv if needed

# Load and Shape
raw_data <- read_csv("all_data.csv")

seattle_base <- raw_data %>%
  filter(city == "Seattle") %>%
  select(date, total_upt, unemp_rate, gas_price) # Add raw covariates here

donors_wide <- raw_data %>%
  filter(city %in% DONOR_CITIES) %>%
  select(date, city, total_upt) %>%
  pivot_wider(names_from = city, values_from = total_upt, names_prefix = "upt_")

wide_data <- seattle_base %>%
  left_join(donors_wide, by = "date") %>%
  arrange(date) %>%
  mutate(time_idx = as.numeric(date - min(date)) / 30)

# Define X (Predictors) and Y (Outcome)
y_all <- log(wide_data$total_upt)

# Define predictor matrix here - easy to add/remove
X_all <- wide_data %>%
  transmute(
    across(starts_with("upt_"), log), # Log all donor cities
    unemp_rate = unemp_rate,
    gas_price = gas_price
  )

# Create Splines for GAM
B_basis <- bs(wide_data$time_idx, df = 10, degree = 3, intercept = FALSE)

# Split and Clean
pre_idx <- wide_data$date < TREATMENT_DATE
post_idx <- wide_data$date >= TREATMENT_DATE

# Pre-treatment
keep_pre <- complete.cases(y_all[pre_idx], X_all[pre_idx, ])
y_pre <- y_all[pre_idx][keep_pre]
X_pre <- X_all[pre_idx, ][keep_pre, ]
B_pre <- B_basis[pre_idx, ][keep_pre, ]
dates_pre <- wide_data$date[pre_idx][keep_pre]

# Post-treatment
keep_post <- complete.cases(X_all[post_idx, ])
X_post <- X_all[post_idx, ][keep_post, ]
B_post <- B_basis[post_idx, ][keep_post, ]
dates_post <- wide_data$date[post_idx][keep_post]

# Scale and Center
scaling <- scale(X_pre)
X_pre_scaled <- as.matrix(scaling)
X_post_scaled <- scale(X_post, 
                       center = attr(scaling, "scaled:center"), 
                       scale = attr(scaling, "scaled:scale"))

y_mean <- mean(y_pre)
y_pre_centered <- y_pre - y_mean

# Helper for plotting
get_intervals <- function(draws, dates, offset) {
  tibble(
    date = dates,
    mean = colMeans(draws) + offset,
    lo90 = apply(draws, 2, quantile, 0.05) + offset,
    lo50 = apply(draws, 2, quantile, 0.25) + offset,
    hi50 = apply(draws, 2, quantile, 0.75) + offset,
    hi90 = apply(draws, 2, quantile, 0.95) + offset
  )
}

message("Data loaded and scaled. Ready for Stan.")