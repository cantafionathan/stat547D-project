library(cansim) 
library(janitor)
library(lubridate)

translink_monthly <- get_cansim("23-10-0307") %>%
  clean_names() %>%
  filter(
    urban_transit_agency_name == "South Coast British Columbia Transportation Authority (Translink)",
    total_revenue_and_total_passenger_trips == "Total passenger trips"
  ) %>%
  mutate(
    date = floor_date(date, "month"),
    total_upt = as.numeric(value) * 1000,
    city = "Vancouver (Translink)"
  ) %>%
  select(date, total_upt, city)

write_csv(translink_monthly, "data/translink.csv")