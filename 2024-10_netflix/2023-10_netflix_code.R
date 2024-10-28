# Load packages ----------------------------------------------------------------

library(readxl)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(gt)

# Source of data 'What we Watched on Netflix': 

# https://about.netflix.com/en/news/what-we-watched-a-netflix-engagement-report

# https://about.netflix.com/en/news/what-we-watched-the-second-half-of-2023

# Source of data Netflix subscribers:

# https://www.statista.com/statistics/250934/quarterly-number-of-netflix-streaming-subscribers-worldwide/

# Import data Netflix ----------------------------------------------------------

raw_netflix_jan_jun_2023 <- read_xlsx("2024-10_netflix_data_report_2023_jan-jun.xlsx",
                                skip = 3) |>
  clean_names() |>
  mutate(release_date = ymd(release_date))

raw_netflix_jul_dec_2023 <- read_xlsx("2024-10_netflix_data_report_2023_jul-dec.xlsx",
                                skip = 3) |>
  clean_names() |>
  mutate(release_date = ymd(release_date)) |>
  select(-runtime,
         -views)

# Transform and tidy data ------------------------------------------------------

tidy_netflix_full_year <- bind_rows(lst(raw_netflix_jan_jun_2023,
                                        raw_netflix_jul_dec_2023),
                                    .id = "source_id") |>
  mutate(source_id = str_remove_all(source_id, "raw_netflix_")) |>
  arrange(desc(hours_viewed)) |>
  mutate(release_year = year(release_date),
         .after = release_date) |>
  relocate(source_id,
           .after = last_col())

# Create tibble and calculate hours in one lifetime (avarage of 4000 weeks according to Oliver Burke) -----

tidy_netflx_time_spend <- tibble(total_hours_viewed_in_2023 = sum(tidy_netflix_full_year$hours_viewed),
                                       total_hours_per_average_lifetime = (4000 * 7) * 24 )|>
  mutate(total_lifetimes_viewed = round(total_hours_viewed_in_2023 / total_hours_per_average_lifetime, 0),
         # hours_per_month_per_subscriber_2023 = round(total_hours_viewed_in_2023 / 269600000 / 12, 0),
         total_hours_viewed_in_2023 = round(total_hours_viewed_in_2023 / 1000000)) |>
  rename(total_hours_viewed_in_2023_in_millions = total_hours_viewed_in_2023) |>
  pivot_longer(cols = everything(),
               names_to = "Time spend",
               values_to = "Value") |>
  mutate(`Time spend` = str_to_title(`Time spend`),
         `Time spend` = str_replace_all(`Time spend`,
                                    c("_" = " ")),
         `Time spend` = paste0(`Time spend`, ":"))

# Create table and export to html for easy copy-paste --------------------------

tidy_netflx_time_spend |>
  gt() |>
  tab_header(title = "Exported Netflix data for infographic") |>
  gtsave("2024-10_netflix_table_html_export.html")
