# Load packages ----------------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(showtext)
library(ggtext)
library(ggplot2)

# Source of data:

# https://climate.copernicus.eu/copernicus-2024-first-year-exceed-15degc-above-pre-industrial-level

# Import data ------------------------------------------------------------------

raw_warmest_year_in_dutch_data <- read_delim("2025-01_warmest_year_in_dutch_data.csv",
                                                 delim = ",",
                                                 skip = 2)

# Tidy and transform data ------------------------------------------------------

tidy_warmest_year_in_dutch_data <- read_delim("2025-01_warmest_year_in_dutch_data.csv",
                                     delim = ",",
                                     skip = 2) |>
  clean_names() |>
  filter(str_detect(era5, "-999.000",
                    negate = TRUE)) |>
  rename(temperature_anomalies_in_celcius = era5) |>
  select(year,
         temperature_anomalies_in_celcius) |>
  mutate(year = as.numeric(year),
         temperature_anomalies_in_celcius = as.numeric(temperature_anomalies_in_celcius)) |>
  mutate(plot_color = case_match(
    year,
    1940:1976 ~ "klimaatverandering_bestaat_niet",
    1976:2004 ~ "oké_het_bestaat_wel_maar_is_niet_veroorzaakt_door_mensen",
    2005:2019 ~ "oeps",
    2020:2024 ~ "fuck"))

# Add XKCD font ----------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("xkcd_font",
         paste0(font_path, "xkcd-script.ttf"))

showtext_auto()

# Make color palette -----------------------------------------------------------

color_palette <- c("klimaatverandering_bestaat_niet" = "#82EEEE",
                   "oké_het_bestaat_wel_maar_is_niet_veroorzaakt_door_mensen" = "#F2DC17",
                   "oeps" = "#FB7C15",
                   "fuck" = "#C52817")

# Visualize data and export graph ----------------------------------------------

tidy_warmest_year_in_dutch_data |>
  ggplot(aes(x = factor(year),
           y = temperature_anomalies_in_celcius,
           fill = plot_color)) +
  # Text and line at +0.5 °C
  geom_segment(aes(x = 0.6,
                   xend = 85.4,
                   y = 0.5,
                   yend = 0.5),
               color = "#252525") +
  annotate("text",
           x = factor(1940),
           y = 0.6,
           label = "+0.5°C",
           family = "xkcd_font",
           color = "#252525",
           size = 40,
           hjust = 0.1) +
  # Text and line at +1.0 °C
  geom_segment(aes(x = 0.6,
                   xend = 85.4,
                   y = 1,
                   yend = 1),
               color = "#252525") +
  annotate("text",
           x = factor(1940),
           y = 1.1,
           label = "+1.0°C",
           family = "xkcd_font",
           color = "#252525",
           size = 40,
           hjust = 0.1) +
  # Text and line at +1.5 °C
  geom_segment(aes(x = 0.6,
                   xend = 85.4,
                   y = 1.5,
                   yend = 1.5),
               color = "#252525") +
  annotate("text",
           x = factor(1940),
           y = 1.6,
           label = "+1.5°C",
           family = "xkcd_font",
           color = "#252525",
           size = 40,
           hjust = 0.1) +
  geom_col(width = 0.85) +
  theme_void(base_family = "xkcd_font") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 125,
                                   color = "#252525",
                                   vjust = 0.75),
        plot.margin = margin(t = 25,
                             r = 50,
                             b = 25,
                             l = 50,
                             unit = "pt")) +
  scale_fill_manual(values = (color_palette)) +
  scale_x_discrete(breaks = c(seq(1940, 2010, 20), 2024),
                   labels = c("1940",
                              "1960",
                              "1980",
                              "2000",
                              "2024"))

ggsave("2025-01_warmest_year_in_dutch_graph_r_export.png",
        width = 6000,
        height = 3000,
        units = "px",
        bg = "#F4E7D5",
        dpi =  300)