# Load packages ----------------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(showtext)
library(ggtext)
library(ggplot2)

# Source of data:

# https://climate.copernicus.eu/copernicus-2024-virtually-certain-be-warmest-year-and-first-year-above-15degc

# Import data ------------------------------------------------------------------

raw_warmest_year_data <- read_csv("2024-11_warmest_year_data.csv")

# Tidy and transform data ------------------------------------------------------

tidy_warmest_year_data <- read_csv("2024-11_warmest_year_data.csv",
                                   skip = 7) |>
  clean_names() |>
  rename(temperature_anomalies_in_celcius = x2t_ano) |>
  mutate(plot_color = case_match(
    year,
    1940:1976 ~ "climate_change_isnt_real",
    1976:2004 ~ "ok_it_is_real_but_not_caused_by_humans",
    2005:2019 ~ "oops",
    2020:2024 ~ "fuck"))

# Add XKCD font ----------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("xkcd_font",
         paste0(font_path, "xkcd-script.ttf"))

showtext_auto()

# Make color palette -----------------------------------------------------------

color_palette <- c("climate_change_isnt_real" = "#82EEEE",
                   "ok_it_is_real_but_not_caused_by_humans" = "#F2DC17",
                   "oops" = "#FB7C15",
                   "fuck" = "#C52817")

# Visualize data and export graph ----------------------------------------------

tidy_warmest_year_data |>
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

ggsave("2024-11_warmest_year_graph_r_export.png",
        width = 6000,
        height = 3000,
        units = "px",
        bg = "#F4E7D5",
        dpi =  300)