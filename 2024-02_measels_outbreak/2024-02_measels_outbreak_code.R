# Load packages ----------------------------------------------------------------

library(devtools)
library(tidyr)
library(dplyr)
library(showtext)
library(MoMAColors)
library(ggplot2)
library(ggtext)

# Install MoMAColors package via GitHub ----------------------------------------

# devtools::install_github("BlakeRMills/MoMAColors")

# Source of measels data: https://cdn.who.int/media/docs/librariesprovider2/euro-health-topics/vaccines-and-immunization/eur_mr_monthly-_update_en_december-2023.pdf?sfvrsn=699d575a_2&download=true

measels <- tibble(year = c(2022,
                           2023),
                  measels_cases = c(942,
                                    42207)) |>
  mutate(percent = round(measels_cases / 942 * 100, 0),
         increase = round(measels_cases / 942, 0))

# Increase of measels in Europe in 2023 is 45-fold

sum(measels$measels_cases)

# Sum of measels cases in Europa in 2022 & 2023 is 43149

# Create grid for data visualization -------------------------------------------

measels_grid <- expand_grid(x = 0:207,
                            y = 0:207) |>
  slice(1:43149) |>
  mutate(point_color = case_when(
    x <= 30 & y <= 30 ~ "2022",
    .default = "2023")) |>
  mutate(point_color = case_when(
    x == 30 & y >= 12 ~ "2023",
    .default = point_color))

# Add custom font --------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("FranklinGothic",
         paste0(font_path, "FranklinGothic.ttf"))

showtext_auto()

# Musem of Modern Art color palette --------------------------------------------

display.all.moma()

levine_palette <- moma.colors("Levine2",
                              n = 2,
                              type = "discrete",
                              return_hex = TRUE)

# Create treemap & waffle chart with ggplot2 and save visualization ------------

measels_grid |>
  ggplot(aes(x = x,
             y = y,
             color = point_color)) +
  geom_point(size = 1.3) +
  theme_void() +
  coord_flip() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F4E7D5", # Original is #8A6AA0
                                        color = NA, 
                                        linewidth = NA),
        plot.background = element_rect(fill = "#F4E7D5"), # Black is #252525
        plot.title = element_markdown(size = 190,
                                      family = "FranklinGothic",
                                      color = "#252525",
                                      hjust = 0.21,
                                      margin = margin(t = 50,
                                                      r = 0,
                                                      b = 25,
                                                      l = 0)),
        plot.margin = margin(t = 1,
                             r = 1.5,
                             b = 1,
                             l = 1.5,
                             unit = "cm")) +
  ggtitle(paste0(measels |> filter(year == 2023) |> pull(increase),
                 " keer meer mazelen in Europa")) +
  # Text annotation of 2022
  annotate("richtext",
           x = -7,
           y = -1.5,
           label = paste0(measels |> filter(year == 2022) |> pull(measels_cases),
                          " cases in ",
                          measels |> filter(year == 2022) |> pull(year)),
           text.color = "#AD5A6B",
           hjust = 0,
           lineheight = 0,
           size = 50,
           family = "FranklinGothic",
           color = NA,
           fill = NA) +
  # Text annotation of 2023
  annotate("richtext",
           x = 213,
           y = 119,
           label = paste0(measels |> filter(year == 2023) |> pull(measels_cases),
                          " cases in ",
                          measels |> filter(year == 2023) |> pull(year)),
           text.color = "#4D8F8B",
           hjust = 0,
           lineheight = 0,
           size = 50,
           family = "FranklinGothic",
           color = NA,
           fill = NA) +
  # Text source
  annotate("richtext",
           x = -7,
           y = 162,
           label = "Bron: WHO",
           text.color = "#252525",
           hjust = 0,
           lineheight = 0,
           size = 50,
           family = "FranklinGothic",
           color = NA,
           fill = NA) +
  scale_color_manual(values = levine_palette)

ggsave("2024-02_measels_outbreak_viz.png",
       width = 5000,
       height = 5500,
       units = "px",
       dpi = 300,
       bg = "#F4E7D5")
