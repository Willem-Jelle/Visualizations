# Load packages ----------------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(showtext)
library(ggplot2)
library(ggtext)
library(ggforce)

# Source of data trend broedvogels: https://www.cbs.nl/nl-nl/cijfers/detail/84498NED

# Import data trend broedvogels ------------------------------------------------

codes_broedvogels <- read_csv2("2025-01_broedvogels_data_broedvogels.csv") |>
  clean_names() |>
  select(identifier_broedvogels = identifier,
         naam_broedvogel = title)

codes_perioden <- read_csv2("2025-01_broedvogels_data_perioden.csv") |>
  clean_names() |>
  select(identifier_perioden = identifier,
         jaar = title)

observations_broedvogels <- read_csv2("2025-01_broedvogels_data_observations.csv") |>
  clean_names() |>
  select(identifier_broedvogels = broedvogels,
         identifier_perioden = perioden,
         trendcijfer = value)

# Create tidy dataframe --------------------------------------------------------

trend_broedvogels <- observations_broedvogels |>
  left_join(codes_broedvogels,
            by = "identifier_broedvogels") |>
  left_join(codes_perioden,
            by = "identifier_perioden") |>
  # Filter out 'Soortgroeptrend broedvogels'
  filter(naam_broedvogel != "Soortgroeptrend broedvogels") |>
  select(naam_broedvogel,
         trendcijfer,
         jaar) |>
  # Replace NA in column trendcijfer with 0
  mutate(trendcijfer = replace_na(trendcijfer, 0)) |>
  mutate(trend = case_when(
    # Temporary fix with 'Gierzwaluw' and 'Rode Wouw'
    jaar == 2023 & naam_broedvogel == "Gierzwaluw" ~ "toegenomen",
    jaar == 2023 & naam_broedvogel == "Rode Wouw" ~ "toegenomen",
    jaar == 2023 & trendcijfer >= 100 ~ "toegenomen",
    jaar == 2023 & trendcijfer < 100 ~ "afgenomen",
    .default = NA)) |>
  tidyr::fill(trend,
              .direction = "up") |>
  # Rename long bird names
  mutate(naam_broedvogel = str_replace_all(naam_broedvogel,
                                           c("Midden-Europese Goudvink" = "Goudvink"))) |>
  mutate(min_label_y = min(trendcijfer) - 25,
         max_label_y = max(trendcijfer) * 1.25,
         .by = "naam_broedvogel")

# Add Google font --------------------------------------------------------------

font_add_google("Lato")

showtext_auto()

# Create plot title with some styling ------------------------------------------

plot_title <- paste0("<span style = 'color:",
                     "#252525",
                     ";'>",
                     paste0(trend_broedvogels |> distinct(naam_broedvogel) |> nrow()),
                     " broedvogelsoorten in Nederland van ",
                     paste0(min(trend_broedvogels$jaar)), 
                     " t/m ",
                     paste0(max(trend_broedvogels$jaar)),
                     ", ",
                     "</span>",
                     "<span style = 'color:",
                     "#4D8F8B",
                     ";'>",
                     "toegenomen ",
                     "<span style = 'color:",
                     "#252525",
                     ";'>",
                     "of ",
                     "</span>",
                     "<span style = 'color:",
                     "#AD5A6B",
                     ";'>",
                     "afgenomen",
                     "</span>",
                     "<span style = 'color:",
                     "#252525",
                     ";'>",
                     "?",
                     "</span>")

# Create color palette ---------------------------------------------------------
  
color_palette <- c("toegenomen" = "#4D8F8B",
                   "afgenomen" = "#AD5A6B")

bg_color <- "#F4E7D5"

# Create visualization ---------------------------------------------------------

# Set nrow and ncol for facets

facet_nrow = 8
facet_ncol = 8

# The actual plot and styling

gg_viz <- trend_broedvogels |>
  ggplot(aes(x = jaar,
             y = trendcijfer,
             color = trend)) +
  geom_line(linewidth = 0.75) +
  geom_text(aes(x = 2006,
                y = max_label_y,
                label = naam_broedvogel),
            size = 19) +
  geom_segment(aes(x = min(jaar),
                   xend = max(jaar),
                   y = min_label_y,
                   yend = min_label_y),
               linewidth = 0.7) +
  facet_wrap_paginate(~ naam_broedvogel,
                      nrow = facet_ncol,
                      ncol = facet_nrow,
                      scales = "free") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks =  c(1990,
                                 2006,
                                 2022)) +
  theme_minimal(base_family = "Lato") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing.x = unit(25, "pt"),
        panel.spacing.y = unit(25, "pt"),
        plot.title = element_markdown(size = 107,
                                      color = "#252525",
                                      hjust = 0.5,
                                      margin = margin(t = 5,
                                                      r = 0,
                                                      b = 50,
                                                      l = 0)),
        strip.text = element_blank(),
        plot.caption = element_text(size = 61,
                                    color = "#252525",
                                    hjust = 0.5,
                                    margin = margin(t = 50,
                                                    b = 0,
                                                    unit = "pt")),
        plot.margin = margin(t = 50,
                             r = 75,
                             b = 50,
                             l = 75,
                             unit = "pt")) +
  scale_color_manual(values = color_palette)

# Save vizualization to multiple PNG's

for(i in 1:n_pages(gg_viz)){

  gg_save <- gg_viz +
    labs(title = plot_title,
         caption = paste0(i,
                          " van ",
                          n_pages(gg_viz))) +
    facet_wrap_paginate(~ naam_broedvogel,
                        ncol = facet_ncol,
                        nrow = facet_nrow,
                        scales = "free",
                        page = i)

  ggsave(plot = gg_save,
         filename = paste0("2025-01_broedvogels_viz_0",
                           i,
                           ".png"),
         width = 7500,
         height = 7500,
         units = "px",
         bg = "#F9F9F9",
         dpi = 300)

}

