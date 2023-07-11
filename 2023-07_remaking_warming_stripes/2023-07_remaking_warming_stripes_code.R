# Remaking 'Warming Stripes' (originally by Ed Hawwkins), inspired by Dan Oehm

# Load libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(showtext)
library(ggplot2)
library(ggtext)
library(RColorBrewer)

# Import Tidy Tuesday data week 28 2023 ----------------------------------------

raw_global_temps <- read_csv("2023-07_remaking_warming_stripes_data.csv") # Data source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-07-11

# Transform data ---------------------------------------------------------------

processed_global_temps <- raw_global_temps |>
  select(Year,
         Jan, Feb, Mar, Apr, May, Jun,
         Jul, Aug, Sep, Oct, Nov, Dec) |>
  pivot_longer(-Year,
               names_to = "months",
               values_to = "temperature") |>
  rename_with(~ tolower(.x)) |>
  filter(!(is.na(temperature))) |> # Latest data is from May 2023
  summarize(mean_temperature = mean(temperature),
            .by = year)

# Add Google font and colors for title plot ------------------------------------

font_add_google("Open Sans")

showtext_auto()

color_palette_plot_title <- c("1880" = "#4896C4",
                              "2023" = "#710221")

# Make breaks

breaks <- c(seq(1880, 2023, 30), 2023)

# Visualize data and save ------------------------------------------------------

processed_global_temps |>
  ggplot(aes(x = as.factor(year),
             y = 1,
             fill = mean_temperature,
             width = 1.1)) +
  geom_col() +
  xlab("") +
  ylab("") +
  ggtitle(paste0("Verandering jaarlijkse temperatuur wereldwijd van",
                 "<span style = 'color:",
                 color_palette_plot_title["1880"],
                 ";'>",
                 " 1880 ",
                 "</span>",
                 "tot",
                 "<span style = 'color:",
                 color_palette_plot_title["2023"],
                 ";'>",
                 " 2023",
                 "</span>")) +
  theme_minimal(base_size = 50,
             base_family = "Open Sans") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 6.5,
                                   color = "#000000",
                                   face = "bold",
                                   size = 55),
        plot.title = element_markdown(face = "bold",
                                  hjust = 0.5,
                                  margin = margin(t = 35)),
        plot.margin = grid::unit(c(-7.9, 8, -15, -5), "mm")) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) + # Color palette source: https://cortinah.github.io/hockeystick/reference/warming_stripes.html
  scale_x_discrete(breaks = c(seq(1880, 2023, 30), 2023))

?scale_x_discrete

ggsave("2023-07_remaking_warming_stripes_viz.png",
       width = 11,
       height = 5,
       bg = "#FFFFFF")
