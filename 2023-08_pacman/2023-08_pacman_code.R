# Info -------------------------------------------------------------------------

# Source of pie chart Pac-Man idea not known. Someone on the internet? ¯\_(ツ)_/¯

# Load packages ----------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)

# Create data ------------------------------------------------------------------

pacman <- tibble(value = c(80, 20),
                 is = c("pacman", "not_pacman"))

# Add custom font -------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("emulogic",
         paste0(font_path, "Emulogic.ttf"))

showtext_auto()

# Make color palette -----------------------------------------------------------

color_palette <- c("pacman" = "#FFFF01",
                   "not_pacman" = "#000000")

# Visualize data and save visualization ----------------------------------------

pacman |>
ggplot(aes(x = "",
           y = value,
           fill = is)) +
  geom_bar(stat = "identity") +
  coord_polar("y",
              start = 99.65) +
  theme_void() +
  scale_fill_manual(values = color_palette) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#000000",
                                        color = "#2222DF",
                                        linewidth = 6),
        plot.background = element_rect(fill = "#000000"),
        plot.title = element_text(size = 75,
                                  color = "#DEDEDE",
                                  lineheight = 0.37,
                                  family = "emulogic",
                                  hjust = 0.5,
                                  margin = margin(t = -23,
                                                  b = 27)),
        plot.caption = element_text(size = 46.25,
                                  color = "#DEDEDE",
                                  lineheight = 0.37,
                                  family = "emulogic",
                                  hjust = 0.5,
                                  margin = margin(t = 25)),
        plot.margin = margin(t = 1.7,
                             r = 1.25,
                             l = 1.25,
                             b = 1,
                             unit = "cm")) +
  labs(title = "Pie chart Pac-Man",
       caption = "80% Pac-Man 20% niet Pac-Man")

ggsave("2023-08_pacman_viz.png",
       width = 2500,
       height = 2500,
       units = "px",
       dpi = 300,
       bg = "#000000")