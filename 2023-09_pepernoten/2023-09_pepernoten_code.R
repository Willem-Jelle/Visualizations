# Load packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggtext)

# Create data ------------------------------------------------------------------

pepernoot <- tibble(x = 1,
                    y = 1)

# Make 'pepernoot' icon for caption and save as png ----------------------------

pepernoot |>
  ggplot(aes(x = x,
             y = x)) +
  geom_point(size = 35,
             fill = "#7B3F00",
             color = "#000000",
             shape = 21,
             stroke = 9) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA,
                                        color = NA),
        plot.background = element_rect(fill = NA,
                                       color = NA))

ggsave("2023-09_pepernoten_icon.png",
       width = 500,
       height = 500,
       units = "px",
       dpi = 300,
       bg = "transparent")

# Add pepernoot icon with HTML in plot -----------------------------------------

pepernoot_icon <- paste0("<img src = '2023-09_pepernoten_icon.png' width = '60'>")

# Visualize data and save visualization ----------------------------------------

pepernoot |>
  ggplot(aes(x = x,
           y = x)) +
  geom_point(size = 149.5,
             fill = "#7B3F00",
             color = "#000000",
             shape = 21,
             stroke = 9) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#9E2020",
                                        color = NA,
                                        linewidth = 6),
        plot.background = element_rect(fill = "#9E2020"),
        plot.title = element_text(size = 35,
                                  color = "#FFBF00",
                                  lineheight = 0.9,
                                  family = "Comic Sans MS",
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(t = -10,
                                                  b = 25)),
        plot.caption = element_markdown(size = 35,
                                    color = "#FFBF00",
                                    family = "Comic Sans MS",
                                    face = "bold",
                                    hjust = c(0.35, -0.30),
                                    vjust = c(0.5, 0.6),
                                    margin = margin(t = 25)),
        plot.margin = margin(t = 1.7,
                             r = 1.25,
                             l = 1.25,
                             b = 1,
                             unit = "cm")) +
  labs(title = "Wanneer liggen pepernoten \n jaarlijks in de winkel?",
       caption = c(pepernoot_icon,
                   "Te vroeg"))

ggsave("2023-09_pepernoten_viz.png",
       width = 2500,
       height = 2500,
       units = "px",
       dpi = 300,
       bg = "#9E2020")

