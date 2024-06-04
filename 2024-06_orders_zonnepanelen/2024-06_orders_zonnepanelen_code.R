# Load packages ----------------------------------------------------------------

library(tibble)
library(dplyr)
library(stringr)
library(showtext)
library(ggtext)
library(MoMAColors)
library(monochromeR)
library(ggplot2)

# Source of news (and data): https://www.trouw.nl/duurzaamheid-economie/techniek-nederland-luidt-noodklok-markt-zonnepanelen-stort-in-na-hoofdlijnenakkoord~b72e38b1/

# Create data from source ------------------------------------------------------

orders_zonnepanelen <- tibble(date = c("Augustus 2023",
                                       "Juni 2024"),
                              orders_perc = c(round(100 / 95 * 95, 1),
                                              round(100 / 95 * 5, 1)))

# Create color palette ---------------------------------------------------------

display.all.moma()

moma.colors("Connors",
            type = "discrete",
            return_hex = TRUE)

generate_palette("#FFBA1B",
                 modification = "go_darker",
                 n_colors = 5,
                 view_palette = TRUE)

# Add Google font --------------------------------------------------------------

font_add_google("Open Sans")

showtext_auto()

# Create color palette ---------------------------------------------------------

color_palette <- c("Augustus 2023" = "#FFBA1B",
                   "Juni 2024" = "#FFBA1B")

# Test -------------------------------------------------------------------------

?seq

seq(0, 100, by = 20)

?geom_line

?geom_hline

# Visualize data and save visualization ----------------------------------------

orders_zonnepanelen |>
  ggplot(aes(x = date,
             y = orders_perc,
             fill = date)) +
  geom_col(width = 0.7) +
  # geom_hline(yintercept = seq(0.1, 100.1, by = 5),
  #            linewidth = 1.75,
  #            color = "#F4E7D5") +
  # geom_vline(xintercept = seq(0, 3, by = 0.15),
  #            linewidth = 1.75,
  #            color = "#F4E7D5") +
  geom_point(aes(x = date,
                 y = orders_perc + 5),
             size = 25,
             color = "#CC9415") + # Dark yellow is #CC9415
  geom_line(aes(x = date,
                y = orders_perc + 5,
                group = 1),
            linewidth = 9,
            color = "#CC9415") +
  annotate("richtext",
           x = 1.557,
           y = 90,
           label = paste0("Het aantal orders",
                          "<br>",
                          "zonnepanelen ligt",
                          "<br>",
                          "95 procent lager",
                          "<br>",
                          "dan in augustus 2023"),
           hjust = 0,
           lineheight = 0.4,
           size = 50,
           family = "Open Sans",
           fontface = "bold",
           text.color = "#252525",
           color = NA,
           fill = NA) +
  geom_curve(x = 1.69,
             xend = 2.1,
             y = 50,
             yend = 77,
             curvature = 0.33,
             angle = 90,
             color = "#252525",
             linewidth = 4) +
  xlab("") +
  ylab("") +
  theme_minimal(base_family = "Open Sans") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 130,
                                   color = "#252525",
                                   face = "bold",
                                   vjust = 3),
        plot.margin = margin(t = 100,
                             r = 50,
                             b = 100,
                             l = 50,
                             unit = "pt")) +
  scale_fill_manual(values = color_palette)

ggsave("2024-06_orders_zonnepanelen_viz.png",
       width = 6000,
       height = 6000,
       units = "px",
       bg = "#F4E7D5",
       dpi = 300)


