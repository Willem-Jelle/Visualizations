# Load packages ----------------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(stringr)
library(png)
library(grid)
library(showtext)
library(monochromeR)
library(ggplot2)
library(ggtext)

# Source of cherry tree blossom data: https://ourworldindata.org/grapher/date-of-the-peak-cherry-tree-blossom-in-kyoto

# Import cherry tree blossom data from Our World in Data -----------------------

raw_cherry_tree_blossom <- read_csv("2024-03_cherry_tree_blossom_data.csv")

# Tidy and transform -----------------------------------------------------------

cherry_tree_blossom <- raw_cherry_tree_blossom |>
  clean_names() |>
  select(year,
         day_number_peak_blossom = day_of_the_year_with_peak_cherry_blossom,
         average_20_year_peak_blossom = twenty_year_average_day_of_the_year_with_peak_cherry_blossom) |>
  mutate(temp_year = ifelse(year < 1000, paste0("0", year), year),
         jan_1st_year_x = dmy(paste0("01-01-", temp_year)),
         day_month_peak_blossom = format(jan_1st_year_x + day_number_peak_blossom - 1, "%d %B")) |>
  select(-temp_year,
         -jan_1st_year_x) |>
  relocate(day_month_peak_blossom, .before = year) |>
  mutate(day_month_peak_blossom = str_replace_all(day_month_peak_blossom,
                                                  c("March" = "maart",
                                                    "April" = "april",
                                                    "May" = "mei"))) |>
  mutate(point_color = case_when(
    day_number_peak_blossom == min(day_number_peak_blossom) ~ "#FF4CFF",
    day_number_peak_blossom == max(day_number_peak_blossom) ~ "#FF4CFF",
    .default = "#FFB3FF"))
  
# Import PNG image -------------------------------------------------------------

png_trunk <- readPNG("2024-03_cherry_tree_blossom_trunk.png",
                     native = TRUE)

# Add Google font --------------------------------------------------------------

font_add_google("Open Sans")

showtext_auto()

# Colors for plot

# Source cherry blossom tree color palette: https://www.color-hex.com/color-palette/26108

cherry_blossom_tree_color_palette <- c("#FFE6FF",
                                       "#FFB3FF",
                                       "#FF99FF",
                                       "#FF6699",
                                       "#4D2600")

# Make visualization, patch it together and save visualization -----------------

gg_blossom <- cherry_tree_blossom |>
  ggplot() +
  geom_point(aes(x = year,
                 y = day_number_peak_blossom),
             size = 9,
             color = cherry_tree_blossom$point_color,
             stroke = NA,
             alpha = 1) +
  geom_line(aes(x = year,
                y = average_20_year_peak_blossom),
                color = "#FF4CFF",
                linewidth = 3) +
  # Annotation curve for earliest peak blossom
  geom_curve(x = 2016,
             xend = 1850,
             y = 83.75,
             yend = 80,
             curvature = -0.1,
             angle = 90) +
  # Annotation text earliest peak blossom
  annotate("richtext",
           x = 1550,
           y = 80,
           label = paste0("De vroegste piekbloei",
                          "<br>",
                          "was op 25 maart 2023"),
           hjust = 0,
           lineheight = 0.4,
           size = 33,
           family = "Open Sans",
           fontface = "bold",
           text.color = "#252525",
           color = NA,
           fill = NA) +
  # Annotation curve for latest peak blossom
  geom_curve(x = 1330,
             xend = 1546,
             y = 124.25,
             yend = 126,
             curvature = -0.1,
             angle = 90) +
  # Annotation text latest peak blossom
  annotate("richtext",
           x = 1559,
           y = 126,
           label = paste0("De laatste piekbloei",
                          "<br>",
                          "was op 4 mei 1323"),
           hjust = 0,
           lineheight = 0.4,
           size = 33,
           family = "Open Sans",
           fontface = "bold",
           text.color = "#252525",
           color = NA,
           fill = NA) +
  # Annotation text average every 20 year
  annotate("richtext",
           x = 950,
           y = 109,
           label = paste0("Gemiddelde ",
                          "<br>",
                          "elke 20 jaar"),
           hjust = 0,
           lineheight = 0.4,
           size = 33,
           family = "Open Sans",
           fontface = "bold",
           text.color = "#FF4CFF",
           color = NA,
           fill = NA) +
  labs(title = "Kersenbloesem in Kyoto, Japan bloeit steeds vroeger",
       subtitle = "Piekbloei elk voorjaar van 812 t/m 2023") +
  scale_x_continuous(breaks =  c(812,
                                 2023)) +
  scale_y_continuous(breaks = c(84,
                                104,
                                120),
                     labels = c("Maart",
                               "April",
                               "Mei")) +
  theme_void(base_family = "Open Sans") +
  xlab("") +
  ylab("") +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "#252525",
                                   face = "bold",
                                   size = 100,
                                   vjust = -38),
        axis.text.y = element_text(color = "#252525",
                                   face = "bold",
                                   size = 100,
                                   hjust = 1.25),
        plot.title = element_markdown(face = "bold",
                                      size = 150,
                                      lineheight = 0.33,
                                      color = "#252525",
                                      hjust = 0.1,
                                      margin = margin(t = -125,
                                                      b = 50)),
        plot.subtitle = element_markdown(face = "bold",
                                      size = 100,
                                      lineheight = 0.33,
                                      color = "#252525",
                                      hjust = 0.025,
                                      margin = margin(t = -90,
                                                      b = 75)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA,
                                        color = NA),
        plot.background = element_rect(fill = NA,
                                       color = NA),
  plot.margin = margin(400,
                       100,
                       600,
                       100,
                       unit = "pt")) +
  coord_cartesian(clip = "off")

# Patch blossom plot and PNG image of trunk (designed in Affinity Photo) together

gg_blossom + inset_element(png_trunk,
                           top = 1,
                           left = 1,
                           bottom = 0,
                           right = 0,
                           align_to = "full",
                           on_top = FALSE,
                           clip = TRUE) & 
  theme(panel.background = element_rect(fill = NA,
                                       color = NA),
        plot.background = element_rect(fill = "#F4E7D5",
                                        color = NA))

# Export and save visualization

ggsave("2024-03_cherry_tree_blossom_viz.png",
       width = 7500,
       height = 7500,
       units = "px",
       bg = "#F4E7D5",
       dpi = 300)