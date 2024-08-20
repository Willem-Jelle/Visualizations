# Remaking 'Warming Stripes' (originally by Ed Hawwkins), inspired by Dan Oehm

# Load libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(showtext)
library(MoMAColors)
library(monochromeR)
library(ggplot2)
library(ggtext)

# Import 'live' data, source URL is derived from Tidy Tuesday GitHub -----------

raw_global_temps <- read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", skip = 1) |>
  mutate(across(-Year, as.numeric),
         Year = as.factor(Year))

# Transform data ---------------------------------------------------------------

processed_global_temps <- raw_global_temps |>
  select(Year,
         Jan, Feb, Mar, Apr, May, Jun,
         Jul, Aug, Sep, Oct, Nov, Dec) |>
  pivot_longer(-Year, names_to = "months", values_to = "temperature") |>
  rename_with(~ tolower(.x)) |>
  summarize(mean_temperature = mean(temperature, na.rm = TRUE), .by = year)

# Add Google font --------------------------------------------------------------

font_add_google("Open Sans")

showtext_auto()

# Musem of Modern Art color palette --------------------------------------------

display.all.moma()

?moma.colors

moma.colors("Ernst",
            type = "continuous",
            return_hex = TRUE)

# Make color palette -----------------------------------------------------------

color_palette <- c("2024" = "#67000d", # Warmest
                   "#a50f15",
                   "#cb181d",
                   "#ef3b2c",
                   "#fb6a4a",
                   "#fc9272",
                   "#fcbba1",
                   "#fee0d2", # Color palette source: https://en.wikipedia.org/wiki/Warming_stripes
                   "#c6dbef",
                   "#9ecae1",
                   "#6baed6",
                   "1880" = "#4292c6",
                   "#2171b5",
                   "#08519c",
                   "#08306b") # Coolest

# color_palette <- c("2024" = "#8B174D", # Warmest
#                             "#A3215E",
#                             "#B6326F",
#                             "#C34982",
#                             "#CD6696",
#                             "#D589AB",
#                             "#D9ADBD",
#                             "#D9D2CC", # Color palette source: https://en.wikipedia.org/wiki/Warming_stripes
#                             "#B9C392",
#                             "#9EB466",
#                             "#86A246",
#                    "1880" = "#728939",
#                             "#5C6F2E",
#                             "#475525",
#                             "#343D1F") # Coolest

# Visualize data and save visualization ----------------------------------------

processed_global_temps |>
  ggplot(aes(x = year,
             y = 1,
             fill = mean_temperature,
             width = 1.03)) +
  geom_col() +
  xlab("") +
  ylab("") +
  labs(title = paste0("Verandering jaarlijkse temperatuur wereldwijd van januari",
                      "<span style = 'color:",
                      color_palette["1880"],
                      ";'>",
                      " 1880 ",
                      "</span>",
                      "tot en met juli",
                      "<span style = 'color:",
                      color_palette["2024"],
                      ";'>",
                      " 2024",
                      "</span>")) +
  theme_minimal(base_size = 45,
                base_family = "Open Sans") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 55,
                                   color = "#F4E7D5",
                                   face = "bold",
                                   vjust = 13.5),
        plot.title = element_markdown(face = "bold",
                                      color = "#252525",
                                      hjust = 0.5,
                                      margin = margin(t = 40)),
        plot.margin = grid::unit(c(t = -7.9,
                                   r = 8.5,
                                   b = -19.9,
                                   l = -5),
                                 units = "mm")) +
  scale_fill_gradientn(colours = rev(color_palette)) +
  scale_x_discrete(breaks = c(seq(1884, 2024, 30), 2020),
                   labels = c("1880",
                              "1910",
                              "1940",
                              "1970",
                              "2000",
                              "2024"))

ggsave("2024-07_remaking_warming_stripes_viz.png",
       width = 11,
       height = 5,
       bg = "#F4E7D5")
