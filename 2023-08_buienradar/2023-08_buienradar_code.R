# Load packages ----------------------------------------------------------------

library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(showtext)
library(ggplot2)
library(ggtext)

# Import data ------------------------------------------------------------------

raw_buienradar <- read_csv("2023-08_buienradar_data.csv") |>
  filter(str_detect(advertentiepartner, "^Privacyverklaring.*tabblad$", negate = TRUE)) |>
  distinct(advertentiepartner) |>
  mutate(id = row_number(), .before = advertentiepartner)

# Make tibble ------------------------------------------------------------------

buienradar <- tibble(x = rep(-1:-25, each = 38)) |>
  mutate(y = seq(1, 38, 1), .by = x) |>
  mutate(id = row_number(), .before = x,
         icon_color = case_match(
            id,
            1:933 ~ "advertentiepartner",
            934:950 ~ "empty"),
         icon_html = paste0("<span style='font-family:fa-pro-5-solid'>",
                            "&#xf043;", # Rain icon FontAwesome: f043
                            "</span>")) |>
  left_join(raw_buienradar, by = "id") |>
  mutate(advertentiepartner = replace_na(advertentiepartner, "empty")) |>
  relocate(advertentiepartner, .after = id)

# Add font and FontAwesome -----------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("fa-pro-5-solid",
         paste0(font_path, "Font Awesome 5 Pro-Solid-900.otf"))

font_add("Montserrat-Bold",
         paste0(font_path, "Montserrat-Bold.ttf"))

showtext_auto()

# Create values for plot -------------------------------------------------------

color_palette <- c("advertentiepartner" =  "#152B81",
                   "empty" = "#F9F9F9")

blue <- "#152B81"

green <- "#008000"

plot_title <- paste0("<span style = 'color:",
                     green,
                     ";'>",
                     "Buienradar deelt jouw gegevens met ",
                     "</span>",
                     "<span style = 'color:",
                     blue,
                     ";'>",
                     buienradar |> filter(advertentiepartner != "empty") |> nrow(),
                     "</span>",
                     "<span style = 'color:",
                     green,
                     ";'>",
                     " advertentiepartners",
                     "</span>")

plot_subtitle <- paste0("<span style = 'color:",
                        green,
                        ";'>",
                        "Als je hun cookies accepteert. Elke ",
                        "</span>",
                        "<span style = 'color:",
                        blue,
                        "; font-family:fa-pro-5-solid; font-size:70px'>",
                        "&#xf043;",
                        "</span>",
                        "<span style = 'color:",
                        green,
                        ";'>",
                        " is een advertentiepartner.",
                        "</span>")

# Create waffle chart with ggplot2 and FontAwesome, save visualization ---------

buienradar |>
  ggplot(aes(x = y, y = x, color = icon_color, label = icon_html)) +
  geom_richtext(family = "fa-pro-5-solid",
                size = 18.5,
                label.colour = NA,
                fill = NA) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 56.75,
                                      family = "Montserrat-Bold",
                                      hjust = 0.5,
                                      margin = margin(t = 13,
                                                      r = 0,
                                                      b = 10,
                                                      l = 0)),
        plot.subtitle = element_markdown(size = 47,
                                         family = "Montserrat-Bold",
                                         margin = margin(t = -5,
                                                         r = 0,
                                                         b = 5,
                                                         l = 22)),
        plot.margin = margin(t = 0.5,
                             r = 0.25,
                             b = 0.25,
                             l = 0.25,
                             unit = "cm")) +
  ggtitle(label = plot_title,
          subtitle = plot_subtitle) +
  scale_color_manual(values = color_palette)

ggsave("2023-08_buienradar_viz.png",
       width = 9,
       height = 9,
       bg = "#F9F9F9")
