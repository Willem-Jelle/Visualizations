library(readxl)
library(dplyr)
library(tidyr)
library(showtext)
library(ggplot2)
library(ggtext)

# Import data ------------------------------------------------------------------

raw_weeronline <- read_xlsx("2023-04_weeronline_data.xlsx") |>
  distinct(derde_partij) |>
  mutate(id = row_number(), .before = derde_partij)

# Make tibble ------------------------------------------------------------------

weeronline <- tibble(x = rep(-1:-25, each = 21)) |>
  mutate(y = seq(1, 21, 1), .by = x) |>
  mutate(id = row_number(), .before = x,
         icon_color = case_match(
            id,
            1:510 ~ "derde_partij",
            511:525 ~ "empty"),
         icon_html = paste0("<span style='font-family:fa-pro-5-solid'>",
                            "&#xf0c2;", # Cloud icon FontAwesome: f0c2
                            "</span>")) |>
  left_join(raw_weeronline, by = "id") |>
  mutate(derde_partij = replace_na(derde_partij, "empty")) |>
  relocate(derde_partij, .after = id)

# Add Google font and FontAwesome ----------------------------------------------

font_add("fa-pro-5-solid",
         "/Users/Willem-Jelle/Library/Fonts/Font Awesome 5 Pro-Solid-900.otf")

font_add_google("Montserrat")

showtext_auto()

# Create values for plot -------------------------------------------------------

color_palette <- c("derde_partij" =  "#0382EB",
                   "empty" = "#F9F9F9")

light_blue <- "#0382EB"

dark_blue <- "#013661"

plot_title <- paste0("<span style = 'color:",
                     dark_blue,
                     ";'>",
                     "Weeronline verkoopt jouw gegevens aan ",
                     "</span>",
                     "<span style = 'color:",
                     light_blue,
                     ";'>",
                     weeronline |> filter(derde_partij != "empty") |> nrow(),
                     "</span>",
                     "<span style = 'color:",
                     dark_blue,
                     ";'>",
                     " derde partijen",
                     "</span>")

plot_subtitle <- paste0("<span style = 'color:",
                        dark_blue,
                        ";'>",
                        "Als je hun cookies accepteert. Elke ",
                        "</span>",
                        "<span style = 'color:",
                        light_blue,
                        "; font-family:fa-pro-5-solid; font-size:75px'>",
                        "&#xf0c2;",
                        "</span>",
                        "<span style = 'color:",
                        dark_blue,
                        ";'>",
                        "  is een derde partij.",
                        "</span>")

# Create waffle chart with ggplot2 and FontAwesome, save visualization ---------

weeronline |>
  ggplot(aes(x = y, y = x, color = icon_color, label = icon_html)) +
  geom_richtext(family = "fa-pro-5-solid",
                size = 20,
                label.colour = NA,
                fill = NA) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 59,
                                      family = "Montserrat",
                                      face = "bold",
                                      hjust = 0.5,
                                      margin = margin(t = 15,
                                                      r = 0,
                                                      b = 10,
                                                      l = 0)),
        plot.subtitle = element_markdown(size = 47,
                                         family = "Montserrat",
                                         face = "bold",
                                         margin = margin(t = -5,
                                                         r = 0,
                                                         b = 5,
                                                         l = 15)),
        plot.margin = margin(t = 0.5,
                             r = 0.5,
                             b = 0.5,
                             l = 0.5,
                             unit = "cm")) +
  ggtitle(label = plot_title,
          subtitle = plot_subtitle) +
  scale_color_manual(values = color_palette)

ggsave("2023-04_weeronline_viz.png",
       width = 9,
       height = 9,
       bg = "#F9F9F9")
