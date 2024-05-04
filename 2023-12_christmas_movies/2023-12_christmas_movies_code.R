# Load packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)
library(monochromeR)
library(ggstream)
library(ggtext)

# Inspired by the great 'stream graph' visualization by Gilbert Fontana --------

# https://twitter.com/GilbertFontana/status/1681734005668888601

# https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html

# Source and read holiday movies data ------------------------------------------

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-12/

holiday_movies <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv")

holiday_movie_genres <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv")

# Filter on Christmas movies ---------------------------------------------------

christmas_movies <- holiday_movies |>
  filter(christmas == TRUE)

# Join both datasets and filter on year and popular genres ---------------------

christmas_movie_genres <- holiday_movie_genres |>
  inner_join(select(christmas_movies,
                    tconst,
                    primary_title,
                    year),
             by = "tconst") |>
  filter(str_detect(genres, "Action|Comedy|Drama|Family|Horror|Romance")) |>
  filter(year >= 2003 & year <= 2023) |>
  group_by(year) |>
  count(genres)

# Count Christmas movie popular genres from 2003 to 2023 -----------------------

christmas_movie_genres_per_year <- holiday_movie_genres |>
  inner_join(select(christmas_movies,
                    tconst,
                    primary_title,
                    year),
             by = "tconst") |>
  filter(str_detect(genres, "Action|Comedy|Drama|Family|Horror|Romance")) |>
  filter(year >= 2003 & year <= 2023) |>
  count(genres)

# Prepare data for mean of ratings of all Christmas movies from 2003 to 2023 ---

christmas_movies_with_ratings <- holiday_movie_genres |>
  inner_join(select(christmas_movies,
                    tconst,
                    primary_title,
                    average_rating,
                    num_votes,
                    year),
             by = "tconst") |>
  distinct(tconst,
           .keep_all = TRUE) |>
  filter(year >= 2003 & year <= 2023)

# Add Google Font and FontAwesome ----------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("fa-pro-6",
         paste0(font_path, "fa-pro-6-solid-900.ttf"))

font_add_google("Open Sans")

showtext_auto()

# Create Christmas color palette -----------------------------------------------

# Source color palette: https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html

color_palette = c("#2F4B7C",
                  "#665191",
                  "#D45087",
                  "#F95d6A",
                  "#FF7C43",
                  "#FFA600")

# Custom annotate functions ----------------------------------------------------

# Annotate icon function

annotate_icon <- function(position_y,
                          fa_icon,
                          fa_icon_color,
                          fa_icon_size)
{
  annotate("richtext",
         x = 2017.65,
         y = position_y,
         label = paste0("<span style='font-family:fa-pro-6'>",
                        "&#x",
                        fa_icon,
                        ";",
                        "</span>"),
         family = "fa-pro-6",
         size = fa_icon_size,
         text.color = fa_icon_color,
         label.color = NA,
         fill = NA)
}

# Annotate text function

annotate_text <- function(position_y,
                          label_text,
                          label_text_color)
{
  annotate("richtext",
         x = 2018,
         y = position_y,
         label = label_text,
         hjust = 0,
         lineheight = 0,
         size = 17,
         family = "Open Sans",
         # fontface = "bold",
         text.color = label_text_color,
         color = NA,
         fill = NA)
}

# Annotate year function

annotate_year <- function(position_x,
                          label_text,
                          label_text_color)
{
  annotate("richtext",
           x = position_x,
           y = 6.5,
           label = label_text,
           hjust = 0,
           lineheight = 0,
           size = 17,
           family = "Open Sans",
           # fontface = "bold",
           text.color = "#F6EFF0",
           color = NA,
           fill = NA)
}
  
# Visualize data and save visualization ----------------------------------------

christmas_movie_genres |>
  ggplot(aes(x = year,
             y = n,
             group = genres,
             fill = genres)) +
  geom_stream(type = "ridge",
              bw = 1,
              n_grid = 1000) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(breaks = c(seq(2003, 2023, 20))) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(# face = "bold",
                                      family = "Open Sans",
                                      size = 70,
                                      lineheight = 0.33,
                                      color = "#2F4B7C",
                                      hjust = 0.2,
                                      margin = margin(t = 15)),
        plot.margin = margin(t = 0,
                             r = -1.1,
                             b = -0.55,
                             l = -1.1,
                             unit = "cm"),
        panel.background = element_rect(fill = "#F6EFF0",
                                        color = NA),
        plot.background = element_rect(fill = "#F6EFF0",
                                       linewidth = 0)) +
  ggtitle(paste0("<span style='font-family:fa-pro-6;'>",
                 "&#x",
                 "f7dc",
                 ";",
                 "</span>",
                 " Populaire genres in 20 jaar kerstfilms")) +
  # Text subtitle
  annotate("richtext",
           x = 2003.55,
           y = 242,
           label = paste0("Het gemiddelde IMDb-cijfer van alle ",
                          christmas_movies |> filter(year >= 2003 & year <= 2023) |> nrow(),
                          " kerstfilms <br>",
                          "uitgekomen tussen ",
                           min(christmas_movie_genres$year),
                           " en ",
                           max(christmas_movie_genres$year),
                           " is een ",
                           round(mean(christmas_movies_with_ratings$average_rating), 1)),
           hjust = 0,
           lineheight = 0.4,
           size = 15,
           family = "Open Sans",
           # fontface = "bold",
           text.color = "#2F4B7C",
           color = NA,
           fill = NA) +
  # Year 2003
  annotate_year(position_x = 2003.1,
              label_text = min(christmas_movie_genres$year)) +
  # Year 2013
  annotate_year(position_x = 2013,
                label_text = median(christmas_movie_genres$year) - 1) +
  # Year 2023
  annotate_year(position_x = 2021.55,
                label_text = max(christmas_movie_genres$year)) +
  # Icon 'Actie'
  annotate_icon(position_y = 231,
                fa_icon = "f06d",
                fa_icon_color = "#2F4B7C",
                fa_icon_size = 19) +
  # Text 'Actie'
  annotate_text(position_y = 230,
                label_text = paste0("Actie (",
                                    christmas_movie_genres_per_year |> filter(genres == "Action") |> pull(n),
                                    ")"),
                label_text_color = "#2F4B7C") +
  # Icon 'Komedie'
  annotate_icon(position_y = 191,
                fa_icon = "f599",
                fa_icon_color = "#2F4B7C",
                fa_icon_size = 19) +
  # Text 'Komedie'
  annotate_text(position_y = 190,
                label_text = paste0("Komedie (",
                                    christmas_movie_genres_per_year |> filter(genres == "Comedy") |> pull(n),
                                    ")"),
                label_text_color = "#F6EFF0") +
  # Icon 'Drama'
  annotate_icon(position_y = 141,
                fa_icon = "f79d",
                fa_icon_color = "#F6EFF0",
                fa_icon_size = 23) +
  # Text 'Drama'
  annotate_text(position_y = 140,
                label_text = paste0("Drama (",
                                    christmas_movie_genres_per_year |> filter(genres == "Drama") |> pull(n),
                                    ")"),
                label_text_color = "#F6EFF0") +
  # Icon 'Familie'
  annotate_icon(position_y = 101,
                fa_icon = "e390",
                fa_icon_color = "#F6EFF0",
                fa_icon_size = 17) +
  # Text 'Familie'
  annotate_text(position_y = 100,
                label_text = paste0("Familie (",
                                    christmas_movie_genres_per_year |> filter(genres == "Family") |> pull(n),
                                    ")"),
                label_text_color = "#F6EFF0") +
  # Icon 'Horror'
  annotate_icon(position_y = 73,
                fa_icon = "f6e2",
                fa_icon_color = "#F6EFF0",
                fa_icon_size = 21) +
  # Text 'Horror'
  annotate_text(position_y = 72,
                label_text = paste0("Horror (",
                                    christmas_movie_genres_per_year |> filter(genres == "Horror") |> pull(n),
                                    ")"),
                label_text_color = "#F6EFF0") +
  # Icon 'Romantisch'
  annotate_icon(position_y = 41,
                fa_icon = "f004",
                fa_icon_color = "#F6EFF0",
                fa_icon_size = 19) +
  # Text 'Romantisch'
  annotate_text(position_y = 40,
                label_text = paste0("Romantisch (",
                                    christmas_movie_genres_per_year |> filter(genres == "Romance") |> pull(n),
                                    ")"),
                label_text_color = "#F6EFF0")

ggsave("2023-12_christmas_movies_viz.png",
       width = 2500,
       height = 1500,
       units = "px",
       dpi = 300,
       bg = "#F6EFF0")