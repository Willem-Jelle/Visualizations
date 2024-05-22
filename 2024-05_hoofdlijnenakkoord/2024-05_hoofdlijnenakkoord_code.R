# Load packages ----------------------------------------------------------------

library(pdftools)
library(tidyr)
library(tidytext)
library(stopwords)
library(stringr)
library(dplyr)
library(packcircles)
library(showtext)
library(monochromeR)
library(ggplot2)

# Source of hoofdlijnenakkoord: https://files.tweedekamer.nl/sites/default/files/2024-05/20240515%202024D19455%20-%20Coalitieakkoord%202024-2028%20HOOP%2C%20LEF%20EN%20TROTS%20%283%29.pdf

# Import pdf 'Hoofdlijnenakkoord' as text --------------------------------------

text_hoofdlijnenakkoord <- pdf_text("2024-05_hoofdlijnenakkoord_doc.pdf")

# Convert document to data -----------------------------------------------------

raw_hoofdlijnenakkoord <- tibble(text_hoofdlijnenakkoord = text_hoofdlijnenakkoord)

# Create list of Dutch stopwords -----------------------------------------------

dutch_stopwords <- stopwords(language = "nl")

# Tidy and transform data ------------------------------------------------------

tidy_hoofdlijnenakkoord <- raw_hoofdlijnenakkoord |>
  unnest_tokens(output = word, 
                input = text_hoofdlijnenakkoord) |>
  # Filter Dutch stopwords
  filter(! word %in% dutch_stopwords) |>
  # Let words begin with a 'hoofdletter'
  mutate(word = str_to_title(word)) |>
  # Filter some words
  filter(str_detect(word, "Pagina",
                    negate = TRUE)) |>
  # Filter tokens less than 1 character
  filter(str_count(word) > 1) |>
  # Rename and conflate some words
  mutate(word = str_replace_all(word, c("^Eu$" = "EU",
                                        "^Nederland.*" = "Nederland",
                                        "^Nieuwe$" = "Nieuw",
                                        "^Onze$" = "Ons",
                                        "^Wij$" = "We"))) |>
  count(word,
        sort = TRUE,
        name = "aantal") |>
  # Limit number of words to 25
  head(n = 25) |>
  mutate(circle_id = row_number(),
         circle_color = case_when(
           aantal == min(aantal) ~ "#ABCAC8",
           aantal == max(aantal) ~ "#234240",
           .default = "#4D8F8B"),
         circle_font_color = case_when(
           aantal == min(aantal) ~ "#234240",
           .default = "#F4E7D5"))

# Prepare data for visualization -----------------------------------------------

pc_packing <- circleProgressiveLayout(tidy_hoofdlijnenakkoord$aantal,
                                      sizetype = "area")

pc_hoofdlijnenakkoord <- cbind(tidy_hoofdlijnenakkoord,
                               pc_packing) |>
  unite("woord_en_aantal",
        c("word",
          "aantal"),
        sep = "\n(",
        remove = FALSE) |>
  mutate(woord_en_aantal = paste0(woord_en_aantal,
                                  ")"))

pc_dat_gg <- circleLayoutVertices(pc_packing,
                                  npoints = 100) |>
  left_join(select(tidy_hoofdlijnenakkoord,
                   circle_id,
                   circle_color),
            by = c("id" = "circle_id"))

# Musem of Modern Art color palette --------------------------------------------

display.all.moma()

moma.colors("Levine2",
            type = "discrete",
            return_hex = TRUE)

# Add Google font --------------------------------------------------------------

font_add_google("Lato",
                bold.wt = 700)

showtext_auto()

# Create ans save visualization ------------------------------------------------

ggplot() +
  geom_polygon(data = pc_dat_gg,
               aes(x,
                   y,
                   group = id),
               fill = pc_dat_gg$circle_color,
               colour = "#F4E7D5",
               linewidth = 9,
               alpha = 1) +
  geom_text(data = pc_hoofdlijnenakkoord,
            aes(x,
                y,
                label = woord_en_aantal,
                size = aantal),
            family = "Lato",
            fontface = "bold",
            lineheight = 0.33,
            color = pc_hoofdlijnenakkoord$circle_font_color) +
  ggtitle("25 meestgebruikte woorden hoofdlijnenakkoord \n 2024-2028 van PVV, VVD, NSC en BBB") +
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 175,
                                  family = "Lato",
                                  face = "bold",
                                  lineheight = 0.39,
                                  color = "#252525",
                                  hjust = 0.5,
                                  vjust = -1.75,
                                  margin = margin(t = 25,
                                                      r = 0,
                                                      b = 25,
                                                      l = 0)),
        plot.margin = margin(t = 70,
                             r = 70,
                             b = 70,
                             l = 70,
                             unit = "pt")) +
  scale_size_continuous(range = c(23,
                                  39))

ggsave("2024-05_hoofdlijnenakkoord_viz.png",
       width = 6000,
       height = 6000,
       units = "px",
       dpi = 300,
       bg = "#F4E7D5")