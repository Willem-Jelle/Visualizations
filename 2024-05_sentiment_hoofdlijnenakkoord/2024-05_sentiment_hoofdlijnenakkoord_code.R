# Load packages ----------------------------------------------------------------

library(pdftools)
library(tibble)
library(tidytext)
library(dplyr)
library(stringr)
library(DutchSentimentAnalysis)
library(writexl)
library(readxl)
library(showtext)
library(ggtext)
library(MoMAColors)
library(ggplot2)

# Source of 'Hoofdlijnenakkoord': https://files.tweedekamer.nl/sites/default/files/2024-05/20240515%202024D19455%20-%20Coalitieakkoord%202024-2028%20HOOP%2C%20LEF%20EN%20TROTS%20%283%29.pdf

# Import pdf 'Hoofdlijnenakkoord' as text --------------------------------------

text_hoofdlijnenakkoord <- pdf_text("2024-05_hoofdlijnenakkoord_doc.pdf")

# Convert document to data -----------------------------------------------------

raw_hoofdlijnenakkoord <- tibble(text_hoofdlijnenakkoord = text_hoofdlijnenakkoord)

# Tidy and transform data ------------------------------------------------------

export_sentiment_hoofdlijnenakkoord <- raw_hoofdlijnenakkoord |>
  # Manual cleaning and tidying
  mutate(text_hoofdlijnenakkoord = str_squish(text_hoofdlijnenakkoord),
         text_hoofdlijnenakkoord = str_remove_all(text_hoofdlijnenakkoord, "\\s{0,1}\\-")) |>
  # Get sentences
  unnest_tokens(output = sentence,
                input = text_hoofdlijnenakkoord,
                token = "sentences") |>
  # Some more manual cleaning and tidying
  filter(str_detect(sentence, "^pagina.*[[:digit:]]$",
                    negate = TRUE)) |>
  filter(str_count(sentence) > 3) |>
  mutate(sentence = str_remove_all(sentence, "[[:digit:]]\\.$"),
         sentence = str_remove_all(sentence, "pagina\\s\\|\\s[[:digit:]]{1,2}$"),
         sentence = str_remove_all(sentence, "[[:digit:]]$"),
         sentence = str_remove(sentence, "[[:punctuation:]]"),
         sentence = str_squish(sentence)) |>
  # Dutch sentiment anaylsis on sentences from 'Hoofdlijnenakkoord' document
  mutate(ml_score = dutch_sentiment_analysis(sentence),
         ml_sentiment = dutch_sentiment_analysis(sentence,
                                          output = "label")) |>
  # Create six categories
  mutate(category = case_when(
    str_detect(sentence, ".*wonen.*|.*^woning$.*") ~ "Wonen",
    str_detect(sentence, ".*asiel.*|.*migratie.*") ~ "Asiel en migratie",
    str_detect(sentence, ".*zorg\\s.*") ~ "Zorg",
    str_detect(sentence, ".*\\sklimaat.*|.*natuur.*") ~ "Klimaat en natuur",
    str_detect(sentence, ".*onderwijs.*") ~ "Onderwijs",
    str_detect(sentence, ".*veiligheid.*") ~ "Veiligheid",
    .default = NA)) |>
  # Filter NA's
  filter(!is.na(category)) |>
  # Add id
  mutate(id = row_number(),
         .before = sentence) |>
  # Add column for manual sentiment corrections
  mutate(manual_sentiment = ml_sentiment,
         .after = ml_sentiment) |>
  # Sort from A to Z
  arrange(category)

# Export 'sentiment_hoofdlijnenakkoord' to .xlsx for manual --------------------
# sentiment classification (done in Excel for convenience) ---------------------

write_xlsx(export_sentiment_hoofdlijnenakkoord,
           "2024-05_sentiment_hoofdlijnenakkoord_data_export.xlsx")

# Import "2024-05_sentiment_hoofdlijnenakkoord_data_import.xlsx' with ----------
# added manual sentiment classication (done in Excel for convenience) ----------

import_sentiment_hoofdlijnen_akkoord <- read_xlsx("2024-05_sentiment_hoofdlijnenakkoord_data_import.xlsx")

# Add Google font --------------------------------------------------------------

font_add_google("Merriweather")

showtext_auto()

# Create plot subtitle with some styling ---------------------------------------

plot_subtitle <- paste0("<span style = 'color:",
                        "#6DC5B2",
                        ";'>",
                        "Positief",
                        "</span>",
                        "<span style = 'color:",
                        "#252525",
                        ";'>",
                        " | ",
                        "</span>",
                        "<span style = 'color:",
                        "#EAD0AF",
                        ";'>",
                        "Neutraal ",
                        "</span>",
                        "<span style = 'color:",
                        "#252525",
                        ";'>",
                        " | ",
                        "</span>",
                        "<span style = 'color:",
                        "#db95cb",
                        ";'>",
                        "Negatief",
                        "</span>")

# Musem of Modern Art color palette --------------------------------------------

display.all.moma()

moma.colors("Sidhu",
            type = "discrete",
            return_hex = TRUE)

moma.colors("Flash",
            type = "discrete",
            return_hex = TRUE)

# Create color palette ---------------------------------------------------------

color_palette <- c("positive" = "#6DC5B2",
                   "negative" = "#db95cb",
                   "neutral" = "#EAD0AF")

# Visualize data and save visualization ----------------------------------------

import_sentiment_hoofdlijnen_akkoord |>
  arrange(id) |>
  ggplot(aes(x = 25,
             y = reorder(factor(id), id,
                         decreasing = TRUE),
             fill = manual_sentiment)) +
  geom_col(width = 0.5) +
  labs(title = paste0("Sentiment per onderwerp in hoofdlijnenakkoord",
                      "\n",
                      "2024-2028 van PVV, VVD, NCS en BBB"),
       subtitle = plot_subtitle) +
  theme_void(base_family = "Merriweather") +
  theme(legend.position = "none",
        panel.spacing.x = unit(25, "pt"),
        panel.spacing.y = unit(25, "pt"),
        strip.text = element_text(size = 63,
                                  face = "bold",
                                  color = "#252525",
                                  hjust = 0.5,
                                  margin = margin(t = 15,
                                                  r = 0,
                                                  b = 30,
                                                  l = 0)),
        plot.title = element_text(size = 93,
                                  face = "bold",
                                  color = "#252525",
                                  lineheight = 0.39,
                                  hjust = 0.5,
                                  margin = margin(t = 15,
                                                      r = 0,
                                                      b = 25,
                                                      l = 0)),
        plot.subtitle = element_markdown(size = 63,
                                         lineheight = 0.5,
                                         hjust = 0.5,
                                         face = "bold",
                                         padding = unit(c(t = 13,
                                                          r = 7,
                                                          b = 35,
                                                          l = 7),
                                                          unit = "pt")),
        plot.margin = margin(t = 30,
                             r = 50,
                             b = 50,
                             l = 50,
                             unit = "pt")) +
    facet_wrap(~ category,
               nrow = 3,
               ncol = 3,
               scales = "free_y",
               labeller = labeller(paste0(import_sentiment_hoofdlijnen_akkoord$category, "Test"))) +
  scale_fill_manual(values = color_palette)

ggsave("2024-05_sentiment_hoofdlijnenakkoord_viz.png",
       width = 3507.9,
       height = 4960.6,
       units = "px",
       bg = "#F4E7D5",
       dpi = 300)