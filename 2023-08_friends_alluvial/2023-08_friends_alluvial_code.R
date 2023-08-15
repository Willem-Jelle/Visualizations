# Install 'Friends' package for data -------------------------------------------

# install.packages("friends")

# Load packages ----------------------------------------------------------------

library(friends)
library(dplyr)
library(stringr)
library(showtext)
library(png)
library(ggplot2)
library(ggalluvial)
library(ggtext)

# Custom function str_detect with ignore_case = TRUE ---------------------------

str_detect_ignore_case = function(variable, string) {
  
  str_detect(variable, regex(string, ignore_case = TRUE))
  
}

# Transform data ---------------------------------------------------------------

friends_who <- friends |>
  select(text, speaker) |>
  filter(str_detect_ignore_case(text, "Chandler|Joey|Monica|Phoebe|Rachel|Ross"),
         str_detect(text, "^\\(|^\\[", negate = TRUE)) |>
  mutate(name = case_when(
    str_detect_ignore_case(text, "Chandler") ~ "Chandler",
    str_detect_ignore_case(text, "Joey") ~ "Joey",
    str_detect_ignore_case(text, "Monica") ~ "Monica",
    str_detect_ignore_case(text, "Phoebe") ~ "Phoebe",
    str_detect_ignore_case(text, "Rachel") ~ "Rachel",
    str_detect_ignore_case(text, "Ross") ~ "Ross")) |>
  filter(str_detect_ignore_case(speaker, "^Chandler\\sBing$|^Joey\\sTribbiani$|^Monica\\sGeller$|^Phoebe\\sBuffay$|^Rachel\\sGreen$|^Ross\\sGeller$")) |>
  mutate(speaker = word(speaker, 1)) |>
  count(speaker, name) |>
  rename(from = speaker,
         to = name) |>
  filter(from != to) |>
  mutate(to = paste0(to, " ")) # Simple 'hack' to make both axis unique

# Add Friends font -------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("gabriel_weiss_friends",
         paste0(font_path, "Gabriel Weiss' Friends Font.ttf"))

showtext_auto()

# Add PNG image ----------------------------------------------------------------

friends_logo <- readPNG("2023-08_friends_alluvial_logo.png", native = TRUE)

# Make color palette -----------------------------------------------------------

color_palette_axis1 <- c("Chandler" = "#02B1F2",
                          "Joey" = "#F24B38",
                          "Monica" = "#F4CA3C",
                          "Phoebe" = "#F24B38",
                          "Rachel" = "#02B1F2",
                          "Ross" = "#F4CA3C")

color_palette_axis2 <- c("Rachel " = "#02B1F2",
                          "Phoebe " = "#F24B38",
                          "Monica " = "#F4CA3C",
                          "Chandler " = "#02B1F2",
                          "Joey " = "#F24B38",
                          "Ross " = "#F4CA3C")

# Make HTML plot title ---------------------------------------------------------

plot_title <- paste0("<img src = '2023-08_friends_alluvial_logo.png' width = '305'>",
                     "<p>", "<p>",
                     "Wie noemt wie het meest bij naam?")

# Visualize data ---------------------------------------------------------------

friends_who |>
  ggplot(aes(axis1 = factor(from, level = c("Rachel",
                                            "Monica",
                                            "Joey",
                                            "Ross",
                                            "Phoebe",
                                            "Chandler")),
             axis2 = factor(to, level = c("Ross ",
                                          "Joey ",
                                          "Chandler ",
                                          "Monica ",
                                          "Phoebe ",
                                          "Rachel ")),
             y = n)) +
  geom_alluvium(aes(fill = from),
                color = "#F9F9F9",
                width = 1/12,
                alpha = 0.25,
                linewidth = 0) +
  geom_stratum(aes(fill = from),
               color = "#F9F9F9",
               width = 1.5/12,
               linewidth = 1.5) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            family = "gabriel_weiss_friends",
            color = "#393536",
            size = 9) +
  geom_hline(yintercept = 7, color = "#F9F9F9", linewidth = 1.5) +
  geom_hline(yintercept = 5255, color = "#F9F9F9", linewidth = 1.5) +
  ggtitle(plot_title) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F9F9F9",
                                        color = NA),
        plot.background = element_rect(fill = "#F9F9F9",
                                       linewidth = 0),
        plot.title = element_markdown(size = 50,
                                      family = "gabriel_weiss_friends",
                                      color = "#393536",
                                      hjust = 0.5,
                                      margin = margin(t = -5,
                                                      b = 9)),
        plot.margin = margin(t = 1,
                             r = 0,
                             l = 0,
                             b = 0.11,
                             unit = "cm")) +
  scale_fill_manual(values = color_palette_axis1,
                    na.value = color_palette_axis2)

ggsave("2023-08_friends_alluvial_viz.png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300,
       bg = "#F9F9F9")