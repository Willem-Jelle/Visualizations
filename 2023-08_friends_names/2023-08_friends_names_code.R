# Install 'Friends' package for data -------------------------------------------

# install.packages("friends")

# Load packages ----------------------------------------------------------------

library(friends)
library(dplyr)
library(stringr)
library(tidyr)
library(showtext)
library(png)
library(grid)
library(ggplot2)

# Custom function str_detect with ignore_case = TRUE ---------------------------

str_detect_ignore_case = function(variable, string) {
  
  str_detect(variable, regex(string, ignore_case = TRUE))
  
}

# Transform data ---------------------------------------------------------------

friends_names <- friends |>
  select(text) |>
  filter(str_detect_ignore_case(text, "Chandler|Joey|Monica|Phoebe|Rachel|Ross"),
         str_detect(text, "^\\(|^\\[", negate = TRUE)) |>
    mutate(name = case_when(
      str_detect_ignore_case(text, "Chandler") ~ "Chandler",
      str_detect_ignore_case(text, "Joey") ~ "Joey",
      str_detect_ignore_case(text, "Monica") ~ "Monica",
      str_detect_ignore_case(text, "Phoebe") ~ "Phoebe",
      str_detect_ignore_case(text, "Rachel") ~ "Rachel",
      str_detect_ignore_case(text, "Ross") ~ "Ross")) |>
  drop_na(name) |>
  count(name)

# Add Friends font -------------------------------------------------------------

font_path <- "/Users/Willem-Jelle/Library/Fonts/"

font_add("gabriel_weiss_friends",
         paste0(font_path, "Gabriel Weiss' Friends Font.ttf"))

showtext_auto()

# Add PNG image ----------------------------------------------------------------

friends_logo <- readPNG("2023-08_friends_names_logo.png", native = TRUE)

friends_grob <- rasterGrob(friends_logo, interpolate = TRUE)

# Make color palette -----------------------------------------------------------

color_palette <- c("Chandler" = "#F24B38",
                   "Joey" = "#02B1F2",
                   "Monica" = "#F4CA3C",
                   "Phoebe" = "#F24B38",
                   "Rachel" = "#F4CA3C",
                   "Ross" = "#02B1F2")

# Visualize data and save visualization ----------------------------------------

friends_names |>
  ggplot(aes(x = factor(name, level = c("Chandler", "Joey", "Monica",
                                        "Phoebe", "Rachel", "Ross")),
             y = 1,
             color = name)) +
  geom_point(aes(size = n)) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F9F9F9",
                                        color = NA),
        plot.background = element_rect(fill = "#F9F9F9",
                                       linewidth = 0),
        plot.margin = margin(b = 0.25,
                             unit = "cm"),
        plot.title = element_text(size = 52.5,
                                  color = "#393536",
                                  lineheight = 0.37,
                                  family = "gabriel_weiss_friends",
                                  hjust = 0.5,
                                  vjust = -65)) +
  ggtitle(paste0(friends_names |> filter(n == max(n)) |> pull(name),
                 " wordt in alle seizoenen het meest",
                 "\n",
                 "bij naam genoemd, ",
                 friends_names |> filter(n == min(n)) |> pull(name),
                 " het minst")) +
  geom_text(aes(label = n),
            family = "gabriel_weiss_friends",
            color = "#393536",
            size = 19,
            vjust = 0.7) +
  geom_text(aes(label = name),
            family = "gabriel_weiss_friends",
            color = "#393536",
            size = 15,
            vjust = 9.25) +
  scale_color_manual(values = color_palette) +
  scale_radius(range = c(23, 43), trans = "identity") + 
  expand_limits(x = c(0.5, 6.5)) +
  annotation_custom(friends_grob,
                    xmin = 1.55, xmax = 5.45, # Change width/position of image with xmin and xmax
                    ymin = 1, ymax = 1.95) + # Change height/position of image with ymin and ymax
  coord_cartesian(ylim = c(0.8, 1.5), 
                  clip = 'off') # Add this function + arguments to go outside the plot

ggsave("2023-08_friends_names_viz.png",
       width = 2500,
       height = 1500,
       units = "px",
       dpi = 300,
       bg = "#F9F9F9")