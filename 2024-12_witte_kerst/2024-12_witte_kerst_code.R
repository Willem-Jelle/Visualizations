# Load packages ----------------------------------------------------------------

library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(monochromeR)
library(showtext)
library(ggtext)
library(ggplot2)

# Source of data:

# https://www.knmi.nl/nederland-nu/klimatologie/lijsten/wittekerst

# Import data ------------------------------------------------------------------

raw_witte_kerst <- read_xlsx("2024-12_witte_kerst_data.xlsx")

# Tidy and transform data ------------------------------------------------------

tidy_witte_kerst <- raw_witte_kerst |>
  filter(!is.na(nr))

# Make color palette -----------------------------------------------------------

# Source pf color palette:

# https://www.schemecolor.com/christmas-hex-color-codes.php

# Visualize data and export graph ----------------------------------------------

tidy_witte_kerst |>
  ggplot(aes(x = factor(jaar),
             y = sneeuwhoogte_de_bilt_in_cm)) +
  geom_segment(
    data = tidy_witte_kerst,
    aes(x = factor(jaar),
        xend = factor(jaar),
        y = 0,
        yend = sneeuwhoogte_de_bilt_in_cm),
    color = '#F9F9F9',
    linetype = 2,
    linewidth = 1) +
  geom_point(size = 3,
             color = "#F9F9F9") +
  theme_void()

ggsave("2024-12_witte_kerst_graph_r_export.png",
       width = 7000,
       height = 2500,
       units = "px",
       bg = "#165B33",
       dpi =  300)
