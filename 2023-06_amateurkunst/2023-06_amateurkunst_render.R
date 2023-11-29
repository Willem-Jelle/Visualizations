library(quarto)
library(webshot)

# Render Quarto code to HTML ---------------------------------------------------

quarto_render("2023-06_amateurkunst_code.qmd",
              output_format = "html",
              output_file = "2023-06_amateurkunst_html.html")

# Make PNG screenshot of HTML file ---------------------------------------------

webshot("2023-06_amateurkunst_html.html",
        zoom = 1.5,
        file = "2023-06_amateurkunst_viz.png")

# Remove redundant HTML file ---------------------------------------------------

file.remove("2023-06_amateurkunst_html.html")