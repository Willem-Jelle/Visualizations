library(quarto)
library(webshot)

# Render Quarto code to HTML ---------------------------------------------------

quarto_render("2023-11_talenthub_brabant_treemap_code.qmd",
              output_format = "html",
              output_file = "2023-11_talenthub_brabant_treemap_html.html")

# Make PNG screenshot of HTML file ---------------------------------------------

webshot("2023-11_talenthub_brabant_treemap_html.html",
        # vwidth = 2588,
        # vheight = 2588,
        zoom = 1.5,
        file = "2023-11_talenthub_brabant_treemap_viz.png")

# Remove redudant files to avoid file clutter ----------------------------------

file.remove("2023-11_talenthub_brabant_treemap_ggsave.png")

file.remove("2023-11_talenthub_brabant_treemap_html.html")
