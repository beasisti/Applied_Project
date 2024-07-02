library(magick)

# METTI PERCORSO + NOME FOTO 

input_path <- "/Users/albertorogano/Desktop/PoliMI/Applied Statistics/Progetto/Plots/Poster/beta.png"
output_path <- "/Users/albertorogano/Desktop/PoliMI/Applied Statistics/Progetto/Plots/Poster/Modificati/beta_mod.png"

img <- image_read(input_path)

img <- image_convert(img, format = "png")

img_no_bg <- image_transparent(img, color = "white")

image_write(img_no_bg, path = output_path)

