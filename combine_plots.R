install.packages("magick")
library(magick)
setwd ("C:/Users/Pedro M/Desktop/r_analizes/mixed_forest_biomass/plots/")
img1 <- image_read("biomass_distri_DHB_percente.jpg")
img2 <- image_read("Ind_distri_DHB_percente.jpg")

combined <- image_append(c(img1, img2), stack = TRUE)
image_write(combined, path = "combined_DBH_plots.jpg", format = "jpg")

img1 <- image_read("top5_species_bio.jpg")
img2 <- image_read("top5_species_ind.jpg")

combined <- image_append(c(img1, img2), stack = TRUE)
image_write(combined, path = "combined_top5_plots.jpg", format = "jpg")
