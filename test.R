library(magick)

img1 <- image_read("biomass_distri_DHB_percente.jpg")
img2 <- image_read("Ind_distri_DHB_percente.jpg")

combined <- image_append(c(img1, img2), stack = TRUE)
image_write(combined, path = "combined_DBH_plots.jpg", format = "jpg")
