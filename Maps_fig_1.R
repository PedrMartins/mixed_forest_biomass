pkg <- c("sf", "terra","ggplot2","rnaturalearth",
         "geobr","elevatr", "patchwork", "progress")

pkg <- pkg[!pkg%in%installed.packages()]
pkg
install.packages (pkg)

library(sf)
library (geobr)
library(terra)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(elevatr)
library(progress)

# Brazil outline
brazil <- geobr::read_country()

# States
states <- geobr::read_state("all")

southeast_conifers <- states |>
  dplyr::filter(abbrev_state %in% c("SP", "MG"))
background_states <- states |>
  dplyr::filter(abbrev_state %in% c("SP", "MG", "RJ", "ES",
                                    "BA","MS","GO", "PR"))


map_brazil <-
  ggplot() +
  geom_sf(data = brazil, fill = "gray90") +
  geom_sf(data = states, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = southeast_conifers, fill = "yellowgreen", alpha = 0.4) +
  theme_void()

map_se <-
  ggplot() +
  geom_sf(data = southeast_conifers, fill = "yellowgreen", alpha = 0.4) +
  geom_sf(data = background_states, fill = NA, color = "black") +
  theme_void()

# Example elevation raster (replace with real data if needed)

elev <- elevatr::get_elev_raster(southeast_conifers,
                        z=7,
                        clip = "locations")

elev <- terra::rast(elev)

slope <- terra::terrain(elev, v = "slope", unit = "degrees")


slope_df <- as.data.frame(slope, xy = TRUE)

names(slope_df) <- c("x", "y", "slope")
# Crop to southeast bounding box
bbox_se <- st_bbox(southeast_conifers)

elev_crop <- crop(elev, ext(bbox_se))

# Compute slope


# Convert to data.frame for ggplot
slope_df <- as.data.frame(slope, xy = TRUE)

map_slope <-
  ggplot(slope_df) +
  geom_raster(aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c() +
  geom_sf(data = southeast, fill = NA, color = "white") +
  theme_void()
