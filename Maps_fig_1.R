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


map_slope <-
  ggplot() +
  geom_raster(
    data = slope_df,
    aes(x = x, y = y, fill = slope)
  ) +
  scale_fill_gradient2(low = "cyan",
                       mid= "yellowgreen",
                       high = "darkred",
                       midpoint = 0)+
  geom_sf(data = southeast_conifers, fill = NA,
          color = "black") +
  geom_sf(data = background_states, fill = NA,
          color = "black") +
  coord_sf(
    xlim = c(-49, -42),
    ylim = c(-26, -19),
    expand = FALSE
  )+

  theme_void()
