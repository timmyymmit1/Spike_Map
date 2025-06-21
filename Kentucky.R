install.packages("R.utils")
install.packages("httr")
library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(abind)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(httr)
library(R.utils)

map_data <- st_read("data/kontur_population_US_20231101.gpkg")

Kentucky <- states %>% 
  filter(NAME == "Kentucky") %>%
  st_transform(crs = st_crs(map_data))

Kentucky %>%
  ggplot() +
  geom_sf()+
  theme_classic()

KY_intersection <- st_intersection(Kentucky, map_data)

Kentucky_BB <- st_bbox(KY_intersection)

Kentucky_get_raster_size <- function() {
  Kentucky_height <- st_distance(st_point(c(Kentucky_BB[["xmin"]], Kentucky_BB["ymin"])),
                        st_point(c(Kentucky_BB[["xmin"]], Kentucky_BB["ymax"]))
                        )
  Kentucky_width <- st_distance(st_point(c(Kentucky_BB[["xmin"]], Kentucky_BB["ymin"])),
                                st_point(c(Kentucky_BB[["xmax"]], Kentucky_BB["ymin"]))
                                )
  if (Kentucky_height > Kentucky_width){
    Kentucky_height_ratio <-1
    Kentucky_width_ratio <- Kentucky_width/Kentucky_height_ratio
  } else {
    Kentucky_width_ratio <-1
    Kentucky_height_ratio <- Kentucky_height/Kentucky_width
  }
  return(list(Kentucky_width_ratio, Kentucky_height_ratio))
} 
Kentucky_get_raster_size()

Kentucky_height_ratioWfunction <- Kentucky_get_raster_size()[[2]]
Kentucky_width_ratioWfunction <- Kentucky_get_raster_size()[[1]]

Kentucky_size <- 3000
Kentucky_width_again <- round((size * Kentucky_width_ratioWfunction), 0)
Kentucky_height_again <- round((size * Kentucky_height_ratioWfunction), 0)

Kentucky_rats <- st_rasterize(KY_intersection, 
                             nx = floor(Kentucky_width_again),
                             ny = floor(Kentucky_height_again))


Kencutky_mat <- matrix(Kentucky_rats$population,
                     nrow = floor(Kentucky_width_again),
                     ncol = floor(Kentucky_height_again))


KY1 <- met.brewer("Pissaro")
swatchplot(KY1)

?height_shade
KY_texture <- grDevices::colorRampPalette(KY1, bias = 2)(256)
swatchplot(KY_texture)
?height_shade

Kencutky_mat %>%
  height_shade(texture = KY1) %>%
  plot_3d(heightmap = Kencutky_mat,
          zscale = 100/2,
          shadowdepth = 1)

render_camera(theta = 0, phi = 45, zoom = .7, fov = 45)
?render_camera
?plot_3d

KY_Plot <- "images/Kentucky_plot.png"    

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
render_highquality(
  filename = KY_Plot,  
  interactive = FALSE,
  title_text = "Kentucky",
  title_size = 35,
  lightdirection = c(3000),
  lightaltitude = c(20,80),
  lightcolor = c(c1[5], "white"),
  lightintensity = c(800,300),
  samples = 450,
  width = 8000,
  height = 8000
)
end_time <- Sys.time()
diff <- end_time - start_time
cat(crayon::cyan(diff), "\n")
}

