install.packages("sf")
install.packages("tigris")
install.packages("stars")
install.packages("abind")
install.packages("rayshader")
install.packages("MetBrewer")
install.packages("colorspace")
library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(abind)
library(rayshader)
library(MetBrewer)
library(colorspace)

# load kontur data https://data.humdata.org/organization/kontur?sort=metadata_modified+desc&groups=usa
mdata <- st_read("data/kontur_population_US_20231101.gpkg")

#load states (tigris)
states <- states()

glimpse(states)
# Filter for Florida 
florida <- states %>% 
  filter(NAME == "Florida") %>%
  st_transform(crs = st_crs(mdata))

# check map
florida %>%
  ggplot() +
  geom_sf()

#checks crs 
st_crs(mdata)

# do intersection on data to limit kontur to Florida 
st_florida <- st_intersection(mdata, florida)

# define aspect ratio based on bounding box
bb <- st_bbox(st_florida)

botton_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(mdata))

botton_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(mdata))

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(mdata))

#check map
florida %>%
  ggplot() +
  geom_sf()+
  geom_sf(data = botton_left) +
  geom_sf(data = botton_right, color = "red")

width <- st_distance(botton_left, botton_right)
width  
height <- st_distance(top_left, botton_left)
height
width > height
width - height
# handle conditions of width or height being the longer side
if (width < height) {
  w_ratio < - 1
  h_ratio <- height/width
} else {
    h_ratio <- 1
    w_ratio <- 1
  }


#convert to raster so we can convert to matrix

size <- 6000
florida_rats <- st_rasterize(st_florida, 
                             nx = floor(size * w_ratio),
                             ny = floor(size* h_ratio))

florda_mat <- matrix(florida_rats$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size* h_ratio))

#create color palette
c1 <- met.brewer("Demuth")
swatchplot(c1)
?height_shade
texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)


#plot 3d map
rgl::rgl.close()

?plot_3d

range(florda_mat, na.rm = TRUE)

florda_mat %>%
  height_shade(texture = texture) %>%
  plot_3d(heightmap = florda_mat,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 1,
          soil = TRUE)

          render_camera(theta = -20, phi = 45, zoom = .8)

  ?render_highquality        
          
 render_highquality(
   filename = "images/test_plot.png",
   interactive = FALSE,
   title_text = "Florida",
   lightdirection = c(290),
   lightaltitude = c(20,80),
   lightcolor = c(c1[5], "white"),
   lightintensity = c(800,300)
   )
 
 
