library(magick)
library(rgl)
library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
library(raster)
library(rayshader)
library(tidyverse)
library(XML)

## TODO ideas
# add layers: kasurila peak, parking spots, wet area layer(?)
# altitude color for the routes?

## Data and map crop ----
r1 <- raster("data/kasurila/P5124C.tif")
r2 <- raster("data/kasurila/P5124E.tif")
kasurila <- merge(r1,r2)

## For faster test plot, let's downsize the matrix
kasurila_mat = raster_to_matrix(kasurila)
kasurila_small = resize_matrix(kasurila_mat,0.25)

## Map crop
# 63.038384795378505, 27.698366705357643
# 63.04499960708799, 27.714974943569842

long_range   = c(27.698366705357643, 27.714974943569842)
lat_range = c(63.038384795378505, 63.04499960708799)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}
crs(kasurila)
utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(kasurila))
utm_bbox
extent(kasurila)

## Basemap ----
extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
kasurila_zoom = crop(kasurila, extent_zoomed)
kasurila_zoom_mat = raster_to_matrix(kasurila_zoom)

base_map = kasurila_zoom_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(kasurila_zoom_mat, texture = "imhof1", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(kasurila_zoom_mat), 0) %>%
  add_shadow(ambient_shade(kasurila_zoom_mat),0) %>% 
  add_shadow(texture_shade(kasurila_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)


## Add Features -----
osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

# ## Testing new layers
# osm_new_layer <- opq(osm_bbox) %>% 
#   add_osm_feature("water") %>% 
#   osmdata_sf() 
# osm_new_layer <- st_transform(osm_new_layer$osm_polygons, crs=crs(kasurila))
# new_layer <- generate_polygon_overlay(osm_new_layer, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = kasurila_zoom_mat)

### Trails layer ----
kasurila_highway <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
kasurila_lines = st_transform(kasurila_highway$osm_lines, crs=crs(kasurila))

# unique(kasurila_lines$highway)

kasurila_trails = kasurila_lines %>% 
  filter(highway %in% c("path","bridleway"))
kasurila_roads = kasurila_lines %>% 
  filter(highway %in% c("unclassified", "secondary","secondary_link", "tertiary", "residential", "service"))
kasurila_motorway = kasurila_lines %>% 
  filter(highway %in% c("motorway", "motorway_link"))
kasurila_track <- kasurila_lines %>% 
  filter(highway %in% c("track"))
kasurila_cycleway <- kasurila_lines %>% 
  filter(highway %in% c("cycleway"))

layer_trails = generate_line_overlay(kasurila_roads,extent = extent_zoomed,
                                     linewidth = 10, color="black",
                                     heightmap = kasurila_zoom_mat) %>% 
  add_overlay(generate_line_overlay(kasurila_roads,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = kasurila_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(kasurila_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3, offset = c(2,-2),
                                    heightmap = kasurila_zoom_mat)) %>%
  add_overlay(generate_line_overlay(kasurila_trails,extent = extent_zoomed,
                                    linewidth = 3, color="red", lty=3,
                                    heightmap = kasurila_zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(kasurila_motorway, extent = extent_zoomed,
  #                                   linewidth = 10, color = "black",
  #                                   heightmap = kasurila_zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(kasurila_motorway, extent = extent_zoomed,
  #                                   linewidth = 7, color = "white",
  #                                   heightmap = kasurila_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(kasurila_track, extent = extent_zoomed,
                                    linewidth = 5, color = "blue3",
                                    heightmap = kasurila_zoom_mat))%>% 
  add_overlay(generate_line_overlay(kasurila_cycleway, extent = extent_zoomed,
                                    linewidth = 5, color = "white",
                                    heightmap = kasurila_zoom_mat))


### Waterway layer -----
# kasurila_water_lines <- opq(osm_bbox) %>% 
#   add_osm_feature("waterway") %>% 
#   osmdata_sf() 
# # kasurila_streams <- st_transform(kasurila_water_lines$osm_lines,crs=crs(kasurila)) 
# layer_stream <-  generate_line_overlay(kasurila_streams,extent = extent_zoomed,
#                                        linewidth = 4, color="skyblue2", 
#                                        heightmap = kasurila_zoom_mat)

### Natural (water, woods) layer -----
kasurila_natural = opq(osm_bbox) %>% 
  add_osm_feature("natural") %>% 
  osmdata_sf() 

## Multipolygons
kasurila_natural1 <- st_transform(kasurila_natural$osm_multipolygons, crs=crs(kasurila))
# 
# kasurila_water <-  kasurila_natural1 %>% 
#   filter(natural %in% c("water"))
# layer_water1 <- generate_polygon_overlay(kasurila_water, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = kasurila_zoom_mat)

kasurila_wood <- kasurila_natural1 %>% 
  filter(natural %in% c("wood"))
layer_wood1 <- generate_polygon_overlay(kasurila_wood, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "brown", heightmap = kasurila_zoom_mat)

## polygons
kasurila_natural2 <- st_transform(kasurila_natural$osm_polygons, crs=crs(kasurila))

# kasurila_water2 <-  kasurila_natural2 %>% 
#   filter(natural %in% c("water"))
# layer_water2 <- generate_polygon_overlay(kasurila_water2, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = kasurila_zoom_mat)

layer_scrub <-  kasurila_natural2 %>%
  filter(natural %in% c("scrub"))
layer_scrub <- generate_polygon_overlay(layer_scrub, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "darkolivegreen4", heightmap = kasurila_zoom_mat)

# kasurila_newlayer2 <-  kasurila_natural2 %>%
#   filter(natural %in% c("bare_rock"))
# kasurila_newlayer2 <- generate_polygon_overlay(kasurila_newlayer2, extent = extent_zoomed, palette = "yellow", linecolor = "skyblue2", heightmap = kasurila_zoom_mat)

### Building layer -----
kasurila_building = opq(osm_bbox) %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() 
layer_buildings = st_transform(kasurila_building$osm_polygons,crs=crs(kasurila))
layer_buildings = generate_polygon_overlay(layer_buildings, extent = extent_zoomed,
                                           heightmap = kasurila_zoom_mat, palette="grey30") 





## Final map with features -----
base_map %>% 
  # add_overlay(layer_water1, alphalayer = 0.8) %>%
  # add_overlay(layer_water2, alphalayer = 0.8) %>%
  # add_overlay(layer_stream, alphalayer = 0.8) %>%
  add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  # add_overlay(generate_line_overlay(strava,
  #                                   extent = extent_zoomed,
  #                                   linewidth = 10, 
  #                                   color="red",
  #                                   heightmap = kasurila_zoom_mat)) %>% 
  plot_map(title_text = "Kasurila", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)

#### Final 3d map -----
base_map %>% 
  # add_overlay(layer_water1, alphalayer = 0.8) %>%
  # # add_overlay(layer_water2, alphalayer = 0.8) %>%
  # add_overlay(layer_stream, alphalayer = 0.8) %>%
  add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  plot_3d(kasurila_zoom_mat, windowsize=c(1200,800))
render_camera(theta=240,  phi=30, zoom=0.4,  fov=60)
render_snapshot()


#### To Movie ----
if(interactive()) {
  filename_movie = "img/trailmaps/kasurila3.mp4" #tempfile()
  
  #By default, the function produces a 12 second orbit at 30 frames per second, at 30 degrees azimuth.
  # \donttest{
  # montereybay %>%
  #   sphere_shade(texture="imhof1") %>%
  #   plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
  #           waterlinecolor="white", waterlinealpha=0.5)
  base_map %>% 
    # add_overlay(layer_water1, alphalayer = 0.8) %>%
    # add_overlay(layer_water2, alphalayer = 0.8) %>%
    # add_overlay(layer_stream, alphalayer = 0.8) %>%
    add_overlay(layer_wood1, alphalayer = 0.7) %>%
    add_overlay(layer_scrub, alphalayer = 0.9) %>%
    add_overlay(layer_buildings) %>%
    add_overlay(layer_trails) %>%
    # add_overlay(generate_line_overlay(strava,
    #                                   extent = extent_zoomed,
    #                                   linewidth = 10, 
    #                                   color="red",
    #                                   heightmap = kasurila_zoom_mat)) %>% 
    plot_3d(kasurila_zoom_mat, windowsize=c(1200,800))
  #Un-comment the following to run:
  render_movie(filename = filename_movie, zoom = .4, fov = 60, phi = 30)
}
