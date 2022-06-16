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
# add layers: Puijo peak, parking spots, wet area layer(?)
# altitude color for the routes?

## Data and map crop ----
rr <- raster("data/jynkanvuori/P5114C.tif")

## For faster test plot, let's downsize the matrix
mat = raster_to_matrix(rr)
small = resize_matrix(mat,0.25)


## Map crop
# 62.840892368643466, 27.653837479269225
# 62.853035205676015, 27.69795445106166
long_range   = c(27.653837479269225, 27.69795445106166)
lat_range = c(62.840892368643466, 62.853035205676015)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}
crs(rr)
utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(rr))
utm_bbox
extent(rr)


## Basemap ----
extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
zoom = crop(rr, extent_zoomed)
zoom_mat = raster_to_matrix(zoom)

base_map = zoom_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(zoom_mat, texture = "imhof1", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(zoom_mat), 0) %>%
  add_shadow(ambient_shade(zoom_mat),0) %>% 
  add_shadow(texture_shade(zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)




## Add Features -----
osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

### Trails layer ----
highway <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
lines = st_transform(highway$osm_lines, crs=crs(rr))

# unique(puijo_lines$highway)

trails = lines %>% 
  filter(highway %in% c("path","bridleway"))
roads = lines %>% 
  filter(highway %in% c("unclassified", "secondary","secondary_link", "tertiary", "residential", "service"))
motorway = lines %>% 
  filter(highway %in% c("motorway", "motorway_link"))
track <- lines %>% 
  filter(highway %in% c("track"))
cycleway <- lines %>% 
  filter(highway %in% c("cycleway"))

layer_trails = generate_line_overlay(roads,extent = extent_zoomed,
                                     linewidth = 10, color="black",
                                     heightmap = zoom_mat) %>% 
  add_overlay(generate_line_overlay(roads,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = zoom_mat)) %>% 
  add_overlay(generate_line_overlay(trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3, offset = c(2,-2),
                                    heightmap = zoom_mat)) %>%
  add_overlay(generate_line_overlay(trails,extent = extent_zoomed,
                                    linewidth = 3, color="red", lty=3,
                                    heightmap = zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(motorway, extent = extent_zoomed,
  #                                   linewidth = 10, color = "black",
  #                                   heightmap = zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(motorway, extent = extent_zoomed,
  #                                   linewidth = 7, color = "white",
  #                                   heightmap = zoom_mat)) %>% 
  add_overlay(generate_line_overlay(track, extent = extent_zoomed,
                                    linewidth = 5, color = "blue3",
                                    heightmap = zoom_mat))%>% 
  add_overlay(generate_line_overlay(cycleway, extent = extent_zoomed,
                                    linewidth = 5, color = "white",
                                    heightmap = zoom_mat))


### Waterway layer -----
water_lines <- opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
streams <- st_transform(water_lines$osm_lines,crs=crs(rr)) 
layer_stream <-  generate_line_overlay(streams,extent = extent_zoomed,
                                       linewidth = 4, color="skyblue2", 
                                       heightmap = zoom_mat)

### Natural (water, woods) layer -----
natural = opq(osm_bbox) %>% 
  add_osm_feature("natural") %>% 
  osmdata_sf() 

## Multipolygons
natural1 <- st_transform(natural$osm_multipolygons, crs=crs(rr))

water <-  natural1 %>% 
  filter(natural %in% c("water"))
layer_water1 <- generate_polygon_overlay(water, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = zoom_mat)

wood <- natural1 %>% 
  filter(natural %in% c("wood"))
layer_wood1 <- generate_polygon_overlay(wood, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "brown", heightmap = zoom_mat)

## polygons
natural2 <- st_transform(natural$osm_polygons, crs=crs(rr))

water2 <-  natural2 %>% 
  filter(natural %in% c("water"))
layer_water2 <- generate_polygon_overlay(water2, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = zoom_mat)

layer_scrub <-  natural2 %>%
  filter(natural %in% c("scrub"))
layer_scrub <- generate_polygon_overlay(layer_scrub, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "darkolivegreen4", heightmap = zoom_mat)

# newlayer2 <-  natural2 %>%
#   filter(natural %in% c("bare_rock"))
# newlayer2 <- generate_polygon_overlay(newlayer2, extent = extent_zoomed, palette = "yellow", linecolor = "skyblue2", heightmap = zoom_mat)

### Building layer -----
building = opq(osm_bbox) %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() 
layer_buildings = st_transform(building$osm_polygons,crs=crs(rr))
layer_buildings = generate_polygon_overlay(layer_buildings, extent = extent_zoomed,
                                           heightmap = zoom_mat, palette="grey30") 





## Final map with features -----
base_map %>% 
  add_overlay(layer_water1, alphalayer = 0.8) %>%
  add_overlay(layer_water2, alphalayer = 0.8) %>%
  add_overlay(layer_stream, alphalayer = 0.8) %>%
  # add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  # add_overlay(generate_line_overlay(strava,
  #                                   extent = extent_zoomed,
  #                                   linewidth = 10, 
  #                                   color="red",
  #                                   heightmap = zoom_mat)) %>% 
  plot_map(title_text = "Jynkänvuori, Kuopio. Data: Maanmittauslaitos Maastotietokanta 03/2022 & OpenStreetMap.org",  title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)

#### Final 3d map -----
base_map %>% 
  add_overlay(layer_water1, alphalayer = 0.8) %>%
  add_overlay(layer_water2, alphalayer = 0.8) %>%
  add_overlay(layer_stream, alphalayer = 0.8) %>%
  # add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  # add_overlay(generate_line_overlay(strava,
  #                                   extent = extent_zoomed,
  #                                   linewidth = 10, 
  #                                   color="red",
  #                                   heightmap = puijo_zoom_mat)) %>% 
  plot_3d(zoom_mat, windowsize=c(1200,800))
render_camera(theta=240,  phi=30, zoom=0.4,  fov=60)
render_snapshot()


#### To Movie ----
if(interactive()) {
  filename_movie = "img/trailmaps/jynkanvuori.mp4" #tempfile()
  
  #By default, the function produces a 12 second orbit at 30 frames per second, at 30 degrees azimuth.
  # \donttest{
  # montereybay %>%
  #   sphere_shade(texture="imhof1") %>%
  #   plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
  #           waterlinecolor="white", waterlinealpha=0.5)
  base_map %>% 
    add_overlay(layer_water1, alphalayer = 0.8) %>%
    add_overlay(layer_water2, alphalayer = 0.8) %>%
    add_overlay(layer_stream, alphalayer = 0.8) %>%
    # add_overlay(layer_wood1, alphalayer = 0.7) %>%
    add_overlay(layer_scrub, alphalayer = 0.9) %>%
    add_overlay(layer_buildings) %>%
    add_overlay(layer_trails) %>%
    # add_overlay(generate_line_overlay(strava,
    #                                   extent = extent_zoomed,
    #                                   linewidth = 10, 
    #                                   color="red",
    #                                   heightmap = puijo_zoom_mat)) %>% 
    plot_3d(zoom_mat, windowsize=c(1200,800))
  #Un-comment the following to run:
  render_movie(filename = filename_movie, zoom = .4, fov = 60, phi = 30)
}


