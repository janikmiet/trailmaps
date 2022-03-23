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
r1 <- raster("data/P5114D.tif")
r2 <- raster("data/P5123C.tif")
puijo <- merge(r1,r2)

## For faster test plot, let's downsize the matrix
puijo_mat = raster_to_matrix(puijo)
puijo_small = resize_matrix(puijo_mat,0.25)

## Map crop
long_range   = c(27.62897146535266, 27.68218648738935)
lat_range = c(62.90088418734646, 62.94245554397741)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}
crs(puijo)
utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(puijo))
utm_bbox
extent(puijo)

## Basemap ----
extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
puijo_zoom = crop(puijo, extent_zoomed)
puijo_zoom_mat = raster_to_matrix(puijo_zoom)

base_map = puijo_zoom_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(puijo_zoom_mat, texture = "imhof1", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(puijo_zoom_mat), 0) %>%
  add_shadow(ambient_shade(puijo_zoom_mat),0) %>% 
  add_shadow(texture_shade(puijo_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)


## Strava gpx to map -----
source("gpx_to_points.R") ## function to trasform gpx to sf object
df_strava <- gpx_to_pts("data/Morning_Ride.gpx")
strava <- st_transform(df_strava, crs = crs(puijo))

## Check route and filter by time 
ggplot(strava[strava$time > "2022-03-11 09:02:00" & strava$time < "2022-03-11 09:38:50",]) + 
  geom_sf() 
strava <- strava[strava$time > "2022-03-11 09:03:00" & strava$time < "2022-03-11 09:39:00",]


## Add Features -----
osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

# ## Testing new layers
# osm_new_layer <- opq(osm_bbox) %>% 
#   add_osm_feature("water") %>% 
#   osmdata_sf() 
# osm_new_layer <- st_transform(osm_new_layer$osm_polygons, crs=crs(puijo))
# new_layer <- generate_polygon_overlay(osm_new_layer, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = puijo_zoom_mat)

### Trails layer ----
puijo_highway <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
puijo_lines = st_transform(puijo_highway$osm_lines, crs=crs(puijo))

# unique(puijo_lines$highway)

puijo_trails = puijo_lines %>% 
  filter(highway %in% c("path","bridleway"))
puijo_roads = puijo_lines %>% 
  filter(highway %in% c("unclassified", "secondary","secondary_link", "tertiary", "residential", "service"))
puijo_motorway = puijo_lines %>% 
  filter(highway %in% c("motorway", "motorway_link"))
puijo_track <- puijo_lines %>% 
  filter(highway %in% c("track"))
puijo_cycleway <- puijo_lines %>% 
  filter(highway %in% c("cycleway"))

layer_trails = generate_line_overlay(puijo_roads,extent = extent_zoomed,
                                     linewidth = 10, color="black",
                                     heightmap = puijo_zoom_mat) %>% 
  add_overlay(generate_line_overlay(puijo_roads,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = puijo_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3, offset = c(2,-2),
                                    heightmap = puijo_zoom_mat)) %>%
  add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
                                    linewidth = 3, color="red", lty=3,
                                    heightmap = puijo_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(puijo_motorway, extent = extent_zoomed,
                                    linewidth = 10, color = "black",
                                    heightmap = puijo_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(puijo_motorway, extent = extent_zoomed,
                                    linewidth = 7, color = "white",
                                    heightmap = puijo_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(puijo_track, extent = extent_zoomed,
                                    linewidth = 5, color = "blue3",
                                    heightmap = puijo_zoom_mat))%>% 
  add_overlay(generate_line_overlay(puijo_cycleway, extent = extent_zoomed,
                                    linewidth = 5, color = "white",
                                    heightmap = puijo_zoom_mat))


### Waterway layer -----
puijo_water_lines <- opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
puijo_streams <- st_transform(puijo_water_lines$osm_lines,crs=crs(puijo)) 
layer_stream <-  generate_line_overlay(puijo_streams,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = puijo_zoom_mat)

### Natural (water, woods) layer -----
puijo_natural = opq(osm_bbox) %>% 
  add_osm_feature("natural") %>% 
  osmdata_sf() 

## Multipolygons
puijo_natural1 <- st_transform(puijo_natural$osm_multipolygons, crs=crs(puijo))

puijo_water <-  puijo_natural1 %>% 
  filter(natural %in% c("water"))
layer_water1 <- generate_polygon_overlay(puijo_water, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = puijo_zoom_mat)

puijo_wood <- puijo_natural1 %>% 
  filter(natural %in% c("wood"))
layer_wood1 <- generate_polygon_overlay(puijo_wood, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "brown", heightmap = puijo_zoom_mat)

## polygons
puijo_natural2 <- st_transform(puijo_natural$osm_polygons, crs=crs(puijo))

puijo_water2 <-  puijo_natural2 %>% 
  filter(natural %in% c("water"))
layer_water2 <- generate_polygon_overlay(puijo_water2, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = puijo_zoom_mat)

layer_scrub <-  puijo_natural2 %>%
  filter(natural %in% c("scrub"))
layer_scrub <- generate_polygon_overlay(layer_scrub, extent = extent_zoomed, palette = "darkolivegreen4", linecolor = "darkolivegreen4", heightmap = puijo_zoom_mat)

# puijo_newlayer2 <-  puijo_natural2 %>%
#   filter(natural %in% c("bare_rock"))
# puijo_newlayer2 <- generate_polygon_overlay(puijo_newlayer2, extent = extent_zoomed, palette = "yellow", linecolor = "skyblue2", heightmap = puijo_zoom_mat)

### Building layer -----
puijo_building = opq(osm_bbox) %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() 
layer_buildings = st_transform(puijo_building$osm_polygons,crs=crs(puijo))
layer_buildings = generate_polygon_overlay(layer_buildings, extent = extent_zoomed,
                                          heightmap = puijo_zoom_mat, palette="grey30") 





## Final map with features -----
base_map %>% 
  add_overlay(layer_water1, alphalayer = 0.8) %>%
  add_overlay(layer_water2, alphalayer = 0.8) %>%
  add_overlay(layer_stream, alphalayer = 0.8) %>%
  add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  add_overlay(generate_line_overlay(strava,
                                    extent = extent_zoomed,
                                    linewidth = 10, 
                                    color="red",
                                    heightmap = puijo_zoom_mat)) %>% 
  plot_map(title_text = "Puijo, Kuopio. Data: Maanmittauslaitos Maastotietokanta 03/2022 & OpenStreetMap.org", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)

#### Final 3d map -----
base_map %>% 
  add_overlay(layer_water1, alphalayer = 0.8) %>%
  add_overlay(layer_water2, alphalayer = 0.8) %>%
  add_overlay(layer_stream, alphalayer = 0.8) %>%
  add_overlay(layer_wood1, alphalayer = 0.7) %>%
  add_overlay(layer_scrub, alphalayer = 0.9) %>%
  add_overlay(layer_buildings) %>%
  add_overlay(layer_trails) %>%
  add_overlay(generate_line_overlay(strava,
                                    extent = extent_zoomed,
                                    linewidth = 10, 
                                    color="red",
                                    heightmap = puijo_zoom_mat)) %>% 
  plot_3d(puijo_zoom_mat, windowsize=c(1200,800))
render_camera(theta=240,  phi=30, zoom=0.4,  fov=60)
render_snapshot()


#### To Movie ----
if(interactive()) {
  filename_movie = "puijo_trailmap.mp4" #tempfile()
  
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
    add_overlay(layer_wood1, alphalayer = 0.7) %>%
    add_overlay(layer_scrub, alphalayer = 0.9) %>%
    add_overlay(layer_buildings) %>%
    add_overlay(layer_trails) %>%
    add_overlay(generate_line_overlay(strava,
                                      extent = extent_zoomed,
                                      linewidth = 10, 
                                      color="red",
                                      heightmap = puijo_zoom_mat)) %>% 
    plot_3d(puijo_zoom_mat, windowsize=c(1200,800))
  #Un-comment the following to run:
  render_movie(filename = filename_movie, zoom = .4, fov = 60)
}
