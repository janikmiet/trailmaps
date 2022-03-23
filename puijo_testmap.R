## Testing Rayshader 
## Elevation map from: https://tiedostopalvelu.maanmittauslaitos.fi/tp/kartta
## Guide from: https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/

## Rayshader paketin testausta
## Tutustutaan MML avoimen data tietokantaan ja korkeusmallien hyödyntämiseen
## Haettu Puijon alueen 2m korkeus data (MML/tif) ja sen päälle hyödynnetään Open Street Map -tietokantaa (tiet, rakennukset yms. tietoja)


library(magick)
library(rgl)
library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
library(raster)
library(rayshader)

## Data and test run ----
## Download data manually from https://tiedostopalvelu.maanmittauslaitos.fi/tp/kartta
## Crop Puijo area and use 2m elevation map. You should get two tif files:

r1 <- raster("data/P5114D.tif")
r2 <- raster("data/P5123C.tif")
puijo <- merge(r1,r2)

## For faster test plot, let's downsize the matrix
puijo_mat = raster_to_matrix(puijo)
puijo_small = resize_matrix(puijo_mat,0.25)

## This is how it looks like:
puijo_small %>% 
  height_shade() %>% 
  plot_map()

## Create more detailed map
puijo_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(puijo_small, texture = "imhof1", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(puijo_small,zscale=6), 0) %>%
  add_shadow(ambient_shade(puijo_small), 0) %>%
  add_shadow(texture_shade(puijo_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()


## Rezizing map area -----

## Get points from Google map (left down & right up corners)
# 62.90088418734646, 27.62897146535266
# 62.94245554397741, 27.68218648738935
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

## Puijo basemap -----

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

## 3d model
base_map %>% 
  plot_3d(puijo_zoom_mat, windowsize=c(1200,800))
render_camera(theta=240,  phi=30, zoom=0.3,  fov=60)
render_snapshot()



## Add Features -----

osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

### Roads and paths -----
puijo_highway <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
puijo_highway

puijo_lines = st_transform(puijo_highway$osm_lines, crs=crs(puijo))

ggplot(puijo_lines,aes(color=osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Open Street Map `highway` attribute in Puijo")


puijo_trails = puijo_lines %>% 
  filter(highway %in% c("path","bridleway"))

puijo_footpaths = puijo_lines %>% 
  filter(highway %in% c("footway"))

puijo_roads = puijo_lines %>% 
  filter(highway %in% c("unclassified", "secondary", "tertiary", "residential", "service"))


## save trails to layer
# trails_layer = generate_line_overlay(puijo_footpaths,extent = extent_zoomed,
#                                      linewidth = 10, color="black", 
#                                      heightmap = puijo_zoom_mat) %>% 
#   add_overlay(generate_line_overlay(puijo_footpaths,extent = extent_zoomed,
#                                     linewidth = 6, color="white",
#                                     heightmap = puijo_zoom_mat)) %>%
#   add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
#                                     linewidth = 3, color="black", lty=3, offset = c(2,-2),
#                                     heightmap = puijo_zoom_mat)) %>%
#   add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
#                                     linewidth = 3, color="white", lty=3,
#                                     heightmap = puijo_zoom_mat)) %>%
#   add_overlay(generate_line_overlay(puijo_roads,extent = extent_zoomed,
#                                     linewidth = 8, color="black",
#                                     heightmap = puijo_zoom_mat)) 

trails_layer = generate_line_overlay(puijo_roads,extent = extent_zoomed,
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
                                      heightmap = puijo_zoom_mat))


### Testing trails overlay -----
base_map %>% 
  add_overlay(generate_line_overlay(puijo_roads,extent = extent_zoomed,
                                    linewidth = 10, color="black",
                                    heightmap = puijo_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(puijo_roads,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = puijo_zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(puijo_footpaths,extent = extent_zoomed,
  #                                   linewidth = 10, color="grey40", 
  #                                   heightmap = puijo_zoom_mat)) %>% 
  # add_overlay(generate_line_overlay(puijo_footpaths,extent = extent_zoomed,
  #                                   linewidth = 6, color="black",
                                    # heightmap = puijo_zoom_mat)) %>%
  add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3, offset = c(2,-2),
                                    heightmap = puijo_zoom_mat)) %>%
  add_overlay(generate_line_overlay(puijo_trails,extent = extent_zoomed,
                                    linewidth = 3, color="red", lty=3,
                                    heightmap = puijo_zoom_mat)) %>%
  plot_map()

### Water layer -----

puijo_water_lines <- opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
puijo_water_lines
puijo_water_lines$osm_lines

puijo_streams = st_transform(puijo_water_lines$osm_lines,crs=crs(puijo)) 

stream_layer = generate_line_overlay(puijo_streams,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = puijo_zoom_mat)

### Lake layer -----
puijo_natural = opq(osm_bbox) %>% 
  add_osm_feature("natural") %>% 
  osmdata_sf() 

puijo_water <- st_transform(puijo_natural$osm_multipolygons, crs=crs(puijo))

puijo_water <-  puijo_water %>% 
  filter(natural %in% c("water"))

water_layer <- generate_polygon_overlay(puijo_water, extent = extent_zoomed, palette = "skyblue2", linecolor = "skyblue2", heightmap = puijo_zoom_mat)

base_map %>% 
  add_overlay(water_layer, alphalayer = 0.8) %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(trails_layer) %>%
  plot_map()


### Other layers -----

# puijo_parking = opq(osm_bbox) %>% 
#   add_osm_feature("parking") %>% 
#   osmdata_sf() 

puijo_building = opq(osm_bbox) %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() 



# puijo_tourism = opq(osm_bbox) %>% 
#   add_osm_feature("tourism") %>% 
#   osmdata_sf() 

# puijo_parking_poly = st_transform(puijo_parking$osm_polygons,crs=crs(puijo))
puijo_building_poly = st_transform(puijo_building$osm_polygons,crs=crs(puijo))
# puijo_tourism_poly = st_transform(puijo_tourism$osm_polygons,crs=crs(puijo))

# puijo_sites_poly = puijo_tourism_poly %>% 
#   filter(tourism %in% c("picnic_site", "camp_site"))

building_layer = generate_polygon_overlay(puijo_building_poly, extent = extent_zoomed,
                                       heightmap = puijo_zoom_mat, palette="grey30")  #%>% 
  # add_overlay(generate_polygon_overlay(puijo_sites_poly, extent = extent_zoomed,
  #                                      heightmap = puijo_zoom_mat, palette="darkgreen"), alphalayer = 0.6)


## Creating final basemap object
final_basemap <- base_map %>% 
  add_overlay(water_layer, alphalayer = 0.8) %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(building_layer) %>%
  add_overlay(trails_layer) 


## Final map with features -----
final_basemap %>% 
  plot_map(title_text = "Puijo Outdoor Center, Kuopio", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)

#### Final 3d map -----
final_basemap %>% 
  plot_3d(puijo_zoom_mat, windowsize=c(1200,800))

render_camera(theta=240,  phi=30, zoom=0.4,  fov=60)
render_snapshot()



## Render movie -----
if(interactive()) {
  filename_movie = "output/puijo.mp4" #tempfile()
  
  #By default, the function produces a 12 second orbit at 30 frames per second, at 30 degrees azimuth.
  # \donttest{
  # montereybay %>%
  #   sphere_shade(texture="imhof1") %>%
  #   plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
  #           waterlinecolor="white", waterlinealpha=0.5)
  base_map %>% 
    add_overlay(water_layer, alphalayer = 0.8) %>% 
    add_overlay(stream_layer, alphalayer = 0.8) %>%
    add_overlay(building_layer) %>%
    add_overlay(trails_layer) %>%
    plot_3d(puijo_zoom_mat, windowsize=c(1200,800))
  #Un-comment the following to run:
  render_movie(filename = filename_movie, zoom = .4, fov = 60)
}
