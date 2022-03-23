### Example for building maps

# https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/
# install.packages("remotes")
# remotes::install_github("tylermorganwall/rayshader")
# remotes::install_github("tylermorganwall/rayimage")
# install.packages("rgl")
# install.packages("magick")

library(magick)
library(rgl)
library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
library(raster)
library(rayshader)

bryce = raster("Bryce_Canyon_GeoTIFF/Bryce_Canyon.tif")
bryce_mat = raster_to_matrix(bryce)


bryce_small = resize_matrix(bryce_mat,0.25)

bryce_small %>% 
  height_shade() %>% 
  plot_map()



bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  plot_map()

bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale = 6),0) %>%
  plot_map()


bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale=6), 0) %>%
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()


bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale=6), 0) %>%
  add_shadow(ambient_shade(bryce_small), 0) %>%
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()


lat_range   = c(37.614998, 37.629084)
long_range = c(-112.174228, -112.156230)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

crs(bryce)


utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(bryce))
utm_bbox


extent(bryce)


extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
bryce_zoom = crop(bryce, extent_zoomed)
bryce_zoom_mat = raster_to_matrix(bryce_zoom)

base_map = bryce_zoom_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(bryce_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_zoom_mat), 0) %>%
  add_shadow(ambient_shade(bryce_zoom_mat),0) %>% 
  add_shadow(texture_shade(bryce_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)


osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

bryce_highway = opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
bryce_highway


bryce_lines = st_transform(bryce_highway$osm_lines, crs=crs(bryce))

ggplot(bryce_lines,aes(color=osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Open Street Map `highway` attribute in Bryce Canyon National Park")



base_map %>% 
  add_overlay(generate_line_overlay(bryce_lines,extent = extent_zoomed,
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()




base_map %>% 
  add_overlay(generate_line_overlay(bryce_lines,extent = extent_zoomed,
                                    linewidth = 3, color="white",
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()


bryce_trails = bryce_lines %>% 
  filter(highway %in% c("path","bridleway"))

bryce_footpaths = bryce_lines %>% 
  filter(highway %in% c("footway"))

bryce_roads = bryce_lines %>% 
  filter(highway %in% c("unclassified", "secondary", "tertiary", "residential", "service"))


base_map %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 6, color="white", 
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 8, color="black",
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()


base_map %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 10, color="black", 
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 8, color="black",
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()



base_map %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 10, color="black", 
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="black", lty=3, offset = c(2,-2),
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 8, color="black",
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()



trails_layer = generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                     linewidth = 10, color="black", 
                                     heightmap = bryce_zoom_mat) %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 6, color="white",
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="black", lty=3, offset = c(2,-2),
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 8, color="grey30",
                                    heightmap = bryce_zoom_mat)) 

bryce_water_lines = opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
bryce_water_lines

bryce_water_lines$osm_lines


bryce_streams = st_transform(bryce_water_lines$osm_lines,crs=crs(bryce)) 

stream_layer = generate_line_overlay(bryce_streams,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = bryce_zoom_mat)

base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()




bryce_parking = opq(osm_bbox) %>% 
  add_osm_feature("parking") %>% 
  osmdata_sf() 

bryce_building = opq(osm_bbox) %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() 

bryce_tourism = opq(osm_bbox) %>% 
  add_osm_feature("tourism") %>% 
  osmdata_sf() 

bryce_parking_poly = st_transform(bryce_parking$osm_polygons,crs=crs(bryce))
bryce_building_poly = st_transform(bryce_building$osm_polygons,crs=crs(bryce))
bryce_tourism_poly = st_transform(bryce_tourism$osm_polygons,crs=crs(bryce))

bryce_sites_poly = bryce_tourism_poly %>% 
  filter(tourism %in% c("picnic_site", "camp_site"))

polygon_layer = generate_polygon_overlay(bryce_parking_poly, extent = extent_zoomed,
                                         heightmap = bryce_zoom_mat, palette="grey30") %>%
  add_overlay(generate_polygon_overlay(bryce_building_poly, extent = extent_zoomed,
                                       heightmap = bryce_zoom_mat, palette="darkred")) %>% 
  add_overlay(generate_polygon_overlay(bryce_sites_poly, extent = extent_zoomed,
                                       heightmap = bryce_zoom_mat, palette="darkgreen"), alphalayer = 0.6)

base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()




bryce_tourism_points = st_transform(bryce_tourism$osm_points,crs=crs(bryce))

bryce_attractions = bryce_tourism_points %>% 
  filter(tourism == "attraction")

base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 1,
                                     seed=1,
                                     heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map()



base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 1, 
                                     halo_color = "white",halo_expand = 5, 
                                     seed=1,
                                     heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map()


base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 1, color = "black",
                                     halo_color = "white", halo_expand = 10, 
                                     halo_blur = 20, halo_alpha = 0.8,
                                     seed=1,
                                     heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map()


base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "white", 
                                     halo_color = "black", halo_expand = 10, 
                                     halo_blur = 20, halo_alpha = 0.8,
                                     seed=1,
                                     heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map()



base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "white", 
                                     halo_color = "black", halo_expand = 10, 
                                     halo_blur = 20, halo_alpha = 0.8,
                                     seed=1, heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map(title_text = "Bryce Canyon National Park, Utah", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)




base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_3d(bryce_zoom_mat, windowsize=c(1200,800))
render_camera(theta=240,  phi=30, zoom=0.3,  fov=60)
render_snapshot()