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
#   filename_movie = tempfile()
#   
#   #You can change to an oscillating orbit. The magnification is increased and azimuth angle set to 30.
#   #A title has also been added using the title_text argument.
#   # \donttest{
#   #Un-comment the following to run:
#   #render_movie(filename = filename_movie, type = "oscillate", 
#   #             frames = 60,  phi = 30, zoom = 0.8, theta = -90,
#   #             title_text = "Monterey Bay: Oscillating")
#   # }             
#   filename_movie = tempfile()
#   
#   #Finally, you can pass your own set of values to the 
#   #camera parameters as a vector with type = "custom".
#   
#   phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
#   phivecfull = c(phivechalf, rev(phivechalf))
#   thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
#   zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
#   zoomvecfull = c(zoomvec, rev(zoomvec))
#   # \donttest{
#   #Un-comment the following to run
#   #render_movie(filename = filename_movie, type = "custom", 
#   #             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
#   rgl::rgl.close()
#   # }
# }
