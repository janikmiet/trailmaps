
## Render rmd and 
rmarkdown::render("trailmaps_kuopio.Rmd", output_dir = "output_website", output_file = "index.html")

## Move to kapsi ----
if(TRUE){
  system("scp -r ./output_website/* janikmiet@kapsi.fi:/home/users/janikmiet/sites/research.janimiettinen.fi/www/material/trailmaps")
  system("scp -r ./img/* janikmiet@kapsi.fi:/home/users/janikmiet/sites/research.janimiettinen.fi/www/material/trailmaps/img")
}

