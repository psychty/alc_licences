
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'spdplyr', 'geojsonio', 'jsonlite', 'sf', 'leaflet', 'htmlwidgets', 'PostcodesioR', 'osrm', 'viridis', 'osmdata')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# base directory
#base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'
base_directory <- '~/Repositories/alc_licences/'

list.files(base_directory)

data_directory <- paste0(base_directory, 'data')
output_directory <- paste0(base_directory, 'outputs')

# visualising outputs
map_theme = function(){
  theme( 
    legend.position = "bottom", 
    legend.key.size = unit(.75,"line"),
    legend.title = element_text(size = 8, face = 'bold'),
    plot.background = element_blank(), 
    plot.title.position = "plot",
    panel.background = element_blank(),  
    panel.border = element_blank(),
    axis.text = element_blank(), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 11), 
    plot.subtitle = element_text(colour = "#000000", size = 10), 
    axis.title = element_blank(),     
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "white"), 
    strip.background = element_rect(fill = "#ffffff"), 
    axis.ticks = element_blank()
  ) 
}

# Choose an area
# Load LTLA boundaries #
lad_boundaries_clipped_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFC_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
  filter(LAD22NM %in% c('Chichester'))

lad_boundaries_full_extent_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFE_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
  filter(LAD22NM %in% areas & LAD22NM != 'Chichester')

lad_boundaries_sf <- rbind(lad_boundaries_clipped_sf, lad_boundaries_full_extent_sf)
lad_boundaries_spdf <- as_Spatial(lad_boundaries_sf, IDs = lad_boundaries_sf$LAD22NM)

#geojson_write(ms_simplify(geojson_json(lad_boundaries_spdf), keep = 0.3), file = paste0(output_directory, '/west_sussex_LTLAs.geojson'))

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = lad_boundaries_sf,
              fill = NA,
              weight = 3,
              opacity = 1)

# Perfect, this will help for presentation purposes.

# We need to get a bounding box to use in OSM extraction. If you use base bbox() function it needs to be on a spatial polygons dataframe version

# At the moment the object is sf format (spatial features), but we need to turn it into a spatial polygons dataframe to plot more easily with Leaflet

area_bb <- bbox(lad_boundaries_spdf)

# The main function is opq( ) which build the final query. We add our filter criteria with the add_osm_feature( ) function. In this first query we will look for cinemas. 

# https://github.com/ropensci/osmdata

q1 <- area_bb %>% 
  opq() %>% 
  add_osm_feature(key = 'amenity', 
                  value = c('bar', 'pub', 'restaurant', 'nightclub')) %>% 
  osmdata_sf() # converts the results into an sf object

# The extract takes a long time, so it is worth saving locally and running only when you need to update it.
# The following will only run if the data file does not exist.
if(file.exists(paste0(output_directory, '/wsx_OSM_bar_club_spdf.geojson'))!= TRUE){

# At the moment the object is sf format (spatial features), but we need to turn it into a spatial points dataframe
# We also only want to keep the points that fall within the polygons (the bounding box is a square/rectangle shape and may include some points outside of the ares)
bar_club_spdf <- st_intersection(lad_boundaries_sf, q1$osm_points) %>% 
  as_Spatial() 

# This is a bit messy as some data entries have 'name' filled in, and others do not. You may have to make some decisions on how to clean up the data.
geojson_write(geojson_json(bar_club_spdf), file = paste0(output_directory, '/wsx_OSM_bar_club_spdf.geojson'))
}

bar_club_spdf@data %>% View()

# We could also look at key - shop, value = alcohol, 

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = lad_boundaries_spdf,
              fill = NA,
              weight = 2,
              color = '#000000') %>%
  addCircleMarkers(data = bar_club_spdf,
             fillColor = 'maroon',
             fillOpacity = 1,
             color = '#ffffff',
             opacity = 1,
             weight = 1,
             radius = 8,
             label = ~name,
             popup = paste0('<Strong>', bar_club_spdf$name, '</Strong><br><br>', bar_club_spdf$addr.place, '<br>', bar_club_spdf$addr.postcode))

# What else is there?
# https://wiki.openstreetmap.org/wiki/Map_features

# OSM
available_features()

available_tags("amenity")

# Lets look for gambling settings ####
q2 <- area_bb %>% 
  opq() %>% 
  add_osm_feature(key = 'amenity', 
                  value = c('gambling', 'casino')) %>% 
  osmdata_sf() 

# At the moment I think I need to search separately for betting shops, as the key is 'shop', not 'amenity' and I do not know how to combine them
q3 <- area_bb %>% 
  opq() %>% 
  add_osm_feature(key = 'shop', 
                  value = c('bookmaker', 'lottery')) %>% 
  osmdata_sf() 

all_shop_tags <- available_tags('shop')
# all_shop_tags %>% View() 

gambling_spdf <- st_intersection(lad_boundaries_sf, q2$osm_points) %>% 
  as_Spatial() 

betting_spdf <- st_intersection(lad_boundaries_sf, q3$osm_points) %>% 
  as_Spatial() 

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = wsx_spdf,
              fill = NA,
              weight = 1) %>%
  # addCircleMarkers(data = cinema_spdf,
  #                  fillColor = 'maroon',
  #                  fillOpacity = 1,
  #                  color = '#ffffff',
  #                  opacity = 1,
  #                  weight = 1,
  #                  radius = 8,
  #                  label = ~name,
  #                  popup = paste0('<Strong>', cinema_spdf$name, '</Strong><br><br>', cinema_spdf$addr.place, '<br>', cinema_spdf$addr.postcode)) %>% 
  addCircleMarkers(data = gambling_spdf,
                   fillColor = 'purple',
                   fillOpacity = 1,
                   color = '#ffffff',
                   opacity = 1,
                   weight = 1,
                   radius = 8,
                   label = ~name,
                   popup = paste0('<Strong>', gambling_spdf$name, '</Strong><br><br>', gambling_spdf$addr.place, '<br>', gambling_spdf$addr.street, '<br>', gambling_spdf$addr.postcode)) %>% 
  addCircleMarkers(data = betting_spdf,
                   fillColor = 'orange',
                   fillOpacity = 1,
                   color = '#ffffff',
                   opacity = 1,
                   weight = 1,
                   radius = 8,
                   label = ~name,
                   popup = paste0('<Strong>', betting_spdf$name, '</Strong><br><br>', betting_spdf$addr.place, '<br>', betting_spdf$addr.street, '<br>', betting_spdf$addr.postcode)) 

# Alternative querying - all OSM points of interest within a radius of a location ####

# This isn't very successful below, I need to spend some time understanding the outputs.

# osmdata::opq_around()
# 
# location_x <- postcode_lookup('')
# 
# poi_around_location <- opq_around(lon = location_x$longitude,
#            lat = location_x$latitude,
#            radius = 100,# This is metres radius (so 1,000 is a kilometre)
#            timeout = 60) %>%  
#   osmdata_sf()
# 
# 
# poi_polygons <- as_Spatial(poi_around_location$osm_polygons)
# 
# poi_polylines <- as_Spatial(poi_around_location$osm_lines)
# 
# leaflet() %>% 
#   addTiles() %>%
#   addPolygons(data = poi_polygons,
#               color = "#03F",
#               weight = 5,
#               opacity = 0.5,
#               fill = TRUE,
#               fillColor = 'green',
#               fillOpacity = 1) %>% 
#   addPolygons(data = poi_polylines,
#               color = "#03F",
#               weight = 5,
#               opacity = 0.5,
#               fill = TRUE,
#               fillColor = 'green',
#               fillOpacity = 1)

