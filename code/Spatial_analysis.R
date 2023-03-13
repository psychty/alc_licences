
# Alcohol licencing ####
# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'PostcodesioR', 'ggrepel', 'readODS', 'openxlsx','httr', 'rvest', 'flextable')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# base directory
base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'
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

# Compiled data ####

Adur_df <- read_csv(paste0(data_directory, '/Adur_premises.csv')) %>% 
  select(!Licence_number)

Arun_df <- read_csv(paste0(data_directory, '/Arun_premises.csv')) %>% 
  rename(Reference = 'Licence Number',
         Name = 'Trading Name',
         Address = 'Dtf Location') %>% 
  mutate(Reference = as.character(Reference))

Worthing_df <- read_csv(paste0(data_directory, '/Worthing_premises.csv')) %>% 
  rename(Name = 'Trader Name')

combined_df <- Adur_df %>% 
  bind_rows(Arun_df) %>% 
  bind_rows(Worthing_df) %>% 
  filter(!is.na(latitude)) %>% 
  filter(!str_detect(Name, '[Ss]chool')) %>% 
  mutate(`Alcohol OFF Sales/Supply` = ifelse(`Transfer Date Alcohol` %in% c('OFF', 'OFF.', 'ON & OFF'), 'Alcohol OFF Sales/Supply', `Alcohol OFF Sales/Supply`)) %>% 
  mutate(`Alcohol ON Sales/Supply` = ifelse(`Transfer Date Alcohol` %in% c('ON', 'ON.', 'ON & OFF'), 'Alcohol ON Sales/Supply', `Alcohol ON Sales/Supply`)) %>% 
  mutate(`Alcohol ON&OFF Sales/Supply` = ifelse(`Transfer Date Alcohol` %in% c('ON & OFF'), 'Alcohol ON&OFF Sales/Supply', `Alcohol ON&OFF Sales/Supply`))

# We have good info on Adur and Worthing regarding on and off licence sales but not for Arun

# Load boundaries ####
wsx_ltla <- st_read(paste0(output_directory, '/west_sussex_LTLAs.geojson'))

ltla <- st_read(paste0(output_directory, '/west_sussex_LTLAs.geojson')) %>% 
  filter(LAD22NM %in% c('Adur', 'Arun', 'Worthing'))

ltla_spdf <- as_Spatial(ltla)

IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select(LSOA11CD = "LSOA code (2011)", LTLA =  "Local Authority District name (2019)", IMD_2019_Score = "Index of Multiple Deprivation (IMD) Score", IMD_National_Rank = "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", IMD_National_Decile =  "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)" ) %>% 
  mutate(IMD_National_Decile = factor(ifelse(IMD_National_Decile == 1, '10% most deprived',  ifelse(IMD_National_Decile == 2, 'Decile 2',  ifelse(IMD_National_Decile == 3, 'Decile 3',  ifelse(IMD_National_Decile == 4, 'Decile 4',  ifelse(IMD_National_Decile == 5, 'Decile 5',  ifelse(IMD_National_Decile == 6, 'Decile 6',  ifelse(IMD_National_Decile == 7, 'Decile 7',  ifelse(IMD_National_Decile == 8, 'Decile 8',  ifelse(IMD_National_Decile == 9, 'Decile 9',  ifelse(IMD_National_Decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  filter(LTLA %in% areas) %>% 
  arrange(desc(IMD_2019_Score)) %>% 
  mutate(Rank_in_West_Sussex = rank(desc(IMD_2019_Score))) %>% 
  mutate(Decile_in_West_Sussex = abs(ntile(IMD_2019_Score, 10) - 11)) %>% 
  mutate(Decile_in_West_Sussex = factor(ifelse(Decile_in_West_Sussex == 1, '10% most deprived',  ifelse(Decile_in_West_Sussex == 2, 'Decile 2',  ifelse(Decile_in_West_Sussex == 3, 'Decile 3',  ifelse(Decile_in_West_Sussex == 4, 'Decile 4',  ifelse(Decile_in_West_Sussex == 5, 'Decile 5',  ifelse(Decile_in_West_Sussex == 6, 'Decile 6',  ifelse(Decile_in_West_Sussex == 7, 'Decile 7',  ifelse(Decile_in_West_Sussex == 8, 'Decile 8',  ifelse(Decile_in_West_Sussex == 9, 'Decile 9',  ifelse(Decile_in_West_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  group_by(LTLA) %>%
  arrange(desc(IMD_2019_Score)) %>% 
  mutate(Rank_in_LTLA = rank(desc(IMD_2019_Score))) %>% 
  mutate(Decile_in_LTLA = abs(ntile(IMD_2019_Score, 10) - 11)) %>% 
  mutate(Decile_in_LTLA = factor(ifelse(Decile_in_LTLA == 1, '10% most deprived',  ifelse(Decile_in_LTLA == 2, 'Decile 2',  ifelse(Decile_in_LTLA == 3, 'Decile 3',  ifelse(Decile_in_LTLA == 4, 'Decile 4',  ifelse(Decile_in_LTLA == 5, 'Decile 5',  ifelse(Decile_in_LTLA == 6, 'Decile 6',  ifelse(Decile_in_LTLA == 7, 'Decile 7',  ifelse(Decile_in_LTLA == 8, 'Decile 8',  ifelse(Decile_in_LTLA == 9, 'Decile 9',  ifelse(Decile_in_LTLA == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) 


# LSOA 2011 boundaries
lsoa_2011_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_Dec_2011_Boundaries_Full_Extent_BFE_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') 

# Standard Area Measurements - 2011 Census geographies
# https://geoportal.statistics.gov.uk/datasets/standard-area-measurements-2011-for-2011-census-areas-in-england-and-wales-1/about

# Measurements are given in hectares (10,000 square metres) to 2 decimal places.

# Four types of measurements are included: total extent (AREAEHECT), area to mean high water (coastline) (AREACHECT), area of inland water (AREAIHECT) and area to mean high water excluding area of inland water (land area) (AREALHECT). The Eurostat-recommended approach is to use the ‘land area’ measurement to compile population density figures.

# A hectare is equal to 10,000 square meters and 2.471 acres in the British Imperial System. A 100 ha is equal to one square kilometre. 

lsoa_2011_size_df <- read_csv(paste0(data_directory, '/LSOA_measure.csv')) %>% 
  select(LSOA11CD, LSOA11NM, AREALHECT) %>% 
  mutate(Square_kilometre = AREALHECT / 100) %>% 
  filter(LSOA11CD %in% IMD_2019$LSOA11CD)

ltla_size_df <- read_csv(paste0(data_directory, '/LAD_measure_2021.csv')) %>% 
  select(LTLA = LAD21NM, AREALHECT) %>% 
  mutate(Square_kilometre = AREALHECT / 100)

# grab population weight centroids of LSOAs 
lsoa_pop_weighted_centroid <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_PWC_in_England_and_Wales_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>% 
  filter(lsoa11cd %in% IMD_2019$LSOA11CD)

# Convert it to a spatial polygon data frame
lsoa_2011_boundaries_spdf <-  as_Spatial(lsoa_2011_sf, IDs = lsoa_2011_sf$LSOA11CD) %>% 
  filter(LSOA11CD %in% IMD_2019$LSOA11CD) %>% 
  arrange(LSOA11CD) %>% 
  left_join(IMD_2019, by = 'LSOA11CD')

imd_colours <- c("#0000FF","#2080FF","#40E0FF","#70FFD0","#90FFB0","#C0E1B0","#E0FFA0","#E0FF70","#F0FF30","#FFFF00")

imd_palette <- colorFactor(imd_colours,
                           levels = levels(lsoa_2011_boundaries_spdf$IMD_National_Decile))

# https://www.gov.uk/government/publications/alcohol-licensing-data-for-public-health-teams

# Plotting points ####
# Turn the premises data into spatial points data

Premises_sp <- combined_df
coordinates(Premises_sp) <- c("longitude","latitude")
as(Premises_sp,"SpatialPoints")
proj4string(Premises_sp) <- CRS("+proj=longlat +datum=WGS84")

Premises_spdf <- cbind(Premises_sp, over(Premises_sp,lsoa_2011_boundaries_spdf))

Premises_final_df <- Premises_spdf %>% 
  as.data.frame()

# Premises_final_df %>% 
#   group_by(LTLA) %>% 
#   summarise(Outlets = n()) %>% 
#   left_join(ltla_size_df, by = 'LTLA') %>% 
#   mutate(Outlets_per_sqkm = Outlets / Square_kilometre) %>%
#   select(!AREALHECT) %>% 
#   flextable() %>% 
#   print( preview = "docx")

# Premises_final_df %>%
#   group_by(LSOA11CD) %>%
#   summarise(Outlets = n()) %>%
#   left_join(lsoa_2011_size_df, by = 'LSOA11CD') %>%
#   mutate(Outlets_per_sqkm = Outlets / Square_kilometre) %>%
#   select(!AREALHECT) %>%
#   arrange(desc(Outlets_per_sqkm)) %>% 
#   select(LSOA11CD, LSOA11NM, Outlets, Square_kilometre, Outlets_per_sqkm) %>% 
#   flextable() %>%
#   print( preview = "docx")

Local_districts <- ltla %>% 
  filter(LAD22NM %in% unique(Premises_final_df$LTLA))

Local_LSOA <- lsoa_2011_boundaries_spdf %>% 
  filter(LTLA %in% unique(Premises_final_df$LTLA))

wsx_LSOA  <- lsoa_2011_boundaries_spdf %>% 
  filter(LTLA %in% unique(wsx_ltla$LTLA))

# Just plot the dots

leaflet() %>% 
  addControl(paste0("<font size = '1px'><b>West Sussex Licenced Premises:</b><br>Data correct as at March 2023;</font>"),
             position = 'topright') %>%
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
           attribution = paste0('&copy; <a href=https://www.openstreetmap.org/copyright>OpenStreetMap</a> contributors &copy; <a href=https://carto.com/attributions>CARTO</a><br>Contains OS data ? Crown copyright and database right 2021<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an circle to find out more.')) %>%
  addPolygons(data = lsoa_2011_boundaries_spdf,
              stroke = TRUE, 
              color = "#000000",
              fillColor = ~imd_palette(IMD_National_Decile),
              fillOpacity = .5,
              weight = 1,
              popup = paste0('LSOA 2011: ', lsoa_2011_boundaries_spdf$LSOA11NM, ' (', lsoa_2011_boundaries_spdf$LSOA11CD, ')'),
              group = 'Show neighbourhood deprivation')

lsoa_2011_boundaries_spdf %>% 
  filter(LSOA11CD %in% c('E01031476', 'E01031348')) %>% 
  bbox()

# Library
library(ggmap)

Local_districts_fortified <- ltla_spdf %>% 
  broom::tidy()

# For google map, you have to give the center of the window you are looking at.
# Possibility for the map type argument: terrain / satellite / roadmap / hybrid

bbox_final <- lsoa_2011_boundaries_spdf %>% 
  filter(LSOA11CD %in% c('E01031476', 'E01031348', 'E01031524', 'E01031770')) %>% 
  bbox()

#bbox_final <- bbox(as_Spatial(terrainr::add_bbox_buffer(ltla, 10000)))

# ("terrain", "terrain-background", "terrain-labels", "terrain-lines",
#   "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
#   "toner-labels", "toner-lines", "toner-lite", "watercolor"),

map_sat <- get_stamenmap(
  bbox = bbox_final, 
  zoom = 10, 
  maptype = "terrain")

First_map <- ggmap(map_sat) +
  map_theme() +
    coord_fixed(1.5) +
    map_theme() +
    geom_polygon(data = Local_districts_fortified,
                 aes(x=long,
                     y=lat,
                     group = group),
                 fill = NA,
                 color="#ff3f99",
                 linewidth = .25,
                 alpha = 1,
                 show.legend = TRUE) +
    labs(title = paste0('Premises licenses'),
         subtitle = paste0('Adur, Arun, and Worthing premises; Map produced ', format(Sys.Date(), '%d %B %Y')),
         caption = paste0('Data from each licensing authority cover different time periods.'))  +
  geom_point(data = Premises_final_df,
             aes(x = longitude,
                 y = latitude),
             shape = 21,
             size = 2,
             colour = '#ffffff',
             fill = 'maroon') 
  
svg(paste0(output_directory,'/First_map.svg'),
    width = 6,
    height = 4)
print(First_map)
dev.off()

Adur_Worthing_premises <- Premises_final_df %>% 
  filter(LTLA %in% c('Adur', 'Worthing')) %>% 
  mutate(ON_Sales = ifelse(is.na(Alcohol.ON.Sales.Supply), 'No', 'Yes')) %>% 
  mutate(OFF_Sales = ifelse(is.na(Alcohol.OFF.Sales.Supply), 'No', 'Yes')) %>% 
  mutate(ON_OFF_Sales = ifelse(is.na(Alcohol.ON.OFF.Sales.Supply), 'No', 'Yes')) %>% 
  mutate(ON_Sales = ifelse(ON_OFF_Sales == 'Yes', 'Yes', ON_Sales)) %>% 
  mutate(OFF_Sales = ifelse(ON_OFF_Sales == 'Yes', 'Yes', OFF_Sales)) %>% 
  mutate(ON_OFF_Sales = ifelse(ON_Sales == 'Yes' & OFF_Sales == 'Yes', 'Yes', ON_OFF_Sales)) %>%   
  mutate(Licence_type = ifelse(ON_OFF_Sales == 'Yes', 'Alcohol ON&OFF Sales/Supply', ifelse(OFF_Sales == 'Yes', 'Alcohol OFF Sales/Supply', ifelse(ON_Sales == 'Yes', 'Alcohol ON Sales/Supply', NA)))) %>% 
  filter(!is.na(Licence_type))

Premises_final_df %>% 
  filter(LTLA %in% c('Adur', 'Worthing')) %>% 
  nrow()

Adur_Worthing_premises %>% 
  nrow()

lsoa_2011_boundaries_spdf %>%
  filter(LSOA11CD %in% c('E01031426', 'E01031348',  'E01031617'))  %>% plot()

Adur_Worthing_LTLA <- lsoa_2011_boundaries_spdf %>%
  filter(LSOA11CD %in% c('E01031426', 'E01031348', 'E01031412','E01031617')) %>%
  bbox()

Adur_Worthing_LTLA <- ltla_spdf %>% 
  filter(LAD22NM %in% c('Adur', 'Worthing')) %>% 
  bbox()

map_sat_2 <- get_stamenmap(
  bbox = Adur_Worthing_LTLA, 
  zoom = 12, 
  maptype = "terrain")

Adur_Worthing_fortified <- ltla_spdf %>% 
  filter(LAD22NM %in% c('Adur', 'Worthing')) %>% 
  broom::tidy()

Second_map <- ggmap(map_sat_2) +
  map_theme() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = Adur_Worthing_fortified,
               aes(x=long,
                   y=lat,
                   group = group),
               fill = NA,
               color="#ff3f99",
               linewidth = .25,
               alpha = 1,
               show.legend = FALSE) +
  labs(title = paste0('Premises licenses by alcohol on/off sales'),
       subtitle = paste0('Adur and Worthing premises; Map produced ', format(Sys.Date(), '%d %B %Y')),
       caption = paste0('Data from each licensing authority cover different time periods.'))  +
  geom_point(data = Adur_Worthing_premises,
             aes(x = longitude,
                 y = latitude,
                 fill = Licence_type),
             shape = 21,
             size = 3,
             colour = '#ffffff') + 
  scale_fill_manual(values = c('#0f6180', 'orange', '#BC066A'),
                    name = 'Alcohol licence type') +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  

svg(paste0(output_directory,'/Second_map.svg'),
    width = 6,
    height = 4)
print(Second_map)
dev.off()

# Adur_Worthing_premises %>%
#   group_by(LTLA) %>%
#   summarise(Outlets = n()) %>%
#   left_join(ltla_size_df, by = 'LTLA') %>%
#   mutate(Outlets_per_sqkm = Outlets / Square_kilometre) %>%
#   select(!AREALHECT) %>%
#   flextable() %>%
#   print( preview = "docx")

Adur_Worthing_premises %>%
  group_by(LSOA11CD) %>%
  summarise(Outlets = n()) %>%
  left_join(lsoa_2011_size_df, by = 'LSOA11CD') %>%
  mutate(Outlets_per_sqkm = Outlets / Square_kilometre) %>%
  select(!AREALHECT) %>%
  arrange(desc(Outlets_per_sqkm)) %>%
  select(LSOA11CD, LSOA11NM, Outlets, Square_kilometre, Outlets_per_sqkm) %>%
  head(10) %>% 
  flextable() %>%
  print( preview = "docx")





# Richardson et al (2015) explored the densities of ON and OFF licenced premises

# Simple measures of premises per square km (at various geographie) implicitly assume that the population of an area is evenly distributed and that residents are nnafected by outlets outside of their immediate geographical boundaries (Richardson et al, 2015).

# Instead Richardson et al (2015) proposed a method of calculating the density of outlets around a single point for each area representing the population weighted centroid to capture the alcohol retail environment exposed to the majority of a population.

Adur_Worthing_premises %>% 
  group_by(IMD_National_Decile) %>% 
  summarise(Outlets = n()) %>% 
  ungroup() %>% 
  mutate(Proportion = Outlets / sum(Outlets))

Adur_Worthing_premises %>% 
  group_by(Decile_in_West_Sussex) %>% 
  summarise(Outlets = n())%>% 
  ungroup() %>% 
  mutate(Proportion = Outlets / sum(Outlets))


# Use kernel density estimation

#https://bookdown.org/lexcomber/brunsdoncomber2e/Ch6.html
#https://rpubs.com/Dr_Gurpreet/kernel_smoothing_R
#https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html

leaf_map_1 <- leaflet() %>% 
  addControl(paste0("<font size = '1px'><b>West Sussex Licenced Premises:</b><br>Data correct as at March 2023;</font>"),
             position = 'topright') %>%
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
           attribution = paste0('&copy; <a href=https://www.openstreetmap.org/copyright>OpenStreetMap</a> contributors &copy; <a href=https://carto.com/attributions>CARTO</a><br>Contains OS data ? Crown copyright and database right 2021<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an circle to find out more.')) %>%
  addMapPane("heatmap", zIndex = 430) %>% # shown below ames_circles
  addMapPane("points", zIndex = 420) %>% # shown above ames_lines
  addPolygons(data = subset(lsoa_2011_boundaries_spdf, LTLA %in% c('Adur', 'Arun','Worthing')),
              stroke = TRUE, 
              color = "#000000",
              fillColor = ~imd_palette(IMD_National_Decile),
              fillOpacity = .5,
              weight = 1,
              popup = paste0('LSOA 2011: ', lsoa_2011_boundaries_spdf$LSOA11NM, ' (', lsoa_2011_boundaries_spdf$LSOA11CD, ')'),
              group = 'Show neighbourhood deprivation') %>% 
  addPolygons(data = ltla,
              fill = FALSE,
              stroke = TRUE,
              color = 'maroon') %>% 
  addCircleMarkers(lng = combined_df$longitude,
                   lat = combined_df$latitude,
                   label = paste0(combined_df$Name),
                   color = 'purple',
                   radius = 4,
                   fillOpacity = 1,
                   stroke = FALSE,
                   group = 'Show premises',
                   options = pathOptions(pane = "points")) %>% 
  addHeatmap(lng = combined_df$longitude,
             lat = combined_df$latitude,
             intensity = 5,
             blur = 20, 
             max = 1, 
             radius = 20,
             group = 'Show density of premises') %>% 
  addLegend(position = 'bottomright',
            colors = imd_colours,
            labels = levels(lsoa_2011_boundaries_spdf$IMD_National_Decile),
            title = 'Deprivation decile<br>(national ranks)',
            opacity = 1) %>% 
  addLayersControl(overlayGroups = c('Show neighbourhood deprivation', 'Show premises', 'Show density of premises'),
                   options = layersControlOptions(collapsed = FALSE))

