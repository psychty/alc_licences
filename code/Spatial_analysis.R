
# Alcohol licencing ####
# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'PostcodesioR', 'ggrepel', 'readODS', 'openxlsx','httr', 'rvest', 'flextable')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# base directory
base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'
#base_directory <- '~/Repositories/alc_licences/'

list.files(base_directory)

data_directory <- paste0(base_directory, '/data')
output_directory <- paste0(base_directory, '/outputs')

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
  filter(!str_detect(Name, '[Ss]chool'))

# Load boundaries ####

ltla <- st_read(paste0(output_directory, '/west_sussex_LTLAs.geojson')) %>% 
  filter(LAD22NM %in% c('Adur', 'Arun', 'Worthing'))

IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select(LSOA11CD = "LSOA code (2011)", LTLA =  "Local Authority District name (2019)", IMD_2019_Score = "Index of Multiple Deprivation (IMD) Score", IMD_National_Rank = "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", IMD_National_Decile =  "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)" ) %>% 
  mutate(IMD_National_Decile = factor(ifelse(IMD_National_Decile == 1, '10% most deprived',  ifelse(IMD_National_Decile == 2, 'Decile 2',  ifelse(IMD_National_Decile == 3, 'Decile 3',  ifelse(IMD_National_Decile == 4, 'Decile 4',  ifelse(IMD_National_Decile == 5, 'Decile 5',  ifelse(IMD_National_Decile == 6, 'Decile 6',  ifelse(IMD_National_Decile == 7, 'Decile 7',  ifelse(IMD_National_Decile == 8, 'Decile 8',  ifelse(IMD_National_Decile == 9, 'Decile 9',  ifelse(IMD_National_Decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  filter(LTLA %in% areas) %>% 
  arrange(desc(IMD_2019_Score)) %>% 
  mutate(Rank_in_West_Sussex = rank(desc(IMD_2019_Score))) %>% 
  mutate(Decile_in_West_Sussex = abs(ntile(IMD_2019_Score, 10) - 11)) %>% 
  mutate(Decile_in_West_Sussex = factor(ifelse(Decile_in_West_Sussex == 1, '10% most deprived',  ifelse(Decile_in_West_Sussex == 2, 'Decile 2',  ifelse(Decile_in_West_Sussex == 3, 'Decile 3',  ifelse(Decile_in_West_Sussex == 4, 'Decile 4',  ifelse(Decile_in_West_Sussex == 5, 'Decile 5',  ifelse(Decile_in_West_Sussex == 6, 'Decile 6',  ifelse(Decile_in_West_Sussex == 7, 'Decile 7',  ifelse(Decile_in_West_Sussex == 8, 'Decile 8',  ifelse(Decile_in_West_Sussex == 9, 'Decile 9',  ifelse(Decile_in_West_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %in% 
  group_by(LTLA) %in%
  arrange(desc(IMD_2019_Score)) %>% 
  mutate(Rank_in_LTLA = rank(desc(IMD_2019_Score))) %>% 
  mutate(Decile_in_LTLA = abs(ntile(IMD_2019_Score, 10) - 11)) %>% 
  mutate(Decile_in_LTLA = factor(ifelse(Decile_in_LTLA == 1, '10% most deprived',  ifelse(Decile_in_LTLA == 2, 'Decile 2',  ifelse(Decile_in_LTLA == 3, 'Decile 3',  ifelse(Decile_in_LTLA == 4, 'Decile 4',  ifelse(Decile_in_LTLA == 5, 'Decile 5',  ifelse(Decile_in_LTLA == 6, 'Decile 6',  ifelse(Decile_in_LTLA == 7, 'Decile 7',  ifelse(Decile_in_LTLA == 8, 'Decile 8',  ifelse(Decile_in_LTLA == 9, 'Decile 9',  ifelse(Decile_in_LTLA == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) 

# LSOA 2011 boundaries
lsoa_2011_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') 

# Standard Area Measurements - 2011 Census geographies
# https://geoportal.statistics.gov.uk/datasets/standard-area-measurements-2011-for-2011-census-areas-in-england-and-wales-1/about

# TODO Create tables for LSOAs and LTLAs (area - km squared)

# TODO grab population weight centroids of LSOAs ####

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

# Richardson et al (2015) explored the densities of ON and OFF licenced premises

# Simple measures of premises per square km (at various geographie) implicitly assume that the population of an area is evenly distributed and that residents are nnafected by outlets outside of their immediate geographical boundaries (Richardson et al, 2015).

# Instead Richardson et al (2015) proposed a method of calculating the density of outlets around a single point for each area representing the population weighted centroid to capture the alcohol retail environment exposed to the majority of a population.

Premises_final_df %>% 
  group_by(Decile_in_West_Sussex) %>% 
  summarise(Outlets = n())


# Use kernel density estimation

#https://bookdown.org/lexcomber/brunsdoncomber2e/Ch6.html
#https://rpubs.com/Dr_Gurpreet/kernel_smoothing_R
#https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html





leaflet() %>% 
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
