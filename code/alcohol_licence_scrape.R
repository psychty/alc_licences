
# Alcohol licencing ####
# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'PostcodesioR', 'ggrepel', 'readODS', 'openxlsx', 'pdftools', 'httr', 'rvest', 'strex')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

# base directory
base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'
base_directory <- '~/Repositories/alc_licences/'

list.files(base_directory)

data_directory <- paste0(base_directory, 'data')
output_directory <- paste0(base_directory, 'outputs')

# To see what is in the directory already
list.files(data_directory)

if(file.exists(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods')) == FALSE){
download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1109599/alcohol-late-night-licensing-tables-2022.ods',
              paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
              mode = 'wb')
}

# premises_24_hour_licence_df_raw <- read_ods(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
#                              sheet = 'Table_4',
#                              skip = 7) 
# 
# latest_total_24_hour_licenced_premises <- premises_24_hour_licence_df_raw %>%
#   filter(`Licensing authority` %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham',  'Mid Sussex', 'Worthing')) %>% 
#   filter(`Year \n(as at 31 March)` == '2021/22') 

premises_licences <- read_ods(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
         sheet = 'Table_2',
         skip = 7)

latest_premises_licences <- premises_licences %>%
  filter(`Licensing authority` %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham',  'Mid Sussex', 'Worthing')) %>% 
  filter(`Year \n(as at 31 March)` == '2021/22') 

# These figures are the figures I'm aiming for.

# Adur ####

# We can try to scrape the pdfs? Get all the href tags on the page for Adur and Worthing
calls_premises_webpage <- read_html('https://www.adur-worthing.gov.uk/licensing-and-permits/consultations-policy-forum/completed-applications/') %>%
  html_nodes("a") %>%
  html_attr("href")

# Return any of the tags with pdf in the name. These are not particularly helpfully named
pdf_strings <- unique(grep('pdf', calls_premises_webpage, value = T))
pdf_strings
# The first link of the page is the personal licences register
# Then there are files for each letter of the alphabet (with xyz together) 

# I used the following to understand how pdf_text was working - https://bookdown.org/Maxine/r4ds/pdf.html
# Also took the approaches in here to find starting and ending points for extraction - https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/
# I stopped at turning it into a function (Dont H8 me Matt)

# I want to loop through the list so lets start with the first premises pdf file link (link number 2)
i = 2
txt_x <- pdf_text(paste0("https://www.adur-worthing.gov.uk", pdf_strings[i]))

# Each string in the vector contains a plain text version of the text on that page.
# I think pdf_text() is reading in everything as a long string, we split it by lines
# str_split(txt_x, "\n", simplify = TRUE)

# I discovered that the Adur licence register is usually three pages (not always) but at the top of each new licence is the word 'Licensing Act 2003', and I know that the details of every new licence will start on that row.

# In this first code chunk I add a new ID (from row_number()) 
table <- as.data.frame(str_split(txt_x, "\n", simplify = TRUE)) %>% 
  group_by(V9) %>% 
  mutate(Licence_number = ifelse(V9 != '  Licensing Act 2003', NA, row_number())) %>%
  ungroup() %>% 
  fill(Licence_number, .direction = 'down') %>% # every time i see the value 'Licensing Act 2003' then back fill (so all subsequent empty rows get the same ID up until the next observation of ID)
  filter(V9 == '  Licensing Act 2003') %>% # Now I can just keep the row with Licensing Act 2003 in the V9 field, the name and address are avialable in these.
  select(Licence_number,
         Name = V6,
         Address = V11) %>% 
  mutate(spaces = str_count(Address, ' ')) %>% # We need to do a bit of wrangling to get the postcode out of it
  mutate(Postcode = str_after_nth(Address, " ", spaces - 1)) %>% # This function is from strex package
  select(!spaces) %>% 
  mutate(Reference = str_trim(str_after_nth(Name, "Reference", 1), side = 'both')) %>% 
  mutate(Name = str_trim(str_before_first(Name, 'Reference')))

# That worked, so lets get it into a loop

# I have also used the same method to grab information on whether the licence has condition of Alcohol On Sale, Alcohol Off sale, or both, and then exclude those which dont have any (because you can have a premises licence for late night openning and club entertainment without alcohol involved (e.g. scouts ))
 
for(i in 2:25){
  if(i == 2){Premises_table <- data.frame(Name = character(), Licence_number = numeric(), Address = character(), Postcode = character(), Reference = character())
  }
  
  txt_x <- pdf_text(paste0("https://www.adur-worthing.gov.uk", pdf_strings[i]))
  
  table <- as.data.frame(str_split(txt_x, "\n", simplify = TRUE)) %>% 
    group_by(V9) %>% 
    mutate(Licence_number = ifelse(V9 != '  Licensing Act 2003', NA, row_number())) %>%
    ungroup() %>% 
    fill(Licence_number, .direction = 'down') %>% 
    filter(V9 == '  Licensing Act 2003') %>% 
    select(Licence_number,
           Name = V6,
           Address = V11) %>% 
    mutate(spaces = str_count(Address, ' ')) %>% 
    mutate(Postcode = str_after_nth(Address, " ", spaces - 1)) %>% 
    select(!spaces) %>% 
    mutate(Reference = str_trim(str_after_nth(Name, "Reference", 1), side = 'both')) %>% 
    mutate(Name = str_trim(str_before_first(Name, 'Reference')))
  
  table_v2 <- as.data.frame(str_split(txt_x, "\n", simplify = TRUE)) %>% 
    group_by(V9) %>% 
    mutate(Licence_number = ifelse(V9 != '  Licensing Act 2003', NA, row_number())) %>%
    ungroup() %>% 
    fill(Licence_number, .direction = 'down') 
  
  Off_sales <- table_v2 %>% 
    filter_all(any_vars(str_detect(., 'Alcohol OFF Sales/Supply'))) %>% 
    select(Licence_number) %>% 
    mutate(`Alcohol OFF Sales/Supply` = 'Alcohol OFF Sales/Supply') %>% 
    unique()
  
  On_sales <- table_v2 %>% 
    filter_all(any_vars(str_detect(., 'Alcohol ON Sales/Supply'))) %>% 
    select(Licence_number)  %>% 
    mutate(`Alcohol ON Sales/Supply` = 'Alcohol ON Sales/Supply') %>% 
    unique()
  
  Off_and_On_sales <- table_v2 %>% 
    filter_all(any_vars(str_detect(., 'Alcohol ON&OFF Sales/Supply'))) %>% 
    select(Licence_number)  %>% 
    mutate(`Alcohol ON&OFF Sales/Supply` = 'Alcohol ON&OFF Sales/Supply') %>% 
    unique()
  
  Late_night_refresh <- table_v2 %>% 
    filter_all(any_vars(str_detect(., 'Late Night Refresh'))) %>% 
    select(Licence_number)  %>% 
    mutate(`Late Night Refresh` = 'Late Night Refresh') %>% 
    unique()
  
  table <- table %>% 
    left_join(Off_sales, by = 'Licence_number') %>% 
    left_join(On_sales, by = 'Licence_number') %>% 
    left_join(Off_and_On_sales, by = 'Licence_number') %>% 
    left_join(Late_night_refresh, by = 'Licence_number') 
  
  Premises_table <- table %>% 
    bind_rows(Premises_table) %>% 
    filter(Postcode != 'West Sussex') %>% 
    mutate(Postcode = ifelse(Postcode == 'BN43 6GT', 'BN43 6GQ', Postcode))
  
  rm(Off_sales, Off_and_On_sales, On_sales, Late_night_refresh, table, table_v2)
  
}

for(i in 1:length(unique(Premises_table$Postcode))){
  if(i == 1){lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double())
  }
  
  lookup_result_x <- postcode_lookup(unique(Premises_table$Postcode)[i]) %>% 
    select(postcode, longitude, latitude)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result) 
  
}

Premises_table <- Premises_table %>% 
  left_join(lookup_result, by = c('Postcode' = 'postcode'))

Premises_table %>% 
  write.csv(., paste0(data_directory, '/Adur_premises.csv'), row.names = FALSE)

Premises_table %>% 
  filter(is.na(`Alcohol OFF Sales/Supply`) & is.na(`Alcohol ON Sales/Supply`) & is.na(`Alcohol ON&OFF Sales/Supply`))


Adur_premises <- read_csv(paste0(data_directory, '/Adur_premises.csv'))%>% 
  filter(!(is.na(`Alcohol OFF Sales/Supply`) & is.na(`Alcohol ON Sales/Supply`) & is.na(`Alcohol ON&OFF Sales/Supply`)))

# This is spot on the number of premises included in 2021/22

# Arun ####

Arun_premises <- read_csv(paste0(data_directory, '/Arun_current_Premises_licence_register_extracted_02_03_2023.csv')) %>% 
  mutate(spaces = str_count(`Dtf Location`, ' ')) %>% 
  mutate(spaces = replace_na(spaces, 0)) %>% 
  mutate(Postcode = ifelse(`Licence Number` == 6883, 'BN18 9AB', NA)) %>% 
  mutate(Postcode = ifelse(is.na(Postcode), str_after_nth(`Dtf Location`, ' ', spaces - 1), Postcode)) %>% 
  select(!spaces) 

Arun_postcodes_to_lookup <- Arun_premises %>% 
  select(Postcode) %>% 
  filter(Postcode != 'West Sussex') %>% 
  unique()


for(i in 1:length(unique(Arun_postcodes_to_lookup$Postcode))){
  if(i == 1){lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double())
  }
  
  postcode_x <- unique(Arun_postcodes_to_lookup$Postcode)[i]
  
  lookup_result_x <- postcode_lookup(postcode_x) %>% 
    select(postcode, longitude, latitude)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result) 
  
}

lookup_result <- lookup_result %>% 
  bind_rows(data.frame(postcode = 'BN18 9AF', longitude = -0.553723, latitude = 50.85448)) %>% 
  bind_rows(data.frame(postcode = 'BN17 5AY', longitude =  -0.543336, latitude = 50.80887)) %>% 
  unique()

Arun_premises <- Arun_premises %>% 
  left_join(lookup_result, by = c('Postcode' = 'postcode'))

Arun_premises %>% 
  write.csv(., paste0(data_directory, '/Arun_premises.csv'), row.names = FALSE)

# Chichester ####
# Difficult

# Crawley ####

# We can try to scrape the site but it is a javascript card type search function with no easy way to download all results 
calls_premises_webpage <- read_html('https://crawley.gov.uk/business/licensing/licences-register') %>%
  html_nodes("a") %>%
  html_attr("href")

# Horsham ####
# Difficult

# Worthing ####

# And this was the day I conceded that we do need Excel for some things.

Worthing_premises <- read_csv(paste0(data_directory, '/Worthing_premises.csv')) %>% 
  rename(Address = 'Premises Address') %>% 
  mutate(spaces = str_count(Address, ' ')) %>% 
  mutate(Postcode = gsub('\\.', '', str_after_nth(Address, " ", spaces - 1))) %>% 
  mutate(Postcode = ifelse(`Trader Name` == 'Green Man Ale & Cider House', 'BN14 7PA', ifelse(`Trader Name` == 'Downsbrook Primary School', 'BN14 8GD', ifelse(`Trader Name` == "St Mary's R.C. Primary School", 'BN11 4BD', ifelse(Postcode == 'BN13 1PR02/03/2006', 'BN13 1PR', ifelse(`Trader Name` == 'Our Lady of Sion Junior School', 'BN11 1RE', Postcode)))))) %>% 
  select(!spaces) %>% 
  filter(!Postcode %in% c('Worthing, West Sussex', 'Worthing, West', 'West Sussex', 'Gardens, Worthing', 'Sussex, BN', "BN13 3Q25/05/2006", 'Road, Worthing,', 'West Sussex,', 'BN14 8', "Road, Wo17/11/2006", 'Worthing, Wes25/05/2006', 'BN11 2', 'West Sus')) 

worthing_postcodes <- Worthing_premises %>% 
  select(Postcode) %>% 
  unique()


for(i in 1:length(unique(worthing_postcodes$Postcode))){
  if(i == 1){lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double())
  }
  
  lookup_result_x <- postcode_lookup(unique(worthing_postcodes$Postcode)[i]) %>% 
    select(postcode, longitude, latitude)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result) 
  
}

lookup_result <- lookup_result %>% 
  bind_rows(data.frame(postcode = 'BN11 3AN', longitude = -0.370446, latitude = 50.81086)) %>% 
  bind_rows(data.frame(postcode = 'BN13 1RB', longitude =  -0.409505, latitude = 50.82776)) %>% 
  bind_rows(data.frame(postcode = 'BN11 3DY', longitude =  -0.36622, latitude = 50.81117)) %>% 
  unique()

Worthing_premises <- Worthing_premises %>% 
  left_join(lookup_result, by = c('Postcode' = 'postcode'))

Worthing_premises %>% 
  write.csv(., paste0(data_directory, '/Worthing_premises.csv'), row.names = FALSE)

# Working with compiled datasets ####
# 
#   Premises Licence
#   If you run commercial premises that are used for the sale or supply of alcohol, you must have a premises licence.
#   
#   A premises licence is also required if you provide hot food and drinks between 11pm and 5am, and/or if you provide the following forms of regulated entertainment, either for profit or for charity:
#  
#   Theatrical performance
#   film exhibition
#   indoor sporting event
#   boxing or wrestling (indoor or outdoor)
#   live music
#   recorded music
#   dance
  
#   Applications for a premises licence must include the applicants' details and those of the designated premises' supervisor, who must be a personal licence holder.
#   
#   The designated premises' supervisor is responsible for making sure the premises operate legally and meet any conditions attached to the licence. Applications must include a detailed plan of the premises and an operating schedule.
# 
# Private clubs are an exception to the requirement for a premises licence, as they may instead require a Club Premises Certificate.

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

# Plotting points

areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')
# 
# lad_boundaries_clipped_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFC_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>% 
#     filter(LAD22NM %in% c('Chichester'))
#    
# lad_boundaries_full_extent_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFE_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
#     filter(LAD22NM %in% areas & LAD22NM != 'Chichester')
# 
# lad_boundaries_sf <- rbind(lad_boundaries_clipped_sf, lad_boundaries_full_extent_sf)
# lad_boundaries_spdf <- as_Spatial(lad_boundaries_sf, IDs = lad_boundaries_sf$LAD22NM)
# 
# geojson_write(ms_simplify(geojson_json(lad_boundaries_spdf), keep = 0.3), file = paste0(output_directory, '/west_sussex_LTLAs.geojson'))
# 
ltla <- st_read(paste0(output_directory, '/west_sussex_LTLAs.geojson')) %>% 
  filter(LAD22NM %in% c('Adur', 'Arun', 'Worthing'))

# IMD


IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select(LSOA11CD = "LSOA code (2011)", LTLA =  "Local Authority District name (2019)", IMD_2019_Score = "Index of Multiple Deprivation (IMD) Score", IMD_National_Rank = "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", IMD_National_Decile =  "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)" ) %>% 
  mutate(IMD_National_Decile = factor(ifelse(IMD_National_Decile == 1, '10% most deprived',  ifelse(IMD_National_Decile == 2, 'Decile 2',  ifelse(IMD_National_Decile == 3, 'Decile 3',  ifelse(IMD_National_Decile == 4, 'Decile 4',  ifelse(IMD_National_Decile == 5, 'Decile 5',  ifelse(IMD_National_Decile == 6, 'Decile 6',  ifelse(IMD_National_Decile == 7, 'Decile 7',  ifelse(IMD_National_Decile == 8, 'Decile 8',  ifelse(IMD_National_Decile == 9, 'Decile 9',  ifelse(IMD_National_Decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  filter(LTLA %in% areas) %>% 
  arrange(desc(IMD_2019_Score)) %>% 
  mutate(Rank_in_West_Sussex = rank(desc(IMD_2019_Score))) %>% 
  mutate(Decile_in_West_Sussex = abs(ntile(IMD_2019_Score, 10) - 11)) %>% 
  mutate(Decile_in_West_Sussex = factor(ifelse(Decile_in_West_Sussex == 1, '10% most deprived',  ifelse(Decile_in_West_Sussex == 2, 'Decile 2',  ifelse(Decile_in_West_Sussex == 3, 'Decile 3',  ifelse(Decile_in_West_Sussex == 4, 'Decile 4',  ifelse(Decile_in_West_Sussex == 5, 'Decile 5',  ifelse(Decile_in_West_Sussex == 6, 'Decile 6',  ifelse(Decile_in_West_Sussex == 7, 'Decile 7',  ifelse(Decile_in_West_Sussex == 8, 'Decile 8',  ifelse(Decile_in_West_Sussex == 9, 'Decile 9',  ifelse(Decile_in_West_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) 


# Read in the lsoa geojson boundaries for our lsoas (actually this downloads all 30,000+ and then we filter)
# LSOA boundaries ####
lsoa_2011_sf <- st_read('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') 

# Convert it to a spatial polygon data frame
lsoa_2011_boundaries_spdf <-  as_Spatial(lsoa_2011_sf, IDs = lsoa_2011_sf$LSOA11CD) %>% 
  filter(LSOA11CD %in% IMD_2019$LSOA11CD) %>% 
  arrange(LSOA11CD) %>% 
  left_join(IMD_2019, by = 'LSOA11CD')

imd_colours <- c("#0000FF","#2080FF","#40E0FF","#70FFD0","#90FFB0","#C0E1B0","#E0FFA0","#E0FF70","#F0FF30","#FFFF00")

imd_palette <- colorFactor(imd_colours,
                              levels = levels(lsoa_2011_boundaries_spdf$IMD_National_Decile))

leaflet() %>% 
  addControl(paste0("<font size = '1px'><b>West Sussex Licenced Premises:</b><br>Data correct as at March 2023;</font>"),
             position = 'topright') %>%
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
           attribution = paste0('&copy; <a href=https://www.openstreetmap.org/copyright>OpenStreetMap</a> contributors &copy; <a href=https://carto.com/attributions>CARTO</a><br>Contains OS data ? Crown copyright and database right 2021<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an circle to find out more.')) %>%
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
                   group = 'Show premises') %>% 
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




