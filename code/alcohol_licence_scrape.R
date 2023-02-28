
# Alcohol licencing ####

# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'PostcodesioR', 'ggrepel', 'readODS', 'openxlsx', 'pdftools', 'httr', 'rvest', 'strex')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

# base directory
base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'
base_directory <- '~/Repositories/alc_licences/'

list.files(base_directory)

data_directory <- paste0(base_directory, '/data')
output_directory <- paste0(base_directory, '/outputs')

# To see what is in the directory already
list.files(data_directory)

if(file.exists(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods')) == FALSE){
download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1109599/alcohol-late-night-licensing-tables-2022.ods',
              paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
              mode = 'wb')
}

premises_24_hour_licence_df_raw <- read_ods(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
                             sheet = 'Table_4',
                             skip = 7) 

latest_total_24_hour_licenced_premises <- premises_24_hour_licence_df_raw %>%
  filter(`Licensing authority` %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham',  'Mid Sussex', 'Worthing')) %>% 
  filter(`Year \n(as at 31 March)` == '2021/22') 

names(premises_24_hour_licence_df_raw)


premises_licences <- read_ods(paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
         sheet = 'Table_2',
         skip = 7)

latest_premises_licences <- premises_licences %>%
  filter(`Licensing authority` %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham',  'Mid Sussex', 'Worthing')) %>% 
  filter(`Year \n(as at 31 March)` == '2021/22') 

# Adur ####

# We can try to scrape the premises.pdf 
calls_premises_webpage <- read_html('https://www.adur-worthing.gov.uk/licensing-and-permits/consultations-policy-forum/completed-applications/') %>%
  html_nodes("a") %>%
  html_attr("href")

# but these are not particularly helpfully named
pdf_strings <- unique(grep('pdf', calls_premises_webpage, value = T))

i = 2

txt_x <- pdf_text(paste0("https://www.adur-worthing.gov.uk", pdf_strings[i]))

table <- as.data.frame(str_split(txt_x, "\n", simplify = TRUE)) %>% 
  filter(V9 == '  Licensing Act 2003') %>% 
  select(Name = V6,
         Address = V11) %>% 
  mutate(spaces = str_count(Address, ' ')) %>% 
  mutate(Postcode = str_after_nth(Address, " ", spaces - 1)) %>% 
  select(!spaces) %>% 
  mutate(Reference = str_trim(str_after_nth(Name, "Reference", 1), side = 'both')) %>% 
  mutate(Name = str_trim(str_before_first(Name, 'Reference')))

for(i in 2:25){
  if(i == 2){Premises_table <- data.frame(Name = character(), Address = character(), Postcode = character(), Reference = character())
  }
  
  txt_x <- pdf_text(paste0("https://www.adur-worthing.gov.uk", pdf_strings[i]))
  
  table <- as.data.frame(str_split(txt_x, "\n", simplify = TRUE)) %>% 
    filter(V9 == '  Licensing Act 2003') %>% 
    select(Name = V6,
           Address = V11) %>% 
    mutate(spaces = str_count(Address, ' ')) %>% 
    mutate(Postcode = str_after_nth(Address, " ", spaces - 1)) %>% 
    select(!spaces) %>% 
    mutate(Reference = str_trim(str_after_nth(Name, "Reference", 1), side = 'both')) %>% 
    mutate(Name = str_trim(str_before_first(Name, 'Reference')))
  
  Premises_table <- table %>% 
    bind_rows(Premises_table) %>% 
    filter(Postcode != 'West Sussex') %>% 
    mutate(Postcode = ifelse(Postcode == 'BN43 6GT', 'BN43 6GQ', Postcode))
  
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
  write.csv(., paste0(output_directory, 'Adur_premises.csv'), row.names = FALSE)


# Worthing
# https://www.adur-worthing.gov.uk/media/Media,127816,smxx.pdf