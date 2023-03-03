
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

# These figures are what I'm aiming for.

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

# Worthing

Worthing_premises <- pdf_text('https://www.adur-worthing.gov.uk/media/Media,127816,smxx.pdf')

#There are five pages. each page is a row, V1 om the top row is the heading, everything else is a new licence
Worthing_table <- as.data.frame(str_split(Worthing_premises, "\n", simplify = TRUE))

Worthing_headings <- c('Reference','Licence Type Desc','Trader Name','Licence Holder','Premises Address','First Issue','Transfer Date','Alcohol','Ward')



# %>% 
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
  

# Working with compiled datasets ####
# 
#   Premises Licence
#   If you run commercial premises that are used for the sale or supply of alcohol, you must have a premises licence.
#   
#   A premises licence is also required if you provide hot food and drinks between 11pm and 5am, and/or if you provide the following forms of regulated entertainment, either for profit or for charity:
#     
#     theatrical performance
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