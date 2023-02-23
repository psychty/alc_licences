
# Alcohol licencing ####


# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'PostcodesioR', 'ggrepel', 'readODS', 'openxlsx')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

# base directory
base_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Alcohol/Alcohol HEA/Licences'

# base_directory <- '~/Repositories/child_immunisations/'
data_directory <- paste0(base_directory, '/data')
output_directory <- paste0(base_directory, '/outputs')

# To see what is in the directory already
list.files(data_directory)

download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1109599/alcohol-late-night-licensing-tables-2022.ods',
              paste0(data_directory, '/alcohol_latr_night_licensing_tables_2022.ods'),
              mode = 'wb')
              