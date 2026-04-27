library(tidyverse)
library(readxl)
# you need to install java to use MaxEnt

species <- c("variegatum", "dromedarii", "anatolicum")

cumming = read.table("data/Main tick database (final).txt",
                     header = TRUE,
                     sep = "\t",
                     quote = "\"",
                     stringsAsFactors = FALSE,
                     fill = TRUE)

variegatum_cumming = cumming %>% 
  filter(str_detect(Species, regex("Amblyomma variegatum", ignore_case = TRUE)))

dromedarii_cumming = cumming %>% 
  filter(str_detect(Species, regex("dromedarii", ignore_case = TRUE)))

anatolicum_cumming = cumming %>% 
  filter(str_detect(Species, regex("anatolicum", ignore_case = TRUE))) %>% 
  data.frame()

cumingTicks = rbind(variegatum_cumming, dromedarii_cumming, anatolicum_cumming) %>% 
  as.data.frame() %>% 
  dplyr::select(Species, Country, Latitude.Y, Longitude.X, year) %>% 
  mutate(year = as.numeric(str_extract(year, "\\d{4}$"))) %>% 
  rename(longitude = "Longitude.X",
         latitude = "Latitude.Y") %>% 
  dplyr::select(species = Species, country =Country, year, latitude, longitude)

southAmericaTicks = read_excel("data/s.america ticks.xlsx")
neotropic_ticks = read_excel("data/neotropic ticks.xlsx")

# South America does not have these focal ticks. so I do not bother to work with these data any further.
neotropic_ticksClean = neotropic_ticks %>% 
  filter(str_detect(SPECIES,
                    regex(paste(species, collapse = "|"),
                          ignore_case = TRUE))) 

southAmericaTicks %>% 
  filter(str_detect(SPECIES,
                    regex(paste(species, collapse = "|"),
                          ignore_case = TRUE))) 


chinaTicks = read_excel("data/chinaTicks.xlsx")

chinaTicks = chinaTicks %>% 
  filter(str_detect(tick_sp,
                    regex(paste(species, collapse = "|"),
                          ignore_case = TRUE))) %>% 
  rename(species = "tick_sp", longitude = 'lon', latitude = 'lat', 
         country = "lc_l1", year = 'smp_end') %>% 
  dplyr::select(species, country, year, latitude, longitude)

##############################################################################

westAfrica<- read_excel("data/westAfricaTicks.xlsx", sheet ="MasterList_WEST_FINAL")

westAfricaTicks = westAfrica %>% 
  filter(str_detect(ScientificName,
                    regex(paste(species, collapse = "|"),
                          ignore_case = TRUE))) %>% 
  dplyr::select(ScientificName, DecimalLongitude, DecimalLatitude, Country, VerbatimCollectingDate) %>% 
  mutate(
    year = map_dbl(str_extract_all(VerbatimCollectingDate, "\\d{4}"),
                   ~ if(length(.x) == 0) NA else max(as.numeric(.x)))) %>% # extract the max date in vector of extracted dates.
  rename(
    species = ScientificName,
    longitude = DecimalLongitude,
    latitude = DecimalLatitude,
    country = Country
  ) %>% dplyr::select(species, country, year, latitude, longitude) %>% as.data.frame() %>% filter(year < 2024)

######################################################################

# Path to file
east_path <- "data/Tick_data_East_Africa.xlsx"
# Get all sheet names
east_sheet_names <- excel_sheets(east_path)
# Read all sheets into a named list
east_sheets <- lapply(east_sheet_names, function(sheet) {
  read_excel(east_path, sheet = sheet)
})
# Name the list elements by sheet names
names(east_sheets) <- east_sheet_names

eastAfricaAvihub = east_sheets %>% 
  map(~ dplyr::select(.x, Country, `Year of Pub`, Longitude, Latitude, Tick_species,`Review Score` )) %>% 
  bind_rows(.id = "sheet_name") %>%
  mutate(
    longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
    latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
  ) %>%
  mutate(
    longitude_clean = ifelse(str_detect(Longitude, "W"), -1 * longitude_clean, longitude_clean),
    latitude_clean = ifelse(str_detect(Latitude, "S"), -1 * latitude_clean, latitude_clean)
  )
names(eastAfricaAvihub)[c(3,7)] <- c("year", "reviewScore")

# Path to your Excel file
west_path = "data/Tick_data_West_Africa.xlsx"
# Get all sheet names
west_sheet_names <- excel_sheets(west_path)
# Read all sheets into a named list
west_sheets <- lapply(west_sheet_names, function(sheet) {
  read_excel(west_path, sheet = sheet)
})
# Name the list elements by sheet names
names(west_sheets) <- west_sheet_names

westAfricaAvihub = west_sheets %>% 
  map(~ dplyr::select(.x,  Country, `Year of Pub`, Longitude, Latitude, Tick_species,`Review Score`)) %>% 
  bind_rows(.id = "sheet_name") %>%
  mutate(
    longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
    latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
  ) %>%
  mutate(
    longitude_clean = ifelse(str_detect(Longitude, "W"), -1 * longitude_clean, longitude_clean),
    latitude_clean = ifelse(str_detect(Latitude, "S"), -1 * latitude_clean, latitude_clean)
  )
names(westAfricaAvihub)[c(3,7)] <- c("year", "reviewScore")



south_path = "data/Tick_data_Southern_Africa.xlsx"

# Get all sheet names
south_sheet_names <- excel_sheets(south_path)
# Read all sheets into a named list
south_sheets <- lapply(south_sheet_names, function(sheet) {
  read_excel(south_path, sheet = sheet)
})
# Name the list elements by sheet names
names(south_sheets) <- south_sheet_names


southAfricaAvihub = bind_rows(
  south_sheets$Winsoul %>% mutate(`Year of Pub` = as.numeric(`Year of Pub`)),
  south_sheets$Debayo %>% mutate(`Year of Pub` = as.numeric(`Year of Pub`))
) %>% dplyr::select( Country, `Year of Pub`, Longitude, Latitude, Tick_species,`Review Score`)  %>%
  mutate(
    longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
    latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
  ) %>%
  mutate(
    longitude_clean = ifelse(str_detect(Longitude, "W"), -1 * longitude_clean, longitude_clean),
    latitude_clean = ifelse(str_detect(Latitude, "S"), -1 * latitude_clean, latitude_clean)
  ) 

names(southAfricaAvihub)[c(2,6)] = c("year", "reviewScore")


Tick_data_North_Africa <- read_excel("data/Tick_data_North_Africa.xlsx")


north_path <- "data/Tick_data_North_Africa.xlsx"

# Get all sheet names
north_sheet_names <- excel_sheets(north_path)
# Read all sheets into a named list
north_sheets <- lapply(north_sheet_names, function(sheet) {
  read_excel(north_path, sheet = sheet)
})
# Name the list elements by sheet names
names(north_sheets) <- north_sheet_names



northAfricaAvihub = north_sheets %>% 
  map(~ dplyr::select(.x,  Country, `Year of Pub`, Longitude, Latitude, Tick_species,`Review Score`)) %>% 
  bind_rows(.id = "sheet_name") %>% data.frame() %>% 
  mutate(
    longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
    latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
  ) %>%
  mutate(
    longitude_clean = ifelse(str_detect(Longitude, "W"), -1 * longitude_clean, longitude_clean),
    latitude_clean = ifelse(str_detect(Latitude, "S"), -1 * latitude_clean, latitude_clean)
  ) 

names(northAfricaAvihub)[c(3,7)] <- c("year", "reviewScore")

avihub = rbind(dplyr::select(westAfricaAvihub, -sheet_name), 
               dplyr::select(eastAfricaAvihub, -sheet_name),
               southAfricaAvihub, 
               dplyr::select(northAfricaAvihub, -sheet_name))

score = c(NA, 2,3)


africaAvihub = avihub %>% dplyr::select(- c("Longitude", "Latitude")) %>% 
  rename(longitude = "longitude_clean",
         latitude = 'latitude_clean') %>% 
  filter(reviewScore %in% score,
         !is.na(latitude),
         !is.na(longitude)) %>% 
  filter(str_detect(Tick_species,
                    regex(paste(species, collapse = "|"),
                          ignore_case = TRUE))) %>% 
  dplyr::select(country = "Country", year, species = "Tick_species", longitude, latitude)

# Bring all data together:


tickdatafromLiteratures = bind_rows(
  africaAvihub %>% mutate(source = "avihub"),
  chinaTicks  %>% mutate(source = "chinaPaper"),
  cumingTicks %>% mutate(source = "cumming"),
  westAfricaTicks %>% mutate(source = "westAfricaReviewPaper")
) %>% 
  mutate(
    species = case_when(
      str_detect(species, regex("dromedarii", ignore_case = TRUE)) ~ "Hyalomma dromedarii",
      str_detect(species, regex("anatolicum", ignore_case = TRUE)) ~ "Hyalomma anatolicum",
      str_detect(species, regex("variegatum", ignore_case = TRUE)) ~ "Amblyomma variegatum"
    )) %>% rename(lat = "latitude", lon = "longitude")

tickdatafromLiteratures %>% 
  filter(!complete.cases(.))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Gbif----

library("raster")
library("dismo")

# download occurrences
variegatumGbif = gbif(genus="Amblyomma",species="variegatum",download=TRUE,end=10000)

variegatumclean = subset(variegatumGbif, (!is.na(lat))&
                           (!is.na(lon))&
                           (lat!=0)&
                           (lon!=0) 
) 

variegatumclean_unique = variegatumclean[!duplicated( variegatumclean[c("lat","lon")]  ),] 

variegatumclean2 =  subset(variegatumclean_unique, basisOfRecord=="PRESERVED_SPECIMEN" |
                             basisOfRecord=="HUMAN_OBSERVATION"| basisOfRecord=="MATERIAL_SAMPLE"
                           | basisOfRecord=="OCCURRENCE")

variegatum_final = subset(variegatumclean2, year>=1900 & year <=2025)


# Hyalomma anatolicum
anatolicumGbif = gbif(genus="Hyalomma",species="anatolicum",download=TRUE,end=10000)

anatolicumclean = subset(anatolicumGbif, (!is.na(lat)) &
                           (!is.na(lon)) &
                           (lat != 0) &
                           (lon != 0))

anatolicumclean_unique = anatolicumclean[!duplicated(anatolicumclean[c("lat","lon")]), ]

anatolicumclean2 = subset(anatolicumclean_unique,
                          basisOfRecord == "PRESERVED_SPECIMEN" |
                            basisOfRecord == "HUMAN_OBSERVATION" |
                            basisOfRecord == "MATERIAL_SAMPLE" |
                            basisOfRecord == "OCCURRENCE")

anatolicum_final = subset(anatolicumclean2, year >= 1900 & year <= 2025)


# H dromedarii
dromedariiGbif = gbif(genus="Hyalomma",species="dromedarii",download=TRUE,end=10000)

dromedariiclean = subset(dromedariiGbif, (!is.na(lat)) &
                           (!is.na(lon)) &
                           (lat != 0) &
                           (lon != 0))

dromedariiclean_unique = dromedariiclean[!duplicated(dromedariiclean[c("lat","lon")]), ]

dromedariiclean2 = subset(dromedariiclean_unique,
                          basisOfRecord == "PRESERVED_SPECIMEN" |
                            basisOfRecord == "HUMAN_OBSERVATION" |
                            basisOfRecord == "MATERIAL_SAMPLE" |
                            basisOfRecord == "OCCURRENCE")

dromedarii_final = subset(dromedariiclean2, year >= 1900 & year <= 2025)

gbifTick = rbind(variegatum_final %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Amblyomma variegatum"), 
                 anatolicum_final %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Hyalomma anatolicum"), 
                 dromedarii_final %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Hyalomma dromedarii")) %>%
  filter(!is.na(lat), !is.na(lon))

# make a gbif data to combine with literature data

gbif2 = rbind(anatolicumclean2   %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Hyalomma anatolicum"), 
              dromedariiclean2  %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Hyalomma dromedarii"), 
              variegatumclean2   %>% dplyr::select(country, lat, lon, year) %>% mutate(species = "Amblyomma variegatum")) %>% 
  filter(year >= 1900 & year <= 2025) %>% 
  mutate(source = "gbif")

# joining all data together
literature_gbifTicks = rbind(tickdatafromLiteratures, gbif2)




  