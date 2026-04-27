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


# Here I was not very good with naming of objects. The most important one you should take not of is 
# 2025 and 1995. 
# I used 2025 tick data but project it to historical 1970-2000 data for the maxent, yet still call the object 2025
# the idea is that data from 2000 - 2025 ticks are additional data to what we have before and they are not so far away in climate from 1970-2000.





devtools::source_url("https://raw.githubusercontent.com/shandongfx/nimbios_enm/master/Appendix2_prepPara.R")

library(ENMeval)
library(dismo)
library(ENMeval)
library(terra)

clim2061_2080 <- stack("data/climate/wc2.1_10m_bioc_ACCESS-CM2_ssp245_2061-2080.tif")
clim2021_2040 <- stack("data/climate/wc2.1_10m_bioc_ACCESS-CM2_ssp245_2021-2040.tif")
clim2081_2100 <- stack("data/climate/wc2.1_10m_bioc_ACCESS-CM2_ssp245_2081-2100.tif")


myCRS1 = CRS("+init=epsg:4326") # WGS 84
# look at the data
plot(clim2061_2080[[1]])

plot(clim2021_2040[[1]])

clim_list = list.files("data/climate/",pattern=".tif$",full.names = T)
clim = raster::stack(clim_list) 
names(clim) = gsub("wc2.1_10m_","",names(clim))

# look at the data
plot(clim[[1]])


dromedarii_final1995 = literature_gbifTicks %>% 
  filter(species == "Hyalomma dromedarii") %>% 
  filter(year <=1995)

variegatum_final1995 = literature_gbifTicks %>% 
  filter(species == "Amblyomma variegatum")  %>% 
  filter(year <=1995)

anatolicum_final1995 = literature_gbifTicks %>% 
  filter(species == "Hyalomma anatolicum")  %>% 
  filter(year <=1995)


# make occurrences spatial
coordinates(variegatum_final1995) <- ~ lon + lat
crs(variegatum_final1995) <- myCRS1

coordinates(anatolicum_final1995) <- ~ lon + lat
crs(anatolicum_final1995) <- myCRS1

coordinates(dromedarii_final1995) <- ~ lon + lat
crs(dromedarii_final1995) <- myCRS1


# further clean data: remove occurrence points outside climate data
clean_occ <- function(occ, clim_layer){
  cond <- extract(clim_layer, occ)
  occ <- subset(occ, !is.na(cond))
  return(occ)
}

anatolicum_final1995  <- clean_occ(anatolicum_final1995, clim[[1]])
variegatum_final1995  <- clean_occ(variegatum_final1995, clim[[1]])
dromedarii_final1995  <- clean_occ(dromedarii_final1995, clim[[1]])


anatolicum_occ_buffer1995 = buffer(anatolicum_final1995,width=4*10^5)
variegatum_occ_buffer1995 = buffer(variegatum_final1995,width=4*10^5)
dromedarii_occ_buffer1995 = buffer(dromedarii_final1995,width=4*10^5)



# rarify the occurrences
rarify_occ <- function(occ, clim_layer) {
  occ_cell_id <- cellFromXY(clim_layer, occ)
  print(tail(sort(table(occ_cell_id))))
  filter_basedOnID <- !duplicated(occ_cell_id)
  occ <- occ[filter_basedOnID, ]
  print(nrow(occ))
  return(occ)
}

anatolicum_final1995  <- rarify_occ(anatolicum_final1995, clim[[1]])
variegatum_final1995  <- rarify_occ(variegatum_final1995, clim[[1]])
dromedarii_final1995  <- rarify_occ(dromedarii_final1995, clim[[1]])

# generate background data
generate_bg <- function(occ, occ_buffer, clim, n_bg = 10000, seed = 999, plot_result = TRUE) {
  
  # restrict climate layers to the buffer region
  clim_mask <- mask(clim, occ_buffer)
  set.seed(seed)
  
  # sample background points
  bg <- sampleRandom(
    x = clim_mask,
    size = n_bg,
    na.rm = TRUE,
    sp = TRUE
  )
  # visualization
  if(plot_result){
    plot(bg, col = "grey")
    plot(occ, add = TRUE, col = "black")
  }
  
  return(bg)
}


bg_anatolicum1995 <- generate_bg(
  occ = anatolicum_final1995,
  occ_buffer = anatolicum_occ_buffer1995,
  clim = clim
)

bg_variegatum1995 <- generate_bg(
  occ = variegatum_final1995,
  occ_buffer = variegatum_occ_buffer1995,
  clim = clim
)

bg_dromedarii1995 <- generate_bg(
  occ = dromedarii_final1995,
  occ_buffer = dromedarii_occ_buffer1995,
  clim = clim
)


anatolicum_env_bg1995 = extract(clim,bg_anatolicum1995) %>% as.data.frame()
anatolicum_env_occ1995 = extract(clim, anatolicum_final1995) %>% as.data.frame()

variegatum_env_bg1995 = extract(clim,bg_variegatum1995) %>% as.data.frame()
variegatum_env_occ1995 = extract(clim,variegatum_final1995) %>% as.data.frame()

dromedarii_env_bg1995 = extract(clim,bg_dromedarii1995) %>% as.data.frame()
dromedarii_env_occ1995 = extract(clim,dromedarii_final1995) %>% as.data.frame()





dromedarii_final2025 = literature_gbifTicks %>% 
  filter(species == "Hyalomma dromedarii")

variegatum_final2025 = literature_gbifTicks %>% 
  filter(species == "Amblyomma variegatum")  

anatolicum_final2025 = literature_gbifTicks %>% 
  filter(species == "Hyalomma anatolicum")  


# make occurrences spatial
coordinates(variegatum_final2025) <- ~ lon + lat
crs(variegatum_final2025) <- myCRS1

coordinates(anatolicum_final2025) <- ~ lon + lat
crs(anatolicum_final2025) <- myCRS1

coordinates(dromedarii_final2025) <- ~ lon + lat
crs(dromedarii_final2025) <- myCRS1




anatolicum_final2025  <- clean_occ(anatolicum_final2025, clim[[1]])
variegatum_final2025  <- clean_occ(variegatum_final2025, clim[[1]])
dromedarii_final2025  <- clean_occ(dromedarii_final2025, clim[[1]])

plot(clim[[1]])
plot(anatolicum_final2025, add = TRUE, col = "blue")
plot(variegatum_final2025, add = TRUE, col = "red")
plot(dromedarii_final2025, add = TRUE, col = "black")

anatolicum_occ_buffer2025 = buffer(anatolicum_final2025, width = 4*10^5)
variegatum_occ_buffer2025 = buffer(variegatum_final2025, width = 4*10^5)
dromedarii_occ_buffer2025 = buffer(dromedarii_final2025, width = 4*10^5)

anatolicum_final2025  <- rarify_occ(anatolicum_final2025, clim[[1]])
variegatum_final2025  <- rarify_occ(variegatum_final2025, clim[[1]])
dromedarii_final2025  <- rarify_occ(dromedarii_final2025, clim[[1]])




bg_anatolicum2025 <- generate_bg(
  occ = anatolicum_final2025,
  occ_buffer = anatolicum_occ_buffer2025,
  clim = clim
)

bg_variegatum2025 <- generate_bg(
  occ = variegatum_final2025,
  occ_buffer = variegatum_occ_buffer2025,
  clim = clim
)

bg_dromedarii2025 <- generate_bg(
  occ = dromedarii_final2025,
  occ_buffer = dromedarii_occ_buffer2025,
  clim = clim
)

anatolicum_env_bg2025 = extract(clim, bg_anatolicum2025) %>% as.data.frame()
anatolicum_env_occ2025 = extract(clim, anatolicum_final2025) %>% as.data.frame()

variegatum_env_bg2025 = extract(clim, bg_variegatum2025) %>% as.data.frame()
variegatum_env_occ2025 = extract(clim, variegatum_final2025) %>% as.data.frame()

dromedarii_env_bg2025 = extract(clim, bg_dromedarii2025) %>% as.data.frame()
dromedarii_env_occ2025 = extract(clim, dromedarii_final2025) %>% as.data.frame()





run_sdm_pipeline <- function(
    occ_points,
    bg_points,
    climate_stack,
    occ_env,
    bg_env,
    species_name,
    vars = c("bio_1","bio_4","bio_5","bio_12","bio_15"), # bioclimatic variable column index
    RMvalues = seq(0.5,4,0.5),
    fc = c("L","LQ","H","LQH")
){
  
  if(!requireNamespace("ENMeval")) stop("ENMeval required")
  if(!requireNamespace("raster")) stop("raster required")
  if(!requireNamespace("dismo")) stop("dismo required")
  
  ################################################
  # 1. ENVIRONMENT
  ################################################
  
  env <- climate_stack[[vars]]
  
  occ_coord <- raster::coordinates(occ_points)
  bg_coord  <- raster::coordinates(bg_points)
  
  message("Running ENMevaluate for ", species_name)
  
  ################################################
  # 2. MODEL TUNING (SPATIAL CROSS-VALIDATION)
  ################################################
  
  evaluation <- ENMeval::ENMevaluate(
    occ = occ_coord,
    env = env,
    bg.coords = bg_coord,
    method = "block",
    RMvalues = RMvalues,
    fc = fc,
    algorithm = "maxent.jar"
  )
  
  ################################################
  # 3. SELECT BEST MODEL
  ################################################
  
  best_index <- which(evaluation@results$delta.AICc == 0)[1]
  best_model <- evaluation@models[[best_index]]
  
  message("Best model selected")
  
  ################################################
  # 4. CONTINUOUS PREDICTION
  ################################################
  
  prediction <- raster::predict(
    object = best_model,
    x = env
  )
  
  ################################################
  # 5. THRESHOLD SELECTION
  ################################################
  
  eval_obj <- dismo::evaluate(
    p = occ_env[, vars],
    a = bg_env[, vars],
    model = best_model
  )
  
  threshold_value <- dismo::threshold(
    eval_obj,
    stat = "spec_sens"
  )
  
  ################################################
  # 6. BINARY MAP (PRESENCE–ABSENCE)
  ################################################
  
  binary_map <- prediction >= threshold_value
  binary_map <- binary_map * 1
  
  # mask invalid climate cells
  binary_map <- raster::mask(binary_map, env[[1]])
  
  ################################################
  # 7. RANGE SIZE CALCULATIONS
  ################################################
  
  pixels_present <- raster::cellStats(binary_map, sum)
  
  total_pixels <- raster::cellStats(
    !is.na(binary_map),
    sum
  )
  
  pixels_absent <- total_pixels - pixels_present
  
  cell_area <- raster::area(binary_map)
  
  suitable_area_km2 <- raster::cellStats(
    cell_area * binary_map,
    sum
  )
  
  summary_table <- data.frame(
    Species = species_name,
    Threshold = threshold_value,
    Suitable_pixels = pixels_present,
    Unsuitable_pixels = pixels_absent,
    Suitable_area_km2 = suitable_area_km2
  )
  
  ################################################
  # RETURN EVERYTHING
  ################################################
  
  return(list(
    evaluation = evaluation,
    best_model = best_model,
    prediction = prediction,
    binary_map = binary_map,
    threshold = threshold_value,
    summary = summary_table
  ))
}






project_sdm_future <- function(
    trained_model,
    threshold,
    future_climate,
    species_name,
    time_period
){
  
  library(raster)
  
  message("Projecting ", species_name,
          " to ", time_period)
  
  ################################################
  # STANDARDIZE NAMES
  ################################################
  
  names(future_climate) <- paste0(
    "bio_", 1:raster::nlayers(future_climate)
  )
  
  ################################################
  # GET MODEL VARIABLES
  ################################################
  
  model_vars <- colnames(trained_model@presence)
  
  ################################################
  # SUBSET FUTURE CLIMATE
  ################################################
  
  future_env <- future_climate[[model_vars]]
  
  ################################################
  # PREDICT
  ################################################
  
  pred_future <- raster::predict(
    trained_model,
    future_env
  )
  
  ################################################
  # THRESHOLD
  ################################################
  
  binary_future <- pred_future >= threshold
  binary_future <- binary_future * 1
  
  binary_future <- raster::mask(
    binary_future,
    future_env[[1]]
  )
  
  ################################################
  # RANGE SIZE
  ################################################
  
  cell_area <- raster::area(binary_future)
  
  suitable_area_km2 <- raster::cellStats(
    cell_area * binary_future,
    sum
  )
  
  summary_table <- data.frame(
    Species = species_name,
    Time = time_period,
    Suitable_area_km2 = suitable_area_km2
  )
  
  return(list(
    prediction = pred_future,
    binary_map = binary_future,
    summary = summary_table
  ))
}


# variegatum_result2025 <- run_sdm_pipeline(
#   occ_points = occ_pts,
#   bg_points  = bg_pts,
#   climate_stack = clim_current,
#   occ_env = occ_env,
#   bg_env  = bg_env,
#   species_name = "Amblyomma variegatum"
# )
# 


variegatum_result2025 <- run_sdm_pipeline(
  occ_points = variegatum_final2025,
  bg_points = bg_variegatum2025,
  climate_stack = clim,
  occ_env = variegatum_env_occ2025,
  bg_env = variegatum_env_bg2025,
  species_name = "Amblyomma variegatum")

variegatum_result2025$summary


plot(
  variegatum_result2025$binary_map,
  main="Amblyomma variegatum (1995)",
  col=c("grey85","darkgreen"),
  legend=FALSE)

legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkgreen"))




variegatum_2040 <- project_sdm_future(
  trained_model = variegatum_result2025$best_model,
  threshold = variegatum_result2025$threshold,
  future_climate = clim2021_2040,
  species_name = "Amblyomma variegatum",
  time_period = "2040"
)

plot(variegatum_2040$binary_map,
     col=c("grey85","darkgreen"),
     main="Amblyomma variegatum (2040 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkgreen"))


variegatum_2080 <- project_sdm_future(
  trained_model = variegatum_result2025$best_model,
  threshold = variegatum_result2025$threshold,
  future_climate = clim2061_2080,
  species_name = "Amblyomma variegatum",
  time_period = "2080")

plot(variegatum_2080$binary_map,
     col=c("grey85","darkgreen"),
     main="Amblyomma variegatum (2080 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkgreen"))

variegatum_2100 <- project_sdm_future(
  trained_model = variegatum_result2025$best_model,
  threshold = variegatum_result2025$threshold,
  future_climate = clim2081_2100,
  species_name = "Amblyomma variegatum",
  time_period = "2100")

plot(variegatum_2100$binary_map,
     col=c("grey85","darkgreen"),
     main="Amblyomma variegatum (2100 suitability)")

legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkgreen"))

# Even if CRS + resolution match, grids can still be offset.

compareRaster(
  clim,
  clim2081_2100,
  extent=TRUE,
  rowcol=TRUE,
  crs=TRUE,
  res=TRUE,
  stopiffalse=FALSE
)


library(rnaturalearth)
library(sf)
library(raster)

# Get all African countries
africa_sf <- rnaturalearth::ne_countries(
  continent = "Africa",
  scale = "medium",
  returnclass = "sf"
)

# Convert to Spatial object (needed for raster package)
africa_sp <- as(africa_sf, "Spatial")

africa_range_summary <- function(
    prediction_raster,
    threshold,
    africa_polygon,
    species_name,
    year_label = NULL
){
  
  if(!requireNamespace("raster"))
    stop("Package 'raster' required")
  
  pred_africa <- raster::crop(
    prediction_raster,
    africa_polygon
  )
  
  pred_africa <- raster::mask(
    pred_africa,
    africa_polygon
  )
  binary_map <- pred_africa >= threshold
  binary_map <- binary_map * 1   # TRUE/FALSE → 1/0
  
  #  4. Pixel Counts
  pixels_present <- raster::cellStats(binary_map, sum)
  
  total_pixels <- raster::cellStats(
    !is.na(binary_map),
    sum
  )
  
  pixels_absent <- total_pixels - pixels_present
  
  #  Area Calculation (km²)
  cell_area <- raster::area(binary_map)
  
  suitable_area_km2 <- raster::cellStats(
    cell_area * binary_map,
    sum
  )
  
  summary_table <- data.frame(
    Species = species_name,
    Year = year_label,
    Threshold = threshold,
    Suitable_pixels = pixels_present,
    Unsuitable_pixels = pixels_absent,
    Suitable_area_km2 = suitable_area_km2
  )
  
  return(list(
    binary_map = binary_map,
    africa_prediction = pred_africa,
    summary = summary_table
  ))
}



variegatum_africa2100 <- africa_range_summary(
  prediction_raster = variegatum_2100$prediction,
  threshold = variegatum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "Amblyomma variegatum",
  year_label = "2100")


plot(variegatum_africa2100$binary_map,
     col = c("grey85","darkgreen"),
     legend = FALSE,
     main = "Amblyomma variegatum (Africa, 2100)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkgreen"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


variegatum_africa2080 <- africa_range_summary(
  prediction_raster = variegatum_2080$prediction,
  threshold = variegatum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "Amblyomma variegatum",
  year_label = "2080")


plot(variegatum_africa2080$binary_map,
     col = c("grey85","darkgreen"),
     legend = FALSE,
     main = "Amblyomma variegatum (Africa, 2080)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkgreen"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

variegatum_africa2040 <- africa_range_summary(
  prediction_raster = variegatum_2040$prediction,
  threshold = variegatum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "Amblyomma variegatum",
  year_label = "2040")


plot(variegatum_africa2040$binary_map,
     col = c("grey85","darkgreen"),
     legend = FALSE,
     main = "Amblyomma variegatum (Africa, 2040)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkgreen"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


variegatum_africa1995 <- africa_range_summary(
  prediction_raster = variegatum_result2025$prediction,
  threshold = variegatum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "Amblyomma variegatum",
  year_label = "1995")


plot(variegatum_africa1995$binary_map,
     col = c("grey85","darkgreen"),
     legend = FALSE,
     main = "Amblyomma variegatum (Africa, 1995)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkgreen"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

################### anatolicum -----------------------------------------------






anatolicum_result2025 <- run_sdm_pipeline(
  occ_points = anatolicum_final2025,
  bg_points = bg_anatolicum2025,
  climate_stack = clim,
  occ_env = anatolicum_env_occ2025,
  bg_env = anatolicum_env_bg2025,
  species_name = "H. anatolicum")


plot(
  anatolicum_result2025$binary_map,
  main="H. anatolicum (1995)",
  col=c("grey85","darkorange"),
  legend=FALSE)

legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkorange"))



anatolicum_2040 <- project_sdm_future(
  trained_model = anatolicum_result2025$best_model,
  threshold = anatolicum_result2025$threshold,
  future_climate = clim2021_2040,
  species_name = "H. anatolicum",
  time_period = "2040"
)

plot(anatolicum_2040$binary_map,
     col=c("grey85","darkorange"),
     main="H. anatolicum (2040 suitability)")

legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkorange"))


anatolicum_2080 <- project_sdm_future(
  trained_model = anatolicum_result2025$best_model,
  threshold = anatolicum_result2025$threshold,
  future_climate = clim2061_2080,
  species_name = "H. anatolicum",
  time_period = "2080")

plot(anatolicum_2080$binary_map,
     col=c("grey85","darkorange"),
     main="H. anatolicum (2080 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkorange"))

anatolicum_2100 <- project_sdm_future(
  trained_model = anatolicum_result2025$best_model,
  threshold = anatolicum_result2025$threshold,
  future_climate = clim2081_2100,
  species_name = "H. anatolicum",
  time_period = "2100")

plot(anatolicum_2100$binary_map,
     col=c("grey85","darkorange"),
     main="H. anatolicum (2100 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","darkorange"))

# anatolicum_2040$summary
# anatolicum_2080$summary
# anatolicum_2100$summary

anatolicum_africa2100 <- africa_range_summary(
  prediction_raster = anatolicum_2100$prediction,
  threshold = anatolicum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. anatolicum",
  year_label = "2100")


plot(anatolicum_africa2100$binary_map,
     col = c("grey85","darkorange"),
     legend = FALSE,
     main = "H. anatolicum (Africa, 2100)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkorange"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


anatolicum_africa2080 <- africa_range_summary(
  prediction_raster = anatolicum_2080$prediction,
  threshold = anatolicum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. anatolicum",
  year_label = "2080")


plot(anatolicum_africa2080$binary_map,
     col = c("grey85","darkorange"),
     legend = FALSE,
     main = "H. anatolicum (Africa, 2080)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkorange"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

anatolicum_africa2040 <- africa_range_summary(
  prediction_raster = anatolicum_2040$prediction,
  threshold = anatolicum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. anatolicum",
  year_label = "2040")


plot(anatolicum_africa2040$binary_map,
     col = c("grey85","darkorange"),
     legend = FALSE,
     main = "H. anatolicum (Africa, 2040)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkorange"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


anatolicum_africa1995 <- africa_range_summary(
  prediction_raster = anatolicum_result2025$prediction,
  threshold = anatolicum_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. anatolicum",
  year_label = "1995")


plot(anatolicum_africa1995$binary_map,
     col = c("grey85","darkorange"),
     legend = FALSE,
     main = "H. anatolicum (Africa, 1995)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","darkorange"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

anatolicum_africa1995$summary
anatolicum_africa2040$summary
anatolicum_africa2080$summary
anatolicum_africa2100$summary

############### dromederii -------------------------------




dromedarii_result2025 <- run_sdm_pipeline(
  occ_points = dromedarii_final2025,
  bg_points = bg_dromedarii2025,
  climate_stack = clim,
  occ_env = dromedarii_env_occ2025,
  bg_env = dromedarii_env_bg2025,
  species_name = "H. dromedarii")


plot(
  dromedarii_result2025$binary_map,
  main="H. dromedarii (1995)",
  col=c("grey85","red"),
  legend=FALSE)

legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","red"))



dromedarii_2040 <- project_sdm_future(
  trained_model = dromedarii_result2025$best_model,
  threshold = dromedarii_result2025$threshold,
  future_climate = clim2021_2040,
  species_name = "H. dromedarii",
  time_period = "2040"
)

plot(dromedarii_2040$binary_map,
     col=c("grey85","red"),
     main="H. dromedarii (2040 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","red"))


dromedarii_2080 <- project_sdm_future(
  trained_model = dromedarii_result2025$best_model,
  threshold = dromedarii_result2025$threshold,
  future_climate = clim2061_2080,
  species_name = "H. dromedarii",
  time_period = "2080")

plot(dromedarii_2080$binary_map,
     col=c("grey85","red"),
     main="H. dromedarii (2080 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","red"))


dromedarii_2100 <- project_sdm_future(
  trained_model = dromedarii_result2025$best_model,
  threshold = dromedarii_result2025$threshold,
  future_climate = clim2081_2100,
  species_name = "H. dromedarii",
  time_period = "2100")

plot(dromedarii_2100$binary_map,
     col=c("grey85","red"),
     main="H. dromedarii (2100 suitability)")
legend(
  "bottomleft",
  legend=c("Unsuitable","Suitable"),
  fill=c("grey85","red"))




dromedarii_africa2100 <- africa_range_summary(
  prediction_raster = dromedarii_2100$prediction,
  threshold = dromedarii_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. dromedarii",
  year_label = "2100")


plot(dromedarii_africa2100$binary_map,
     col = c("grey85","red"),
     legend = FALSE,
     main = "H. dromedarii (Africa, 2100)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","red"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


dromedarii_africa2080 <- africa_range_summary(
  prediction_raster = dromedarii_2080$prediction,
  threshold = dromedarii_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. dromedarii",
  year_label = "2080")


plot(dromedarii_africa2080$binary_map,
     col = c("grey85","red"),
     legend = FALSE,
     main = "H. dromedarii (Africa, 2080)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","red"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

dromedarii_africa2040 <- africa_range_summary(
  prediction_raster = dromedarii_2040$prediction,
  threshold = dromedarii_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. dromedarii",
  year_label = "2040")


plot(dromedarii_africa2040$binary_map,
     col = c("grey85","red"),
     legend = FALSE,
     main = "H. dromedarii (Africa, 2040)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","red"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)


dromedarii_africa1995 <- africa_range_summary(
  prediction_raster = dromedarii_result2025$prediction,
  threshold = dromedarii_result2025$threshold,
  africa_polygon = africa_sp,
  species_name = "H. dromedarii",
  year_label = "1995")


plot(dromedarii_africa1995$binary_map,
     col = c("grey85","red"),
     legend = FALSE,
     main = "H. dromedarii (Africa, 1995)")

legend("bottomleft",
       legend=c("Unsuitable","Suitable"),
       fill=c("grey85","red"))
plot(
  sf::st_geometry(africa_sf),
  add = TRUE,
  border = "black",
  lwd = 0.5
)




dromedarii_africa1995$summary
dromedarii_africa2040$summary
dromedarii_africa2080$summary
dromedarii_africa2100$summary

anatolicum_africa1995$summary
anatolicum_africa2040$summary
anatolicum_africa2080$summary
anatolicum_africa2100$summary

variegatum_africa1995$summary
variegatum_africa2040$summary
variegatum_africa2080$summary
variegatum_africa2100$summary



dromedarii_result2025$summary
dromedarii_2040$summary
dromedarii_2080$summary
dromedarii_2100$summary


anatolicum_result2025$summary
anatolicum_2040$summary
anatolicum_2080$summary
anatolicum_2100$summary

variegatum_result2025$summary
variegatum_2040$summary
variegatum_2080$summary
variegatum_2100$summary

library(dplyr)

africa_summaries <- bind_rows(
  dromedarii_africa1995$summary,
  dromedarii_africa2040$summary,
  dromedarii_africa2080$summary,
  dromedarii_africa2100$summary,
  
  anatolicum_africa1995$summary,
  anatolicum_africa2040$summary,
  anatolicum_africa2080$summary,
  anatolicum_africa2100$summary,
  
  variegatum_africa1995$summary,
  variegatum_africa2040$summary,
  variegatum_africa2080$summary,
  variegatum_africa2100$summary
) %>% mutate(Species = ifelse(Species == "Amblyomma variegatum", "A. variegatum", Species))



africa_summaries$Year <- as.numeric(as.character(africa_summaries$Year))

africa_summaries$Year <-factor(
  global_summaries$Time,
  levels = c("1995","2040","2080","2100"),
  labels = c("Historic","2040","2080","2100")
)
ggplot(africa_summaries,
       aes(x = Year,
           y = Suitable_area_km2,
           color = Species,
           group = Species)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "orange", "red")) +
  labs(
    x = "Year",
    y = "Suitable Area in Africa (km²)",
    color = "Ticks"
  ) + theme_classic()

 

global_summaries = bind_rows(
  
  dromedarii_result2025$summary %>% mutate(Time = "1995", Species = "H. dromedarii") %>%
    dplyr::select(Time,Species, Suitable_area_km2),
  dromedarii_2040$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. dromedarii"),
  dromedarii_2080$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. dromedarii"),
  dromedarii_2100$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. dromedarii"),
  
  
  anatolicum_result2025$summary %>% mutate(Time = "1995",  Species = "H. anatolicum") %>%
    dplyr::select(Time,Species, Suitable_area_km2),
  anatolicum_2040$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. anatolicum"),
  anatolicum_2080$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. anatolicum"),
  anatolicum_2100$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "H. anatolicum"),
  
  variegatum_result2025$summary %>% mutate(Time = "1995", Species = "A. variegatum") %>%  
    dplyr::select(Time, Species, Suitable_area_km2),
  variegatum_2040$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "A. variegatum"),
  variegatum_2080$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "A. variegatum"),
  variegatum_2100$summary %>%  dplyr::select(Time, Suitable_area_km2) %>% mutate(Species = "A. variegatum"))




ggplot(global_summaries,
       aes(x = Time,
           y = Suitable_area_km2,
           color = Species,
           group = Species)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "orange", "red")) +
  scale_x_discrete(
    labels = c(
      "1995" = "Historic",
      "2040" = "2040",
      "2080" = "2080",
      "2100" = "2100"
    )
  ) +
  labs(
    x = "Period",
    y = "Global suitable Area (km²)",
    color = "Species"
  ) +
  theme_classic()



