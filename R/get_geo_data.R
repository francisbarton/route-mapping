library(purrr)
library(jogger)



# get MSOA centroids and bind together ------------------------------------



south_names <- c(
  "Bexley",
  "Greenwich",
  "Bromley",
  "Croydon",
  "Southwark",
  "Lambeth",
  "Lewisham",
  "Merton",
  "Sutton",
  "Kingston upon Thames",
  "Richmond upon Thames", # straddles river
  "Wandsworth",
  # ok Hounslow is north of the river but adding to sth to ~equalise MSOA nos.!
  "Hounslow",   # 13
  "Kent",
  "Medway",
  "Surrey",
  "East Sussex",
  "West Sussex",
  "Brighton and Hove" # 19
)


north_names <- c(
  "Havering",
  "Barking and Dagenham",
  "Redbridge",
  "Waltham Forest",
  "Hackney",
  "Haringey",
  "Tower Hamlets",
  "City of London",
  "Newham",
  "Enfield",
  "Islington",
  "Camden",
  "Ealing",
  "Brent",
  "Barnet",
  "Westminster",
  "Kensington and Chelsea",
  "Hammersmith and Fulham",
  "Harrow",
  "Hillingdon", # 20
  "Hertfordshire",
  "Essex",
  "Thurrock",
  "Southend-on-Sea" # 24
)


south_msoas <- south_names %>%
  map_df( ~ geo_get(
    "msoa",
    .,
    "utla",
    return_centroids = TRUE
  ))


north_msoas <- north_names %>%
  map_df( ~ geo_get(
    "msoa",
    .,
    "utla",
    return_centroids = TRUE
  ))


saveRDS(south_msoas, here::here("rds_data", "south_msoas.Rds"))
saveRDS(north_msoas, here::here("rds_data", "north_msoas.Rds"))




# get LAD centroids and bind together -------------------------------------




south_lads <- south_names %>%
  map_df( ~ geo_get(
    "lad",
    .,
    "utla",
    centroid_fields = TRUE
  )) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-c(bng_e, bng_n)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)


north_lads <- north_names %>%
  map_df( ~ geo_get(
    "lad",
    .,
    "utla",
    centroid_fields = TRUE
  )) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-c(bng_e, bng_n)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)


saveRDS(south_lads, here::here("rds_data", "south_lads.Rds"))
saveRDS(north_lads, here::here("rds_data", "north_lads.Rds"))
