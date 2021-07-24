library(here)
library(tidyverse)
library(openrouteservice)
library(sf)


source(here("R/functions.R"))
source(here("R/crossing_sf.R"))

north_lads <- readRDS(here::here("rds_data", "north_lads.Rds"))
south_lads <- readRDS(here::here("rds_data", "south_lads.Rds"))
north_msoas <- readRDS(here::here("rds_data", "north_msoas.Rds"))
south_msoas <- readRDS(here::here("rds_data", "south_msoas.Rds"))


surrey_vs_north_lads <- south_msoas %>%
  dplyr::filter(utla21nm == "Surrey") %>%
  check_full_list(., north_lads)

saveRDS(surrey_vs_north_lads, here::here("rds_data", "surrey_vs_north_lads.Rds"))

thurrock_vs_south_lads <- north_msoas %>%
  dplyr::filter(utla21nm == "Thurrock") %>%
  check_full_list(., south_lads)

saveRDS(thurrock_vs_south_lads, here::here("rds_data", "thurrock_vs_south_lads.Rds"))
