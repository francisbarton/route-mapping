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
  check_routes(., north_lads)

