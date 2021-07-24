crossing_sf <- sf::st_linestring(
  matrix(
    c(
      0.2519821062, 51.4662165200,  # northwest
      0.2635477915, 51.4662165200,  # northeast
      0.2635477915, 51.4632889186,  # southeast
      0.2519821062, 51.4632889186,  # southwest
      0.2519821062, 51.4662165200), # northwest
    ncol = 2, byrow = TRUE),
) %>%
  sf::st_polygonize() %>%
  sf::st_sfc(crs = 4326)

# I learned a bit about s2 and wondered about doing this, but it didn't work
# crossing_s2 <- s2::s2_make_polygon(
#   longitude = c(0.2519821062, 0.2635477915),
#   latitude = c(51.4632889186, 51.4662165200)
# )
