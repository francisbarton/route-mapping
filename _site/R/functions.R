check_intersect <- function(a, b, radius_a = 250, radius_b = 250) {

  tryCatch(
    {
      list(a, b) %>%
        openrouteservice::ors_directions(output = "sf", instructions = FALSE, suppress_warnings = TRUE, radiuses = c(radius_a, radius_b)) %>%
        dplyr::mutate(radius_a = radius_a) %>%
        dplyr::mutate(radius_b = radius_b) %>%
        dplyr::select(!summary) %>%
        dplyr::mutate(status = "Success") %>%
        dplyr::mutate(crosses = s2::s2_intersects(., crossing_sf)) %>%
        dplyr::relocate(geometry, .after = last_col())
    },

    error = function(cond) {
      probs <- stringr::str_extract_all(
        string = conditionMessage(cond),
        pattern = "(?<=coordinate )[:digit:]+"
      ) %>%
        unlist() %>%
        as.numeric()

      if (length(probs) == 0) {
        dplyr::tribble(
          ~radius_a, ~radius_b, ~way_points, ~status, ~crosses, ~geometry,
          NA, NA, NA, "Error", FALSE, NA
        )
        usethis::ui_warn(conditionMessage(cond))
      } else if ( radius_a < 4001 && radius_a < 4001) {
        if (0 %in% probs) {
          radius_a <- radius_a * 2
        }
        if (1 %in% probs) {
          radius_b <- radius_b * 2
        }
        check_intersect(a, b, radius_a, radius_b)
      } else {
        nf_msg <- "Routable point(s) not found"
        dplyr::tribble(
          ~radius_a, ~radius_b, ~way_points, ~status, ~crosses, ~geometry,
          radius_a, radius_b, NA, nf_msg, FALSE, NA
        )
      }
    }
  )
}

# in theory I have caught all the errors in tryCatch above, but just in case...
check_intersect_possibly <- purrr::possibly(
  check_intersect,
  otherwise = NULL,
  quiet = FALSE
)



# check best rate to use for API calls:
# my token is limited to 20,000 calls a day
# and 100 calls a minute
day_rate <- round(60*60*24/19999, 4) # can average 1 call every 4.3203s
minute_rate <- round(60/99, 4)       # can average 1 call every 0.6061s

# this should allow jobs with >19999 requests to keep running over >24h
# without exceeding daily quota
check_intersect_slowly <- purrr::slowly(
  check_intersect_possibly,
  rate = rate_delay(pause = day_rate), # allows 19999 requests in a day
  quiet = FALSE
)

check_intersect_quite_quickly <- purrr::slowly(
  check_intersect_possibly,
  rate = rate_delay(pause = minute_rate) # uses daily quota in 1 go (max 3h22)
)




work_down <- function(current_msoa, lads_coords, i, n, slowly = FALSE, keep_sf = FALSE) {

  a <- current_msoa %>%
    sf::st_coordinates() %>%
    `[`(1,)

  coords_list <- seq(nrow(lads_coords)) %>%
    purrr::map( ~ dplyr::slice(lads_coords, .)) %>%
    purrr::map(unlist)

  if (slowly) {
    routes_df <- coords_list %>%
      purrr::map_df( ~ check_intersect_slowly(a, .))
  } else {
    routes_df <- coords_list %>%
      purrr::map_df( ~ check_intersect_quite_quickly(a, .))
  }

  # kind of pointless, but anomalies would let us know if NULLs were returned
  total_routes <- routes_df %>%
    nrow()
  crossings <- routes_df %>%
    dplyr::filter(crosses) %>%
    nrow()

  routes_missing <- nrow(lads_coords) - total_routes
  if (routes_missing > 0) {
    any_missing <- paste(routes_missing, "routes missing.")
  } else any_missing <- ""

  # actual errors should have been caught, then converted into valid output,
  # but with status != "Success"
  errors <- routes_df %>%
    dplyr::filter(!status == "Success") %>%
    nrow()

  usethis::ui_info(
    stringr::str_glue(
      "{ current_msoa$msoa11cd } ({i} of {n}) done. { any_missing }Total crossings: {crossings}."
    )
  )


  if (keep_sf) {
    current_msoa <- current_msoa %>%
      sf::st_drop_geometry()
    routes_df %>%
      dplyr::bind_cols(., current_msoa) %>%
      dplyr::relocate(names(current_msoa)) %>%
      dplyr::mutate(
        total_results = n(),
        successful = length(which(status == "Success")),
        crossings = sum(crosses)
      ) %>%
      dplyr::select(!status) %>%
      dplyr::group_by(msoa11cd, msoa11nm, msoa11hclnm, crosses) %>%
      dplyr::summarise(
        # across(starts_with("msoa"), first),
        across(total_results:crossings, mean),
        .groups = "drop"
      ) %>%
      ungroup()
  } else {
    current_msoa %>%
      dplyr::mutate(total_routes = total_routes) %>%
      dplyr::mutate(of_maximum = nrow(lads_coords)) %>%
      dplyr::mutate(crossings = crossings) %>%
      dplyr::mutate(errors = errors)
  }
}




check_full_list <- function(msoas_df, lads_df, keep_sf = FALSE) {

  slowly <- FALSE
  calls <- nrow(msoas_df) * nrow(lads_df)
  if (calls > 19999) slowly <- TRUE

  coords <- lads_df %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble()

  n_msoas <- nrow(msoas_df)

  analyse_msoa <- function(df1, df2, i) {
    df1 %>%
      dplyr::slice(i) %>%
      work_down(df2, i, n_msoas, slowly, keep_sf)
  }

  seq(n_msoas) %>%
    purrr::map_df( ~ analyse_msoa(msoas_df, coords, .x))
}
