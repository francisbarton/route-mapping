# Initially ran with radiuses of 250m (lower than ors default of 350m) but
# I suspect this was optimistic and caused more queries than really necessary
# I think 500m is a reasonable starting point
check_intersect <- function(a, b, radius_a = 500, radius_b = 500) {

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

        # max search radius set to 4km - hopefully never required!
        # Hope this is reasonable
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







work_down <- function(current_msoa, lads_coords, i = 1, n = 1, keep_sf = FALSE) {

  slowly <- FALSE
  calls <- n * length(lads_coords)
  if (calls > 19999) slowly <- TRUE


  # check best rate to use for API calls:
  # my token is limited to 20,000 calls a day
  # and 100 calls a minute

  # this should allow jobs with >19999 requests to keep running over >24h
  # without exceeding daily quota - just keep on running??
  day_rate <- round(60*60*24/19999, 4) # can average 1 call every 4.3203s

  # uses daily quota aqap (~max calls/min) (~ 3h22 runtime)
  minute_rate <- round(60/99, 4)       # can average 1 call every 0.6061s

  rate <- minute_rate
  if (slowly) rate <- day_rate

  check_intersect_slowly <- purrr::slowly(
    check_intersect,
    # rate = purrr::rate_backoff(pause_base = rate, jitter = FALSE)
    rate = purrr::rate_delay(pause = rate)
  )


  a <- current_msoa %>%
    sf::st_coordinates() %>%
    as.vector()

  routes_df <- lads_coords %>%
      purrr::map_df( ~ check_intersect_slowly(a, .))



  # kinda pointless, but inequalities would let us know if NULLs were returned
  total_routes <- routes_df %>%
    nrow()
  crossings <- routes_df %>%
    dplyr::filter(crosses) %>%
    nrow()


  routes_missing <- length(lads_coords) - total_routes
  if (routes_missing > 0) {
    any_missing <- paste(routes_missing, "routes missing.")
  } else {
    any_missing <- ""
  }


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
      )
  } else {
    current_msoa %>%
      dplyr::mutate(total_routes = total_routes) %>%
      dplyr::mutate(of_maximum = nrow(lads_coords)) %>%
      dplyr::mutate(crossings = crossings) %>%
      dplyr::mutate(errors = errors) %>%
      dplyr::relocate(geometry, .after = dplyr::last_col())
  }
}


work_down_safely <- purrr::safely(
  work_down,
  otherwise = NULL,
  quiet = FALSE
)


analyse_msoas <- function(msoas_df, coords_list, i = 1, previous_results = NULL, ...) {

  nm <- nrow(msoas_df)

  current_msoa <- msoas_df %>%
    dplyr::slice(i)

  safe_results <- work_down_safely(current_msoa, coords_list, i, n = nm, ...)

  success <- safe_results %>%
    purrr::map_df("result")

  errors <- safe_results %>%
    purrr::map("error")

  if (length(errors) > 0) {
    list(previous_results, success, errors)
  } else {
    new_results <- previous_results %>%
      dplyr::bind_rows(success)
  }


  if (i < nm) {
    analyse_msoas(msoas_df, coords_list, i = i + 1, new_results, ...)
    } else {
      new_results
  }
}



check_routes <- function(msoas_df, lads_df, keep_sf = FALSE) {

  coords_list <- lads_df %>%
    sf::st_geometry() %>%
    purrr::map(sf::st_coordinates) %>%
    purrr::map(as.vector)

  msoas_df %>%
    analyse_msoas(coords_list, keep_sf = keep_sf)
}
