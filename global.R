#' Create the WTP question text
#'
#' Generates the WTP question based on the treatment. 
#' 
#' Alternative phrasing of the second question: 
#'   "Du er villig til å betale mindre enn..."
#'   "x % av den noreske befolkning er villig til å betale mindre enn deg."
#'
create_wtp_question <- function(wtp, distance, treatment) {
  # Set values for the distribution
  mu <- 1.96 * distance
  sig <- 1.35 * distance
  
  start <- paste0("Basert på våre beregninger er gjennomsnittlig betalingsvilje for å stoppe kongekrabbe fra å etablere seg ",
                round(distance, 0), " km fra Nordkapp ", round(mu, 0), " kroner per år. Du har sagt du er villig til å betale ", wtp, " kroner. Da er")
  
  # Generate the correct question based on the treatment
  middle <- switch(treatment,
                more = paste0(" du villig til å betale mer enn ",
                              floor(pnorm(wtp, mu, sig) * 100),
                              "% av den norske befolkningen"),
                less = paste0(ceiling((1 - pnorm(wtp, mu, sig)) * 100), 
                              "% av den norske befolkning er villig til å betale mer enn deg."))
  
  end <- "Ønsker du å endre hvor mye du er villig til å betale?"
  
  txt <- paste(start, middle, end, sep = " ")
  
  return(txt)
}

#' Create the "average" guess
#'
#' "Triangualtes" the center location among all guesses stored in the database
#' by taking the average of the longitudes and the average of the latitudes to
#' to get the "central" location.
#' 
#' Note that this approach **ignores** the curvature of the Earth. It small
#' scales this should not matter. 
#'
#' The idea is that as more guesses are added, this location should trend 
#' towards the TRUE location. I've added timestamps to the data so that we can
#' create an animation of the guessing process (in a separate app), which should
#' help visualize the 'wisdom of the crowds' in this context.
#'
#' @inheritParams save_db
avg_guess <- function(conn, table) {
  return(
    tbl(conn, table) %>%
      filter(id != "initialize") %>%
      summarize(
        avg_guess_lng = mean(lng, na.rm = TRUE),
        avg_guess_lat = mean(lat, na.rm = TRUE)
      ) %>% 
      collect
  )
}

#' Save to database
#'
#' @param conn A database connection
#' @param x A named list of data. Note that the data type must match the data
#' types defined in the database. Inputs that are strings must have the '' as
#' part of the string. Otherwise the database connection will fail.
#' @param table A string with the name of the table in the database
#' 
save_db <- function(conn, x, table) {
  # Interpolate the elements of x
  x <- do.call(c, lapply(x, function(y) {
    sql <- "?value"
    sqlInterpolate(conn, sql, value = y)
  }))
  
  # Construct the query for sending data
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s)",
    table,
    paste(names(x), collapse = ", "),
    paste(x, collapse = ", ")
  )
  
  # Submit the query to the database
  RMariaDB::dbExecute(conn, query)
}


# # Initialize the guesses table in the database
# dbWriteTable(
#   conn = pool,
#   name = "location_guesses",
#   value = tibble(
#     id = paste0(sample(c(letters, LETTERS, 0:9), 10), collapse = ""),
#     timestamp_start = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#     timestamp_end = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#     treatment = "TREA",
#     lng = 71.00000000,
#     lat = 71.00000000,
#     wtp_original = 99999,
#     wtp_revised = 99999,
#     age = 99,
#     gender = "Foretrekker ikke å si"
#   ),
#   overwrite = TRUE
# )
