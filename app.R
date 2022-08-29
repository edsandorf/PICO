#' PICO: Outreach
#'
#' This is a Shiny application written for the PICO project outreach workshop
#' in Tromsø.
#'
#' Author: Erlend Dancke Sandorf
#'
#' Still to do:
#'   - Calculate the distance between the selected point and the true crab loc
#'   - Calculate the running average of the distances
#'   - Calculate the error of the running average
#'   - Calculate the running average of the individual errors
#'   - Extend to WTP:
#'     * How much would you be willing to pay to avoid it spreading and
#'       establishing as far as you have indicated? (record response)
#'     * Using the km spread as the scenario baseline, establish where they are
#'       in the WTP distribution and report to them. Ask if they want to revise
#'       their WTP, if yes, state the new one. 
#'   - We should get age and gender.
#'   - People who want to join the prize draw should leave their e-mail at the
#'     stand. We want to keep it completely separate from the data.
#'
#'   - Treatment: Random assignment into one or two groups (framing):
#'     * Du er villig til å betale mer enn X % av befolkningen, vil du endre din
#'       betalingsvilje?
#'     * X % av befolkningen er villig til å betale mer enn deg, vil du endre 
#'       din betalingsvilje?
#'
# Load packages ----
library(shiny)
library(shinyjs)
library(leaflet)
library(htmltools)
library(geosphere)
library(pool)
library(RMariaDB)
library(config)

# Define functions ----
#' Define the base map function
#' 
#' We define the base map as a function that can be called inside a reactive
#' value. This enables us to quickly and easily reset the map to its original
#' version using a button connected to an observer.
#' 
#' The function does not take any arguments
base_map <- function() {
  leaflet() %>% 
    clearControls() %>% 
    clearMarkers() %>% 
    clearPopups() %>% 
    setView(lng = 18.949252, lat = 69.673561, zoom = 5) %>% 
    # addTiles() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addLabelOnlyMarkers(
      lng = 25.783742,
      lat = 71.171894,
      label = HTML("<b>Nordkapp:</b> Norske myndigheter <br/> ønsker å stoppe krabben fra å <br/>spre seg lenger vest og sør"),
      labelOptions = labelOptions(noHide = TRUE,
                                  direction = "right",
                                  style = list(
                                    "color" = "black",
                                    "font-family" = "serif",
                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                    "font-size" = "12px",
                                    "border-color" = "rgba(0,0,0,0.5)"
                                  )
      )
    ) %>% 
    addMarkers(
      lng = true_crab_location[[1]],
      lat = true_crab_location[[2]],
      label = HTML("True crab location")
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

#' Define end of session 
#' 
#' Define a function for what happens when the session ends
on_session_ended <- function() {
  
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

#' Create the WTP question text
#'
#' Generates the WTP question based on the treatment. 
#' 
#' Alternative phrasing of the second question: 
#'   "Du er villig til å betale mindre enn..."
#'   "x % av den noreske befolkning er villig til å betale mindre enn deg."
#'
create_wtp_question <- function(wtp, treatment) {
  # Set values for the distribution
  mu <- 250
  sig <- 100
  
  # Generate the correct question based on the treatment
  txt <- switch(treatment,
                more = paste0("Du er villig til å betale mer enn ",
                              floor(pnorm(wtp, mu, sig)),
                              "% av den norske befolkning. Ønsker du å endre hvor mye du er villig til å betale?"),
                less = paste0(ceiling(pnorm(wtp, mu, sig)), 
                              "% av den norske befolkning er villig til å betale mer enn deg. Ønsker du å endre hvor mye du er villig til å betale?"))

  return(txt)
}

# Set up a DB connection ----
# If not on shinyapps.io, set to local to handle connections for testing
# if () {
#   
# }
Sys.setenv(R_CONFIG_ACTIVE = "local")

# Get the connection details
db_config <- config::get("dataconnection")

# Set up the pool for effective handling of multiple connections
# pool <- pool::dbPool(
#   drv = RMariaDB::MariaDB(),
#   dbname = db_config$dbname,
#   host = db_config$host,
#   username = db_config$username,
#   password = db_config$password,
#   ssl.key = db_config$ssl.key,
#   ssl.cert = db_config$ssl.cert,
#   ssl.ca = db_config$ssl.ca
# )


# Define global variables ----
true_crab_location <- list(
  lng = 17.31445,
  lat = 69.79034
)

north_cape <- list(
  lng = 25.783742,
  lat = 71.171894
)

# User interface ----
ui <- fluidPage(
  theme = "styles.css",
  shinyjs::useShinyjs(),
  # The top bar
  fluidRow(
    class = "top-row",
    h1("Hvor langt vest og sør tror du kongekrabbe har blitt observert?")
  ),
  # The main window
  fluidRow(
    class = "map-row",
    leafletOutput("map")
  ),
  # Button row
  fluidRow(
    class = "button-row",
    actionButton("submit", "Send inn ditt svar"),
    actionButton("reset", "Start på nytt")
  ),
  # Footer
  fluidRow(
    class = "bottom-row",
    p("Created by: Erlend Dancke Sandorf")
  ),
  fluidRow(
    h2("Testing output only!"),
    verbatimTextOutput("out"),
    h2("distance in km 'as the crow flies' from the north cape"),
    verbatimTextOutput("dist_north_cape"),
    h2("distance in km 'as the crow flies' from true location"),
    verbatimTextOutput("dist_true_location")
  )
  
)

# Server side ----
server <- function(input, output, session) {
  # Randomly allocate people to the more than or less than treatment
  treatment <- sample(c("more", "less"), 1)
  
  # Map ----
  reactive_map <- reactiveVal(base_map())
  
  output$map <- renderLeaflet({
    reactive_map()
  })
  
  # Add an observer for the event that the map is clicked. When clicked, get 
  # the coordinates for the click and label it their guess. When they have
  # made their final guess, they can click "submit guess"
  observeEvent(input$map_click, {
    click <- input$map_click
    text <- "Du har gjettet at <br/> kongekrabbe har blitt <br/> observert her"
    
    # Define a proxy to avoid redrawing the map every time a new click is made
    proxy <- leafletProxy("map")
    
    proxy %>% 
      clearPopups() %>%
      addPopups(
        click$lng, 
        click$lat, 
        text,
        options = popupOptions(
          closeButton = TRUE
        )
      )
  })
  
  ## Reset the map ----
  observeEvent(input$reset, {
    shinyjs::refresh()
  })
  
  ## Submit the guess ----
  observeEvent(input$submit, {
    # Save the guess to the database. NB! Each click of the button creates a new
    # entry in the database
    guesses <- list(
      "id" = paste0(sample(c(letters, LETTERS, 0:9), 10), collapse = ""),
      "timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "lng" = input$map_click$lng,
      "lat" = input$map_click$lat
    )
    
    # save_db(pool, guesses, "location_guess")
    
    # Move to the next part of the game?
  })
  
  # When the session ends ----
  # session$onSessionEnded(
  #   on_session_ended()
  # )
  
  
  
  ## This is included for debugging ----
  output$out <- renderPrint({
    input$map_click
  })
  
  output$dist_true_location <- renderPrint({
    geosphere::distHaversine(
      unlist(true_crab_location),
      c(input$map_click$lng, input$map_click$lat),
      r = 6378.137
    )
  })
  
  output$dist_north_cape <- renderPrint({
    geosphere::distHaversine(
      unlist(north_cape),
      c(input$map_click$lng, input$map_click$lat),
      r = 6378.137
    )
  })
}

shinyApp(ui, server)