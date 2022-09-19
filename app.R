#' PICO: Outreach
#'
#' This is a Shiny application written for the PICO project outreach workshop
#' in Tromsø.
#'
#' Author: Erlend Dancke Sandorf
#'
#'

# Load packages ----
library(shiny)
library(shinyjs)
library(leaflet)
library(htmltools)
library(geosphere)
library(pool)
library(DBI)
library(RMariaDB)
library(config)
library(tibble)

# Source the global file
source("global.R")

# Get the connection details
db_config <- config::get("dataconnection")

# Set up the pool for effective handling of multiple connections
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = db_config$dbname,
  host = db_config$host,
  username = db_config$username,
  password = db_config$password
  # ssl.key = db_config$ssl.key,
  # ssl.cert = db_config$ssl.cert,
  # ssl.ca = db_config$ssl.ca
)


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
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    clearControls() %>% 
    clearMarkers() %>% 
    clearPopups() %>% 
    # setView(lng = 18.949252, lat = 69.673561, zoom = 5) %>%
    setView(lng = 15.949252, lat = 69.673561, zoom = 5) %>%
    fitBounds(lng1 = 15, lat1 = 72, lng2 = 20, lat2 = 58) %>%
    addMarkers(
      lng = 25.783742,
      lat = 71.171894,
      # label = HTML("<b>Nordkapp:</b> Norske myndigheter <br/> ønsker å stoppe krabben fra å <br/>spre seg lenger vest og sør (til venstre og nedover)"),
      label = HTML("<b>Nordkapp</b> "),
      labelOptions = labelOptions(noHide = TRUE,
                                  direction = "right",
                                  style = list(
                                    "color" = "black",
                                    "font-family" = "serif",
                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                    "font-size" = "16px",
                                    "border-color" = "rgba(0,0,0,0.5)",
                                    "margin-left" = "10px",
                                    "padding-left" = "10px"
                                  )
      )
    ) #%>% 
    # addMarkers(
    #   lng = true_crab_location[[1]],
    #   lat = true_crab_location[[2]],
    #   label = HTML("True crab location")
    # )
}

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
  class = "page",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shinyjs::useShinyjs(),
  # The top bar
  # fluidRow(
  #   class = "top-row",
  #   h1("PICO deltar på forskningsdagene!")
  # ),
  # The main window
  fluidRow(
    class = "ui-row",
    uiOutput("user_interface")
  )#,
  # Footer
  # fluidRow(
  #   class = "bottom-row",
  #   div(
  #     p(id = "creator", "Created by: Erlend Dancke Sandorf"),
  #     actionButton("reset", "Start på nytt")
  #   )
  # )
  # fluidRow(
  #   h2("Testing output only!"),
  #   verbatimTextOutput("current_page"),
  #   verbatimTextOutput("out"),
  #   verbatimTextOutput("results"),
  #   h2("distance in km 'as the crow flies' from the north cape"),
  #   verbatimTextOutput("distance_km"),
  #   h2("distance in km 'as the crow flies' from true location"),
  #   verbatimTextOutput("dist_true_location")
  # )
  
)

# Server side ----
server <- function(input, output, session) {
  # Randomly allocate people to the more than or less than treatment
  id <- paste0(sample(c(letters, LETTERS, 0:9), 10), collapse = "") 
  treatment <- sample(c("more", "less"), 1)
  timestamp_start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Create a reactive value for the page that can update when the submit button
  # is clicked. 
  page <- reactiveVal("page_map")
  distance_km_north_cape <- reactiveVal(0)
  distance_km_true_location <- reactiveVal(0)
  wtp_question <- reactiveVal("")
  
  # Create an empty tibble and assign values to it.
  results <- tibble(
    id = id,
    timestamp_start = timestamp_start,
    timestamp_end = NA, 
    treatment = treatment,
    lng = NA,
    lat = NA,
    wtp_original = NA,
    wtp_revised = NA,
    age = NA,
    gender = NA
  )
  
  # Reset the application ----
  observeEvent(input$reset, {
    shinyjs::refresh() # Does this trigger onSessionEnded()?
  })
  
  # When the session ends ----
  session$onSessionEnded(
    function() {
      # Timestamp 
      results$timestamp_end <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Save the guess to the database. NB! Each click of the button creates a new
      # entry in the database
      
      dbWriteTable(
        conn = pool,
        name = "location_guesses",
        value = results,
        append = TRUE
      )
      
    }
  )
  
  # User interface ----
  ## UI: Map ----
  reactive_map <- reactiveVal(base_map())
  
  output$map <- renderLeaflet({
    reactive_map()
  })
  
  # By default the next button is disabled until a choice is made
  output$map_ui <- renderUI({
    tagList(
      h2("Norske myndigheter ønsker og stoppe kongekrabbe fra å spre seg sør-vest fra Nordkapp (til venstre og nedover langs kysten av Norge). Hvor tror du kongekrabbe har blitt sett og fanget?"),
      div(class = "map-output", leafletOutput("map")),
      shinyjs::disabled(
        actionButton("submit_guess", "Send inn ditt svar")
      ),
      actionButton("reset", "Start på nytt")
    )
  })
  
  # Add an observer for the event that the map is clicked. When clicked, get 
  # the coordinates for the click and label it their guess. When they have
  # made their final guess, they can click "submit guess"
  observeEvent(input$map_click, {
    click <- input$map_click
    text <- "Du har gjettet at <br/> kongekrabbe har blitt <br/> observert her"
    
    # Add guess to results 
    results$lng <<- click$lng
    results$lat <<- click$lat
    
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
    
    # Calculate the distance in km from Nordkapp (updating a reactive value)
    distance_km_north_cape(
      geosphere::distHaversine(
        unlist(north_cape),
        c(click$lng, click$lat),
        r = 6378.137
      )
    )
    
    # Calcualte the distance in km from the true observed location
    distance_km_true_location(
      geosphere::distHaversine(
        unlist(true_crab_location),
        c(click$lng, click$lat),
        r = 6378.137
      )
    )
    
    # Enable the submit button when a guess is made
    shinyjs::enable("submit_guess")

  })
  
  # Submit guess
  observeEvent(input$submit_guess, {

    # Update page
    page("page_wtp_original")
    
  })
  
  ## UI: Original willingness-to-pay ----
  output$wtp_original <- renderUI({
    tagList(
      h2(paste0("Du gjettet at kongekrabbe har blitt observert ",
                round(distance_km_north_cape(), 0),
                "km fra Nordkapp")),
      shiny::numericInput("wtp_original",
                       # label = "Hvor mye er du villig til å betale per år for at den ikke skal etablere seg der, men at den blir stoppet ved Nordkapp?",
                       label = paste0("Hvor mye er du villig til å betale per år for at den skal bli stoppet fra å etablere seg så langt sør-vest som ",
                                      round(distance_km_north_cape(), 0),
                                      "km fra Nordkapp?"),
                       value= "",
                       width = "100%"),
      actionButton("submit_wtp_original", "Send inn ditt svar"),
      actionButton("reset", "Start på nytt")
    )
  })
  
  # Observe changes to WTP question to capture WTP without clicking submit
  observe({
    input$wtp_original
    results$wtp_original <<- input$wtp_original
  })
  
  observeEvent(input$submit_wtp_original, {
    # Make WTP calculations and store the data in reactive values
    txt <- create_wtp_question(input$wtp_original,
                               distance_km_north_cape(),
                               treatment)
    
    wtp_question(txt)
    
    # Update current page
    page("page_wtp_information")
  })
  
  ## UI: Willingness-to-pay information ----
  output$wtp_information <- renderUI({
    tagList(
      h2(
        wtp_question()
      ),
      div(actionButton("submit_yes", "Ja"),
          actionButton("submit_no", "Nei")),
      actionButton("reset", "Start på nytt")
    )
  })
  
  # If yes, send to page where they can revise their WTP
  observeEvent(input$submit_yes, {
    page("page_wtp_revised")
  })
  
  # If no, send to page with socio-demographics and set revise WTP to NA
  observeEvent(input$submit_no, {
    page("page_socio_dem")
  })
  
  ## UI: Revise willingness-to-pay ----
  output$wtp_revised <- renderUI({
    tagList(
      shiny::numericInput("wtp_revised",
                          label = paste0(
                            "Du sa du ønsket å endre hvor mye du vil betale. ",
                            "Hvor mye er du villig til å betale per år for at den skal bli stoppet fra å etablere seg så langt sør-vest som ",
                            round(distance_km_north_cape(), 0),
                            "km fra Nordkapp?"),
                          value= "",
                          width = "100%"),
      actionButton("submit_wtp_revised", "Send inn ditt svar"),
      actionButton("reset", "Start på nytt")
    )
  })
  
  # Observe changes to WTP question to capture WTP without clicking submit
  observe({
    input$wtp_revised
    results$wtp_revised <<- input$wtp_revised
  })
  
  observeEvent(input$submit_wtp_revised, {
    page("page_socio_dem")
  })
  
  ## UI: Socio-demographics ----
  output$socio_dem <- renderUI({
    tagList(
      h2("Helt til slutt:"),
      shiny::numericInput(inputId = "age",
                       label = "Hvor gammel er du?",
                       value= "",
                       width = "100%"),
      selectInput(inputId = "gender",
                  label = "Jeg er: ",
                  choices = c("", "Kvinne", "Mann", "Annet", "Foretrekker ikke å si"),
                  selected = character(0),
                  width = "100%"),
      actionButton("submit_socio", "Send inn ditt svar"),
      actionButton("reset", "Start på nytt")
        
    )
  })
  
  # Observe changes to the sociodemographic questions
  observe({
    input$age
    input$gender
    results$age <<- input$age
    results$gender <<- input$gender
  })
  
  observeEvent(input$submit_socio, {
    page("page_final")
  })
  
  ## UI: Final page ----
  output$page_final <- renderUI({
    tagList(
      h2("Tusen takk for at du deltok!"),
      h3("Husk å skrive deg på liste på standen om du ønsker å være med i trekningen av premie"),
      actionButton("reset", "Start på nytt")
    )
    
  })
  
  ## UI: Reactive UI ----
  user_interface <- reactive({
    
    current_page <- page()
    interface <- switch(current_page,
                        page_map = uiOutput("map_ui"),
                        page_wtp_original = uiOutput("wtp_original"),
                        page_wtp_information = uiOutput("wtp_information"),
                        page_wtp_revised = uiOutput("wtp_revised"),
                        page_socio_dem = uiOutput("socio_dem"),
                        page_final = uiOutput("page_final")
    )
    
    # Return the interface
    return(
      interface
    )
  })
  
  # Render the user interface
  output[["user_interface"]] <- renderUI({
    user_interface()
  })
  
  
  
  # DEBUGGING ----
  output$current_page <- renderText({
    page()
  })
  
  output$out <- renderPrint({
    input$map_click
  })
  
  output$results <- renderPrint({
    results
  })
  
  output$distance_km <- renderText({
    distance_km_north_cape()
  })
  
  output$dist_true_location <- renderText({
    distance_km_true_location()
  })
  
}

# Combine the app
shinyApp(ui, server)