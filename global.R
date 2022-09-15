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
