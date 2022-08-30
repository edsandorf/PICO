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
  mu <- 1.8 * distance
  sig <- 1.2 * distance
  
  # Generate the correct question based on the treatment
  txt <- switch(treatment,
                more = paste0(" er du villig til å betale mer enn ",
                              floor(pnorm(wtp, mu, sig)),
                              "% av den norske befolkning.\n\n Ønsker du å endre hvor mye du er villig til å betale?"),
                less = paste0(" er ", ceiling(pnorm(wtp, mu, sig)), 
                              "% av den norske befolkning er villig til å betale mer enn deg.\n\n Ønsker du å endre hvor mye du er villig til å betale?"))
  
  return(txt)
}