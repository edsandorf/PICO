# # Initialize the guesses table in the database
# dbWriteTable(
#   pool,
#   "location_guess",
#   tibble::tibble(
#     id = "initialize",
#     timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#     lng = 0.00000,
#     lat = 0.00000
#   ),
#   overwrite = TRUE
# )
