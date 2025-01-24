prc_gbif <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/gbif-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/analyzed/gbif-1.0.0/0001929-250121130708018.zip"
  # )
  input_files <- unlist(input_files)

  # Unzip data
  tmp <- file.path(output_path, "tmp/")
  archive::archive_extract(input_files[grepl(".zip", input_files)], tmp)

  # Data
  dat <- file.path(tmp, "occurrence.txt") |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(species, year, month, day, eventDate, decimalLatitude, decimalLongitude, lifeStage) |>
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

  # Export
  sf::st_write(dat, file.path(output_path, "species_occurrences_gbif.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
